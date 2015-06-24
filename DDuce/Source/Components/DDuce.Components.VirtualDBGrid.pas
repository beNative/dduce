{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

{
  The Original Code is "VirtualDBGrid.pas", Version 1.03 beta'.
  The Initial Developer of the Original Code is Peter Sulek
  (virtualdbgrid@virtualdbgrid.wz.cz). Portions created by the Initial Developer
  are Copyright (C) 2003 All Rights Reserved. You may obtain a copy of the
  original code at http://sourceforge.net/projects/virtualdbgrid/

  Changes by Tim Sinaeve:
    - Support for VTV 5.0 and later added
    - Made indicator column fixed.
}

unit DDuce.Components.VirtualDBGrid;

{$I ..\DDuce.inc}

interface

uses
  System.UITypes, System.SysUtils, System.Classes, System.Generics.Collections,
  System.Variants, System.Types,
  WinApi.Windows, WinApi.Messages,
  Vcl.Controls, Vcl.Dialogs, Vcl.ImgList, Vcl.Forms, Vcl.Graphics,
  Data.DB,

  VirtualTrees;

const
  clWhiteSmoke : TColor  = $00F5F5F5;
  clLightYellow : TColor = $00E0FFFF;

  DefaultIndicatorColor = clBtnFace;

  // FieldFlag constants
  ffUndeclared = High(Byte);
  ffDBField    = 0;
  ffCalculated = 1;
  ffIndicator  = 2;

type
  TImageIndex = System.UITypes.TImageIndex;

type
  TVTDBAdvOption = (
    aoEditable, // allows edit nodes and update changes to database
                            // If aoFullRowSelect is True then this flag(aoEditable)
                            // is ignored (none of changes where updated to database)
    aoStrippedRows,         // grid lines will be stripped
    aoShowHorzLines,        // show horizontal grid lines
    aoShowVertLines,        // show vertical grid lines
    aoCenterScrollIntoView, // enables toCenterScrollIntoView
    aoAutoInsertIndicator,  // If AddDefaultsFieldsToColumns is called and aoAutoInsertIndicator
                            // is set then will be insert indicator column automatically
    aoAllowSorting,         // if is set then click on header will begin sorting procedure
                            // depending of SortType in DBOptions
    aoHighlightSortColumn,  // highlight sort column with custom color
    aoHourGlassCursor,      // show hourglass cursor on sort action
                            // (usefull if there is too much records to sort)
    aoSortDBFieldColumns,   // sort columns with column type = ctDBField
                            // {only if aoAllowSorting is set}
    aoEditDBFieldColumns,
    // if set, then editing colum with type ctDBField is allowed
    aoSortCalculatedColumns, // sort columns with column type = ctCalculated
                            // {only if aoAllowSorting is set}
    aoEditCalculatedColumns, // if set, then editing colum with type ctCalculated is allowed
    aoFullRowSelect, // enable full row select, see aoEditable for details
    aoMultiSelect,   // enable multi select
    aoAutoToggleBoolean, // toggle Boolean fields when the cell is double clicked
    aoEditOnClick,    // Editing mode can be entered with a single click
    aoEditOnDblClick, // Editing mode can be entered with a double click
    aoAddDefaultColumns
    // Add default columns even if one or more column(s) are added at design time
  );

  TVTDBAdvOptions = set of TVTDBAdvOption;

  { --- Types --- }
  TColumnType           = (ctDBField, ctCalculated, ctIndicator);
  TIndicatorAlign       = (aiLeft, aiCenter, aiRight);
  TIndicatorVAlign      = (aiTop, aiMiddle, aiBottom);
  TNavigateFromPosition = (nfpBegin, nfpCurrent, nfpEnd);
  // type of sorting
  //  :stNone     - Dont start sorting on header click, but you can start sorting manually
  //                by calling SetSortColumn procedure
  //  :stBuildIn  - buildin sorting feature(slower on big database)
  //  :stCustom   - when there was a click on header then OnCustomSort event
  //                will be triggered to allow user sort database by their way
  //                (in some cases, this is much faster)
  TSortingType = (stNone, stBuildIn, stCustom);

  // type of getting count of records in database
  //  :rcFromDataset - use Dataset.RecordCount, non-functional for most SQL dataset
  //  :rcCustom  - trigger event OnGetRecordCount for getting record count by user
  //               something like SELECT COUNT(*) FROM TABLEXXX  a pass a return value
  //               to RecordCount in OnGetRecordCount event
  TRecordCountType = (
    rcFromDataset,
    rcCustom
  );

const
  DefaultAdvOptions = [
    aoEditable,
    aoStrippedRows,
    aoShowHorzLines,
    aoShowVertLines,
    aoAllowSorting,
    aoHighlightSortColumn,
    aoAutoInsertIndicator,
    aoHourGlassCursor,
    aoSortDBFieldColumns,
    aoEditDBFieldColumns,
    aoSortCalculatedColumns
  ];

type
  TRecordData          = class;
  TCustomVirtualDBGrid = class;
  { --- Events --- }
  { TOnGetRecordCountEvent - Triggered when we need to know how much records is in the database.            }
  {                          If isn't assigned this event, than standard 'dataset.recordcount' will be used }
  TOnGetRecordCountEvent = procedure(
    Sender: TCustomVirtualDBGrid;
    var RecordCount: Integer
  ) of object;

  { TOnCalculateValueEvent - Triggered when at least one column has column type = ctCalculated and }
  {                          we want to fillup value for this calculated column                    }
  TOnCalculateValueEvent = procedure(
    Sender: TCustomVirtualDBGrid;
    const IDText: string;
    Column: TColumnIndex; RecordData: TRecordData;
    RowIndex: Cardinal; var CalculatedValue: string;
    var CalculatedValueType: TFieldType
  ) of object;

  { TOnFormatFieldValueEvent - Triggered when loading data from dataset}
  TOnFormatFieldValueEvent = procedure(
    Sender: TCustomVirtualDBGrid;
    Column: TColumnIndex;
    RecordData: TRecordData;
    RowIndex: Cardinal;
    Field: TField;
    var FieldValue: Variant
  ) of object;

  { TOnLoadRecordEvent - Triggered when record from database was loaded into VirtualDBGrid }
  {                      Assigning this event can reduce speed of VirtualDBGrid            }
  TOnLoadRecordEvent = procedure(
    Sender     : TCustomVirtualDBGrid;
    RecordData : TRecordData;
    RowIndex   : Cardinal
  ) of object;

  { TOnCustomSortEvent - Triggered when SortType in DBOptions is stCustom to sort database }
  {                      by user                                                           }
  {          :Column - Column index by which will be sorted                                }
  {      :ColumnType - type of column (ctDBField, ctCalculated)                            }
  {          :SortBy - If column is ctCalculated, then SortBy = Colum title(Header caption)}
  {                    of column. If column is ctDBField, then SortBy = FieldName property }
  {                    of column.                                                          }
  {   :SortDirection - Sorting direction, can be sdAscending or sdDescending               }
  {     :RefreshGrid - If True then after this event the grid will be refreshed            }
  {                    Default is True.                                                    }
  TOnCustomSortEvent = procedure(
    Sender: TCustomVirtualDBGrid;
    Column: TColumnIndex;
    ColumnType: TColumnType;
    const SortBy: string;
    SortDirection: TSortDirection;
    var RefreshGrid: Boolean
  ) of object;

  { TOnPostChanges - Triggered when grid is at the end of editing cell, you can or not post }
  {                  changes to the grid/database                                           }
  { :FieldNameOrIDText - if ColumnType = ctDBField then value of FieldNameOrIDText contains }
  {                      FieldName property of column                                       }
  {                      if ColumnType = ctCalculated then value of FieldNameOrIDText contains }
  {                      Text property of column                                            }
  {            :Column - Column index on which will be posted changes                       }
  {        :ColumnType - type of column (ctDBField, ctCalculated)                           }
  {        :RecordData - data of current record (TRecordData object)                        }
  {          :NewValue - new posted value                                                   }
  {       :PostChanges - set to True if you want to post changed, or False to not post      }
  TOnPostChanges = procedure(
    Sender: TCustomVirtualDBGrid;
    const FieldNameOrIDText: string;
    Column: TColumnIndex;
    ColumnType: TColumnType;
    RecordData: TRecordData;
    RowIndex: Cardinal;
    var NewValue: string;
    var PostChanges: Boolean
  ) of object;

  { TOnChangeSort  - Triggered when sorting in the grid was changed                          }
  {    :SortColumn - column index of sorted column                                           }
  { :SortDirection - sort direction                                                          }
  TOnChangeSort = procedure(
    Sender: TCustomVirtualDBGrid;
    SortColumn: TColumnIndex;
    SortDirection: TSortDirection
  ) of object;

  TOnCompareRecord = procedure(
    Sender: TCustomVirtualDBGrid;
    Record1, Record2: TRecordData;
    Column: TColumnIndex;
    var Result: Integer
  ) of object;

  TOnRecordDblClick = procedure(
    Sender: TCustomVirtualDBGrid;
    Column: TColumnIndex;
    RecordData: TRecordData
  ) of object;

  PDBFieldValueRec = ^TDBFieldValueRec;

  TDBFieldValueRec = record
    FieldName: string;
    FieldValue: Variant;
    FieldType: TFieldType;
    FieldFlag: Byte;
  end;

  TRecordData = class
  private
    FList : TList;
    FRecNo: Integer;

    function GetCalculatedByIdx(Index: Integer): Boolean;
    function GetCalculated(const IDText: string): Boolean;
    function GetCalculatedValueByIdx(Index: Integer): Variant;
    procedure SetCalculatedValueByIdx(Index: Integer; Value: Variant);
    function GetCalculatedValue(const IDText: string): Variant;
    procedure SetCalculatedValue(const IDText: string; Value: Variant);

    function GetField(Index: Integer) : TDBFieldValueRec;
    procedure SetField(Index: Integer; Value: TDBFieldValueRec);
    function GetFieldName(Index: Integer) : string;
    procedure SetFieldName(Index: Integer; Value: string);
    function GetFieldValueByIdx(Index: Integer) : Variant;
    function GetFieldValue(const FieldName: string) : Variant;
    procedure SetFieldValueByIdx(Index: Integer; Value: Variant);
    procedure SetFieldValue(const FieldName: string; Value: Variant);
    function GetFieldTypeByIdx(Index: Integer) : TFieldType;
    function GetFieldType(const FieldName: string) : TFieldType;
    function GetFieldsCount: Integer; inline;
    function GetIndidicatorByIdx(Index: Integer): Boolean;
    function GetFielFlag(Index: Integer): Byte;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure ClearItems;
    procedure Add(const AFieldName: string; AFieldValue: Variant;
      AFieldType: TFieldType; AFieldFlag: Byte);
    procedure Edit(const AFieldName: string; const AFieldFlag: Byte;
      NewFieldValue: Variant); overload;
    procedure Edit(const AFieldName: string; const AFieldFlag: Byte;
      NewFieldValue: Variant; NewFieldType: TFieldType); overload;
    procedure Insert(Index: Integer; const AFieldName: string;
      AFieldValue: Variant; AFieldType: TFieldType; AFieldFlag: Byte);

    procedure Delete(Index: Integer);
    function IndexOf(const AFieldName: string) : Integer; overload;
    function IndexOf(const AFieldName: string; const AFieldFlag: Byte)
      : Integer; overload;
    procedure Exchange(Index1, Index2: Integer);

    property IsIndicatorByIdx[Index: Integer]: Boolean
      read GetIndidicatorByIdx;
    property IsCalculatedByIdx[Index: Integer]: Boolean
      read GetCalculatedByIdx;
    property IsCalculated[const IDText: string]: Boolean
      read GetCalculated;
    property CalculatedValueByIdx[Index: Integer]: Variant
      read GetCalculatedValueByIdx write SetCalculatedValueByIdx;
    property CalculatedValue[const IDText: string]: Variant
      read GetCalculatedValue write SetCalculatedValue;
    property FieldName[Index: Integer]: string
      read GetFieldName write SetFieldName;
    property FieldValueByIdx[Index: Integer]: Variant
      read GetFieldValueByIdx write SetFieldValueByIdx;
    property FieldValue[const FieldName: string]: Variant
      read GetFieldValue write SetFieldValue;
    property FieldTypeByIdx[Index: Integer]: TFieldType
      read GetFieldTypeByIdx;
    property FieldType[const FieldName: string]: TFieldType
      read GetFieldType;
    property FieldFlag[Index: Integer]: Byte
      read GetFielFlag;
    property Fields[Index: Integer]: TDBFieldValueRec
      read GetField write SetField;
    property FieldsCount: Integer
      read GetFieldsCount;
    property RecNo: Integer
      read FRecNo write FRecNo;
  end;

  PNodeData = ^TNodeData;

  TNodeData = record
    RecordData: TRecordData;
    RecNo: Integer;
  end;

  TRecordDataClass = class of TRecordData;

  { TVirtualDBTreeDataLink }

  TVirtualDBTreeDataLink = class(TDataLink)
  private
    FVirtualDBTree: TCustomVirtualDBGrid;

  public
    constructor Create(ATree: TCustomVirtualDBGrid); virtual;

  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure DataSetScrolled(Distance: Integer); override;
  end;

{ TVirtualDBTreeColumn }

  TVirtualDBTreeColumn = class(TVirtualTreeColumn)
  private
    FFieldName : string;
    FField     : TField;
    FColumnType: TColumnType;
    FIsDefault : Boolean;
    procedure CalculateWidth(Tree: TCustomVirtualDBGrid);
    procedure InternalSetFieldName(const AFieldName: string);
    procedure SetFieldName(const AFieldName: string);
    procedure SetColumnType(Value: TColumnType);
    function GetOwnerTree: TCustomVirtualDBGrid;

    property IsDefault: Boolean
      read FIsDefault write FIsDefault;

  protected
    function GetDisplayName: string; override;

  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function Equals(OtherColumn: TObject): Boolean; override;
    procedure LoadFromStream(const Stream: TStream; Version: Integer);
    procedure SaveToStream(const Stream: TStream);

  published
    property FieldName : string
      read FFieldName write SetFieldName;
    property ColumnType: TColumnType
      read FColumnType write SetColumnType;
  end;

  { TVirtualDBTreeColumns }

  TVirtualDBTreeColumns = class(TVirtualTreeColumns)
  private
    FLastCount: Integer;
    procedure CalculateDefaultColumnsWidth;

  protected
    procedure Update(Item: TCollectionItem); override;
    function IndexOf(const FieldNameOrIDText: string;
      ColumnType: TColumnType): Integer;

  public
    property HeaderBitmap;
  end;

  TVTDBHeader = class(TVTHeader)
  protected
    function GetColumnsClass: TVirtualTreeColumnsClass; override;
  end;

  TVTDBOptions = class(TPersistent)
  private
    FOwner             : TCustomVirtualDBGrid;
    FDataLink          : TVirtualDBTreeDataLink;
    FIndicatorImIndex  : TImageIndex;
    FIndicatorAlign    : TIndicatorAlign;
    FIndicatorVAlign   : TIndicatorVAlign;
    FOddRowColor       : TColor;
    FEvenRowColor      : TColor;
    FSortingType       : TSortingType;
    FRecordCountType   : TRecordCountType;
    FSortColumnBgColor : TColor;
    FAdvOptions        : TVTDBAdvOptions;
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure SetIndicatorImIndex(Value: TImageIndex);
    procedure SetIndicatorAlign(Value: TIndicatorAlign);
    procedure SetIndicatorVAlign(Value: TIndicatorVAlign);
    procedure SetOddRowColor(Value: TColor);
    procedure SetEvenRowColor(Value: TColor);
    procedure SetAdvOptions(Value: TVTDBAdvOptions);
    procedure SetSortColumnBgColor(Value: TColor);

  protected
    function GetOwner: TPersistent; override;

  public
    constructor Create(AOwner: TCustomVirtualDBGrid); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Treeview: TCustomVirtualDBGrid read FOwner;
    property DataLink: TVirtualDBTreeDataLink read FDataLink;

  published
    property DataSource: TDataSource
      read GetDataSource write SetDataSource;
    property IndicatorImageIndex: TImageIndex
      read FIndicatorImIndex write SetIndicatorImIndex default -1;
    property IndicatorAlign: TIndicatorAlign
      read FIndicatorAlign write SetIndicatorAlign stored True default aiCenter;
    property IndicatorVAlign: TIndicatorVAlign
      read FIndicatorVAlign write SetIndicatorVAlign stored True default aiMiddle;
    property OddRowColor : TColor
      read FOddRowColor write SetOddRowColor;
    property EvenRowColor: TColor
      read FEvenRowColor write SetEvenRowColor;
    property SortingType : TSortingType
      read FSortingType write FSortingType stored True default stBuildIn;
    property RecordCountType: TRecordCountType
      read FRecordCountType write FRecordCountType stored True default rcFromDataset;
    property SortColumnBgColor: TColor
      read FSortColumnBgColor write SetSortColumnBgColor;
    property AdvOptions: TVTDBAdvOptions
      read FAdvOptions write SetAdvOptions default DefaultAdvOptions;
  end;

  { TCustomVirtualDBGrid }

  TCustomVirtualDBGrid = class(TCustomVirtualStringTree)
  private
    FInternalDataOffset: longword;
    FLoadingDataFlag   : Integer;
    FLastRecordCount   : Integer;
    FRecordCount       : Integer;

    FDBOptions         : TVTDBOptions;
    FOnGetRecordCount  : TOnGetRecordCountEvent;
    FOnCalculateValue  : TOnCalculateValueEvent;
    FOnFormatFieldValue: TOnFormatFieldValueEvent;
    FOnLoadRecord      : TOnLoadRecordEvent;
    FOnCustomSort      : TOnCustomSortEvent;
    FOnPostChanges     : TOnPostChanges;
    FOnChangeSort      : TOnChangeSort;
    FOnCompareRecord   : TOnCompareRecord;
    FOnRecordDblClick  : TOnRecordDblClick;
    FIndicatorBMP      : TBitmap;

    function GetHeader: TVTDBHeader;
    procedure AddDefaultColumns;
    procedure IndicatorBitmapNeeded;
    procedure RemoveDefaultColumns;
    procedure SetHeader(Value: TVTDBHeader);
    procedure SetDBOptions(const Value: TVTDBOptions);
    function GetOptions: TStringTreeOptions;
    procedure SetOptions(const Value: TStringTreeOptions);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

    function InternalGetNodeData(Node: PVirtualNode): PNodeData;
    procedure InternalInitializeDBTree;
    procedure UpdateVisibleDBTree(AlwaysUpdate: Boolean;
      UpdateLoadedData: Boolean = False);
    procedure UpdateDBTree(StartNode: PVirtualNode; NodeCount: Cardinal;
      AlwaysUpdate: Boolean; UpdateLoadedData: Boolean);

    function IsDataCreated(ANode: PVirtualNode): Boolean;
    // Return number of current record in database
    //   - if database is closed, returns 0
    function GetCurrentDBRecNo: Integer;
    function AddColumn(AColumnType: TColumnType;
      const AFieldName, ACaption: string; AWidth: Integer = -1)
      : TVirtualDBTreeColumn;
    procedure IncLoadingDataFlag;
    procedure DecLoadingDataFlag;
    function IsDataLoading: Boolean;
    function GetIndicatorColumn: TVirtualDBTreeColumn;
    function GetSortingColumn: TVirtualDBTreeColumn;
    procedure SetFocusToActualRecNo;
    procedure UpdateCurrentRecord;

  protected
    procedure DoFocusChange(Node: PVirtualNode; Column: TColumnIndex); override;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect); override;

    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect); override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType;
      var Text: string); override;
    procedure DoInitNode(Parent, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex;
      const Text: string); override;

    procedure DoHeaderClick(const HitInfo: TVTHeaderHitInfo); override;

    procedure DoHeaderDragged(Column: TColumnIndex;
      OldPosition: TColumnPosition); override;
    function DoFocusChanging(OldNode, NewNode: PVirtualNode;
      OldColumn, NewColumn: TColumnIndex): Boolean; override;
    procedure DoBeforeItemErase(Canvas: TCanvas; Node: PVirtualNode;
      ItemRect: TRect; var Color: TColor;
      var EraseAction: TItemEraseAction); override;

    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex)
      : Integer; override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean); override;
    procedure HandleMouseDblClick(var Message: TWMMouse;
      const HitInfo: THitInfo); override;
    procedure AdjustPaintCellRect(var PaintInfo: TVTPaintInfo;
      var NextNonEmpty: TColumnIndex); override;

    // new
    procedure DoGetRecordCount(var RecordCount: Integer); virtual;
    procedure DoCalculateValue(const IDText: string; Column: TColumnIndex;
      RecordData: TRecordData; RowIndex: Cardinal; var CalculatedValue: string;
      var CalculatedValueType: TFieldType);
    procedure DoFormatFieldValue(Column: TColumnIndex; RecordData: TRecordData;
      RowIndex: Cardinal; Field: TField; var FieldValue: Variant); virtual;
    procedure DoLoadRecord(RecordData: TRecordData;
      RowIndex: Cardinal); virtual;
    procedure DoCustomSort(Column: TColumnIndex; ColumnType: TColumnType;
      const SortBy: string; SortDirection: TSortDirection;
      var RefreshGrid: Boolean); virtual;
    procedure DoPostChanges(const FieldNameOrIDText: string;
      Column: TColumnIndex; ColumnType: TColumnType; RecordData: TRecordData;
      RowIndex: Cardinal; var NewValue: string;
      var PostChanges: Boolean); virtual;
    procedure DoChangeSort(SortColumn: TColumnIndex;
      SortDirection: TSortDirection); virtual;
    procedure DoRecordDblClick(Column: TColumnIndex; RecordData: TRecordData);

    function GetRecordCount: Integer;
    procedure LoadRecordData(ANode: PVirtualNode; CreateData: Boolean);

    function FindNodeByRecNo(ARecNo: Integer): PVirtualNode;
    function SetFocusToNode(Node: PVirtualNode; Center: Boolean = True)
      : Boolean;
    procedure GotoRecNo(NewRecNo: Integer);
    function GetNodeByIndex(Index: Cardinal): PVirtualNode;

    function GetSelectedRecord(Index: Integer): TRecordData;
    function GetFullyVisibleCount: Cardinal;
    // Value -1 in SortDirection mean that to autodetect sortdirection of column to sort by
    procedure DoSortColumn(AColumn: TColumnIndex; ASortDirection: Integer = -1);
    procedure RearrangeColumnsRecordData;

  protected
    function GetHeaderClass: TVTHeaderClass; override;
    function GetColumnClass: TVirtualTreeColumnClass; override;
    function GetOptionsClass: TTreeOptionsClass; override;
    procedure DoScroll(DeltaX, DeltaY: Integer); override;
    function GetRecordDataClass: TRecordDataClass; virtual;

    procedure DataLinkActiveChanged; virtual;
    procedure DataLinkChanged; virtual;
    procedure DataLinkRecordChanged(Field: TField); virtual;

    function GetDataSet: TDataSet; inline;

    property InternalRecordCount: Integer read FRecordCount;

    property DBOptions: TVTDBOptions read FDBOptions write SetDBOptions;
    property LinkedDataSet: TDataSet read GetDataSet;

    property OnGetRecordCount: TOnGetRecordCountEvent read FOnGetRecordCount
      write FOnGetRecordCount;
    property OnCalculateValue: TOnCalculateValueEvent read FOnCalculateValue
      write FOnCalculateValue;
    property OnFormatFieldValue: TOnFormatFieldValueEvent
      read FOnFormatFieldValue write FOnFormatFieldValue;
    property OnLoadRecord: TOnLoadRecordEvent read FOnLoadRecord
      write FOnLoadRecord;
    property OnCustomSort: TOnCustomSortEvent read FOnCustomSort
      write FOnCustomSort;
    property OnPostChanges: TOnPostChanges read FOnPostChanges
      write FOnPostChanges;
    property OnChangeSort: TOnChangeSort read FOnChangeSort write FOnChangeSort;
    property OnCompareRecord: TOnCompareRecord read FOnCompareRecord
      write FOnCompareRecord;
    property OnRecordDblClick: TOnRecordDblClick read FOnRecordDblClick
      write FOnRecordDblClick;

    // discarded VirtualTreeView properties that we don't allow to be changed by user
    property RootNodeCount stored False;
    property DefaultText stored False;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure AddDBColumn(const AFieldName, ACaption: string;
      AWidth: Integer = -1);
    procedure AddCalcColumn(const IDText: string; AWidth: Integer);
    procedure AddIndicatorColumn(AWidth: Integer);
    function GetNodeRecordData(Node: PVirtualNode): TRecordData;
    procedure SetSortColumn(const ColumnTitle: string;
      Direction: TSortDirection);
    procedure UpdateAllRecords(UpdateLoadedData: Boolean = True);
    // navigate trought the treeview
    function Navigate(FromPosition: TNavigateFromPosition;
      Delta: Integer): Boolean;
    procedure ReInitializeDBGrid;
    function IsDataOk(AData: PNodeData): Boolean;

    property SortingColumn: TVirtualDBTreeColumn
      read GetSortingColumn;
    property IndicatorColumn: TVirtualDBTreeColumn
      read GetIndicatorColumn;
    property SelectedRecord[Index: Integer]: TRecordData
      read GetSelectedRecord;

  published
    property AccessibleName;
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AnimationDuration;
    property AutoExpandDelay;
    property AutoScrollDelay;
    property AutoScrollInterval;
    property Background;
    property BackgroundOffsetX;
    property BackgroundOffsetY;
    property BiDiMode;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BorderStyle default bsSingle;
    property ButtonFillMode;
    property ButtonStyle;
    property BorderWidth;
    property BottomSpace;
    property ChangeDelay;
    property CheckImageKind;
    property ClipboardFormats;
    property Color;
    property Colors;
    property Constraints;
    property CustomCheckImages;
    property DefaultNodeHeight;
    property DefaultPasteMode;
    property DragCursor;
    property DragHeight;
    property DragKind;
    property DragImageKind;
    property DragMode;
    property DragOperations;
    property DragType;
    property DragWidth;
    property DrawSelectionMode;
    property EditDelay;
    property Enabled;
    property Font;
    property Header: TVTDBHeader
      read GetHeader write SetHeader;
    property HintMode;
    property HintAnimation;
    property HotCursor;
    property Images;
    property IncrementalSearch;
    property IncrementalSearchDirection;
    property IncrementalSearchStart;
    property IncrementalSearchTimeout;
    property Indent;
    property LineMode;
    property LineStyle;
    property Margin;
    property NodeAlignment;
    property NodeDataSize;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBarOptions;
    property SelectionBlendFactor;
    property SelectionCurveRadius;
    property ShowHint;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property TextMargin;
    property TreeOptions: TStringTreeOptions
      read GetOptions write SetOptions;
    property Visible;
    property WantTabs;

    property OnAddToSelection;
    property OnAdvancedHeaderDraw;
    property OnAfterAutoFitColumn;
    property OnAfterAutoFitColumns;
    property OnAfterCellPaint;
    property OnAfterColumnExport;
    property OnAfterColumnWidthTracking;
    property OnAfterGetMaxColumnWidth;
    property OnAfterHeaderExport;
    property OnAfterHeaderHeightTracking;
    property OnAfterItemErase;
    property OnAfterItemPaint;
    property OnAfterNodeExport;
    property OnAfterPaint;
    property OnAfterTreeExport;
    property OnBeforeAutoFitColumn;
    property OnBeforeAutoFitColumns;
    property OnBeforeCellPaint;
    property OnBeforeColumnExport;
    property OnBeforeColumnWidthTracking;
    property OnBeforeGetMaxColumnWidth;
    property OnBeforeHeaderExport;
    property OnBeforeHeaderHeightTracking;
    property OnBeforeItemErase;
    property OnBeforeItemPaint;
    property OnBeforeNodeExport;
    property OnBeforePaint;
    property OnBeforeTreeExport;
    property OnCanResize;
    property OnCanSplitterResizeColumn;
    property OnCanSplitterResizeHeader;
    property OnCanSplitterResizeNode;
    property OnChange;
    property OnChecked;
    property OnChecking;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnColumnClick;
    property OnColumnDblClick;
    property OnColumnExport;
    property OnColumnResize;
    property OnColumnWidthDblClickResize;
    property OnColumnWidthTracking;
    property OnCompareNodes;
    property OnContextPopup;
    property OnCreateDataObject;
    property OnCreateDragManager;
    property OnCreateEditor;
    property OnDblClick;
    property OnDragAllowed;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawText;
    property OnEditCancelled;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEndOperation;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnFocusChanged;
    property OnFocusChanging;
    property OnFreeNode;
    property OnGetCellIsEmpty;
    property OnGetCursor;
    property OnGetHeaderCursor;
    property OnGetHelpContext;
    property OnGetHint;
    property OnGetImageIndex;
    property OnGetImageIndexEx;
    property OnGetImageText;
    property OnGetLineStyle;
    property OnGetNodeDataSize;
    property OnGetPopupMenu;
    property OnGetText;
    property OnGetUserClipboardFormats;
    property OnHeaderClick;
    property OnHeaderDblClick;
    property OnHeaderDragged;
    property OnHeaderDraggedOut;
    property OnHeaderDragging;
    property OnHeaderDraw;
    property OnHeaderDrawQueryElements;
    property OnHeaderHeightDblClickResize;
    property OnHeaderHeightTracking;
    property OnHeaderMouseDown;
    property OnHeaderMouseMove;
    property OnHeaderMouseUp;
    property OnHotChange;
    property OnIncrementalSearch;
    property OnInitChildren;
    property OnInitNode;
    property OnKeyAction;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLoadNode;
    property OnLoadTree;
    property OnMeasureItem;
    property OnMeasureTextHeight;
    property OnMeasureTextWidth;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnNewText;
    property OnNodeCopied;
    property OnNodeCopying;
    property OnNodeExport;
    property OnNodeHeightDblClickResize;
    property OnNodeHeightTracking;
    property OnNodeMoved;
    property OnNodeMoving;
    property OnPaintBackground;
    property OnPaintText;
    property OnRemoveFromSelection;
    property OnRenderOLEData;
    property OnResetNode;
    property OnResize;
    property OnSaveNode;
    property OnSaveTree;
    property OnScroll;
    property OnShortenString;
    property OnShowScrollBar;
    property OnStartDock;
    property OnStartDrag;
    property OnStartOperation;
    property OnStateChange;
    property OnStructureChange;
    property OnUpdating;
  end;

  TVirtualDBGrid = class(TCustomVirtualDBGrid)
  public
    property Canvas;

  published
    // New events by TVirtualDBGrid
    property DBOptions;
    property OnGetRecordCount;
    property OnCalculateValue;
    property OnFormatFieldValue;
    property OnLoadRecord;
    property OnCustomSort;
    property OnPostChanges;
    property OnChangeSort;
    property OnCompareRecord;
    property OnRecordDblClick;
  end;

  TControlClick = class(TControl)
  published
    property OnClick;
  end;

function NullVar2Int(Value: Variant): Integer;
function NullVar2Double(Value: Variant): Double;
function NullVar2DateTime(Value: Variant): TDateTime;
function NullVar2Guid(Value: Variant): string;
function NullVar2Bool(Value: Variant): Boolean;

function CompareRecordData(Record1, Record2: TRecordData; Column: TColumnIndex;
  Reverse: Boolean = False): Integer;

implementation

uses
  System.Math;

type
  TIntegerList = TList<Integer>;

function NullVar2Int(Value: Variant): Integer;
begin
  if VarIsNull(Value) then
    Result := 0
  else
  begin
    try
      Result := VarAsType(Value, varInteger);
    except
      Result := 0;
    end;
  end;
end;

function NullVar2Double(Value: Variant): Double;
begin
  if VarIsNull(Value) then
    Result := 0
  else
  begin
    try
      Result := VarAsType(Value, varDouble);
    except
      Result := 0;
    end;
  end;
end;

function NullVar2DateTime(Value: Variant): TDateTime;
begin
  if VarIsNull(Value) then
    Result := 0
  else
  begin
    try
      Result := VarAsType(Value, varDate);
    except
      Result := 0;
    end;
  end;
end;

function NullVar2Guid(Value: Variant): string;
const
  NullGuid: string = '{00000000-0000-0000-0000-000000000000}';
begin
  Result := VarToStrDef(Value, NullGuid);
end;

function NullVar2Bool(Value: Variant): Boolean;
begin
  if VarIsNull(Value) then
    Result := False
  else
  begin
    try
      Result := VarAsType(Value, varBoolean);
    except
      Result := False;
    end;
  end;
end;

function CompareRecordData(Record1, Record2: TRecordData; Column: TColumnIndex;
  Reverse: Boolean): Integer;
var
  Data1Value,
    Data2Value : Variant;
begin
  // Get Data values of Data1 & Data2
  Data1Value := Record1.FieldValueByIdx[Column];
  Data2Value := Record2.FieldValueByIdx[Column];

  case Record1.FieldTypeByIdx[Column] of
    // string types
    ftString, ftMemo, ftFixedChar, ftWideString, ftFixedWideChar:
      begin
        Result := AnsiCompareText(VarToStr(Data1Value),
          VarToStr(Data2Value));
      end;

    // integer types
    ftByte, ftSmallint, ftInteger, ftLargeint, ftAutoInc, ftWord:
      begin
        Result := CompareValue(NullVar2Int(Data1Value),
          NullVar2Int(Data2Value));
      end;

    // float types
    ftFloat, ftBCD:
      begin
        Result := CompareValue(NullVar2Double(Data1Value),
          NullVar2Double(Data2Value));
      end;

    // date types
    ftDate, ftDateTime, ftTime:
      begin
        Result := CompareValue(NullVar2DateTime(Data1Value),
          NullVar2DateTime(Data2Value));
      end;

    ftCurrency :
      begin
        Result := CompareValue(NullVar2Double(Data1Value),
          NullVar2Double(Data2Value));
      end;

    ftGuid:
      begin
        Result := CompareText(NullVar2Guid(Data1Value),
          NullVar2Guid(Data2Value));
      end;

    ftBoolean :
      begin
        Result := CompareValue(Integer(NullVar2Bool(Data1Value)),
          Integer(NullVar2Bool(Data2Value)));
      end;

  else
    Result := 1;
  end;
  if Reverse then
    Result := -Result;
end;

{ TRecordData }

constructor TRecordData.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TRecordData.Destroy;
begin
  ClearItems;
  FList.Destroy;
  inherited Destroy;
end;

procedure TRecordData.ClearItems;
var
  I   : Integer;
  rec : PDBFieldValueRec;
begin
  for I := 0 to FList.Count - 1 do
  begin
    rec := PDBFieldValueRec(FList[I]);
    if rec <> nil then
      Dispose(rec);
  end;
  FList.Clear;
end;

procedure TRecordData.Edit(const AFieldName: string; const AFieldFlag: Byte;
  NewFieldValue: Variant);
var
  I : Integer;
begin
  if (AFieldName <> '') then
  begin
    for I := 0 to FieldsCount - 1 do
    begin
      if (UpperCase(Fields[I].FieldName) = UpperCase(AFieldName)) and
        (Fields[I].FieldFlag = AFieldFlag) then
      begin
        PDBFieldValueRec(FList[I])^.FieldValue := NewFieldValue;
        Break;
      end;
    end;
  end;
end;

procedure TRecordData.Edit(const AFieldName: string; const AFieldFlag: Byte;
  NewFieldValue: Variant; NewFieldType: TFieldType);
var
  I : Integer;
begin
  if (AFieldName <> '') then
  begin
    for I := 0 to FieldsCount - 1 do
    begin
      if (UpperCase(Fields[I].FieldName) = UpperCase(AFieldName)) and
        (Fields[I].FieldFlag = AFieldFlag) then
      begin
        PDBFieldValueRec(FList[I])^.FieldValue := NewFieldValue;
        PDBFieldValueRec(FList[I])^.FieldType := NewFieldType;
        Break;
      end;
    end;
  end;
end;

procedure TRecordData.Add(const AFieldName: string; AFieldValue: Variant;
  AFieldType: TFieldType;
  AFieldFlag: Byte);
var
  R: PDBFieldValueRec;
begin
  New(R);
  R^.FieldName := AFieldName;
  R^.FieldValue := AFieldValue;
  R^.FieldType := AFieldType;
  R^.FieldFlag := AFieldFlag;
  FList.Add(R);
end;

procedure TRecordData.Insert(Index: Integer; const AFieldName: string;
  AFieldValue: Variant;
  AFieldType: TFieldType; AFieldFlag: Byte);
var
  R: PDBFieldValueRec;
begin
  New(R);
  R^.FieldName := AFieldName;
  R^.FieldValue := AFieldValue;
  R^.FieldType := AFieldType;
  R^.FieldFlag := AFieldFlag;
  FList.Insert(Index, R);
end;

procedure TRecordData.Delete(Index: Integer);
var
  R: PDBFieldValueRec;
begin
  if (Index > -1) and (Index < FieldsCount) then
  begin
    R := PDBFieldValueRec(FList[Index]);
    if R <> nil then
      Dispose(R);
    FList.Delete(Index);
  end;
end;

function TRecordData.IndexOf(const AFieldName: string) : Integer;
var
  I : Integer;
begin
  Result := -1;
  for I := 0 to FieldsCount - 1 do
  begin
    if SameText(PDBFieldValueRec(FList[I])^.FieldName, AFieldName) then
    begin
      if (AFieldName <> '') or
        (PDBFieldValueRec(FList[I])^.FieldFlag = ffIndicator) then
        Result := I;
      Break;
    end;
  end;
end;

function TRecordData.IndexOf(const AFieldName: string; const AFieldFlag: Byte)
  : Integer;
var
  I : Integer;
begin
  Result := -1;
  if (AFieldName <> '') or (AFieldFlag = ffIndicator) then
  begin
    for I := 0 to FieldsCount - 1 do
    begin
      if (UpperCase(PDBFieldValueRec(FList[I])^.FieldName)
        = UpperCase(AFieldName)) and
        (PDBFieldValueRec(FList[I])^.FieldFlag = AFieldFlag) then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

procedure TRecordData.Exchange(Index1, Index2: Integer);
var
  Item1, Item2: PDBFieldValueRec;
begin
  if (Index1 < 0) or (Index1 >= FieldsCount) then
    Exit;
  if (Index2 < 0) or (Index2 >= FieldsCount) then
    Exit;

  Item1 := PDBFieldValueRec(FList[Index1]);
  Item2 := PDBFieldValueRec(FList[Index2]);

  // Item 1
  PDBFieldValueRec(FList[Index1])^.FieldName := Item2^.FieldName;
  PDBFieldValueRec(FList[Index1])^.FieldValue := Item2^.FieldValue;
  PDBFieldValueRec(FList[Index1])^.FieldType := Item2^.FieldType;
  PDBFieldValueRec(FList[Index1])^.FieldFlag := Item2^.FieldFlag;

  // Item 2
  PDBFieldValueRec(FList[Index2])^.FieldName := Item1^.FieldName;
  PDBFieldValueRec(FList[Index2])^.FieldValue := Item1^.FieldValue;
  PDBFieldValueRec(FList[Index2])^.FieldType := Item1^.FieldType;
  PDBFieldValueRec(FList[Index2])^.FieldFlag := Item1^.FieldFlag;
end;

function TRecordData.GetFieldsCount: Integer;
begin
  Result := FList.Count;
end;

function TRecordData.GetIndidicatorByIdx(Index: Integer): Boolean;
begin
  Result := False;

  if (Index > -1) and (Index < FieldsCount) then
    Result := (PDBFieldValueRec(FList[Index])^.FieldFlag = ffIndicator);
end;

function TRecordData.GetFielFlag(Index: Integer): Byte;
begin
  Result := ffUndeclared;

  if (Index > -1) and (Index < FieldsCount) then
    Result := PDBFieldValueRec(FList[Index])^.FieldFlag;
end;

function TRecordData.GetCalculatedByIdx(Index: Integer): Boolean;
begin
  Result := False;
  if (Index > -1) and (Index < FieldsCount) then
    Result := (PDBFieldValueRec(FList[Index])^.FieldFlag = ffCalculated);
end;

function TRecordData.GetCalculated(const IDText: string): Boolean;
var
  I : Integer;
begin
  Result := False;

  if IDText <> '' then
  begin
    for I := 0 to FieldsCount - 1 do
    begin
      if UpperCase(Fields[I].FieldName) = UpperCase(IDText)
      then
      begin
        Result := (Fields[I].FieldFlag = ffCalculated);
        Break;
      end;
    end;
  end;
end;

function TRecordData.GetCalculatedValueByIdx(Index: Integer): Variant;
begin
  if IsCalculatedByIdx[Index] then
    Result := FieldValueByIdx[Index]
  else
    Result := Null;
end;

procedure TRecordData.SetCalculatedValueByIdx(Index: Integer; Value: Variant);
begin
  if IsCalculatedByIdx[Index] then
    FieldValueByIdx[Index] := Value;
end;

function TRecordData.GetCalculatedValue(const IDText: string): Variant;
begin
  if IsCalculated[IDText] then
    Result := FieldValue[IDText]
  else
    Result := Null;
end;

procedure TRecordData.SetCalculatedValue(const IDText: string; Value: Variant);
begin
  if IsCalculated[IDText] then
    FieldValue[IDText] := Value;
end;

function TRecordData.GetField(Index: Integer) : TDBFieldValueRec;
begin
  if (Index > -1) and (Index < FList.Count) then
    Result := PDBFieldValueRec(FList[Index])^;
end;

procedure TRecordData.SetField(Index: Integer; Value: TDBFieldValueRec);
begin
  if (Index > -1) and (Index < FList.Count) then
    PDBFieldValueRec(FList[Index])^ := Value;
end;

function TRecordData.GetFieldName(Index: Integer) : string;
begin
  if (Index > -1) and (Index < FList.Count) then
    Result := PDBFieldValueRec(FList[Index])^.FieldName;
end;

procedure TRecordData.SetFieldName(Index: Integer; Value: string);
begin
  if (Index > -1) and (Index < FList.Count) then
    PDBFieldValueRec(FList[Index])^.FieldName := Value;
end;

function TRecordData.GetFieldValueByIdx(Index: Integer) : Variant;
begin
  if (Index < 0) or (Index >= FieldsCount) then
    Result := Null
  else
    Result := PDBFieldValueRec(FList[Index])^.FieldValue;
end;

function TRecordData.GetFieldValue(const FieldName: string) : Variant;
var
  I: Integer;
begin
  Result := Null;

  if (FieldName <> '') then
  begin
    for I := 0 to FieldsCount - 1 do
    begin
      if UpperCase(Fields[I].FieldName) = UpperCase(FieldName)
      then
      begin
        Result := PDBFieldValueRec(FList[I])^.FieldValue;
        Break;
      end;
    end;
  end;
end;

procedure TRecordData.SetFieldValueByIdx(Index: Integer; Value: Variant);
begin
  if (Index < 0) or (Index >= FieldsCount) then
    Exit;

  PDBFieldValueRec(FList[Index])^.FieldValue := Value;
end;

procedure TRecordData.SetFieldValue(const FieldName: string; Value: Variant);
var
  I : Integer;
begin
  if (FieldName <> '') then
  begin
    for I := 0 to FieldsCount - 1 do
    begin
      if UpperCase(Fields[I].FieldName) = UpperCase(FieldName)
      then
      begin
        PDBFieldValueRec(FList[I])^.FieldValue := Value;
        Break;
      end;
    end;
  end;
end;

function TRecordData.GetFieldTypeByIdx(Index: Integer) : TFieldType;
begin
  if (Index < 0) or (Index >= FieldsCount) then
    Result := ftUnknown
  else
    Result := PDBFieldValueRec(FList[Index])^.FieldType;
end;

function TRecordData.GetFieldType(const FieldName: string) : TFieldType;
var
  I : Integer;
begin
  Result := ftUnknown;

  if (FieldName <> '') then
  begin
    for I := 0 to FieldsCount - 1 do
    begin
      if UpperCase(Fields[I].FieldName) = UpperCase(FieldName)
      then
      begin
        Result := PDBFieldValueRec(FList[I])^.FieldType;
        Break;
      end;
    end;
  end;
end;

{ TVirtualDBTreeDataLink }

constructor TVirtualDBTreeDataLink.Create(ATree: TCustomVirtualDBGrid);
begin
  inherited Create;
  FVirtualDBTree := ATree;
end;

procedure TVirtualDBTreeDataLink.ActiveChanged;
begin
  if Active and Assigned(DataSource) then
    if Assigned(DataSource.DataSet) then
      if DataSource.DataSet.IsUnidirectional then
        raise Exception.Create('DataSet is unidirectional');

  FVirtualDBTree.DataLinkActiveChanged;
end;

procedure TVirtualDBTreeDataLink.DataSetChanged;
begin
  FVirtualDBTree.DataLinkChanged;
end;

procedure TVirtualDBTreeDataLink.RecordChanged(Field: TField);
begin
  FVirtualDBTree.DataLinkRecordChanged(Field);
end;

procedure TVirtualDBTreeDataLink.DataSetScrolled(Distance: Integer);
begin
  FVirtualDBTree.SetFocusToActualRecNo;
end;

{ TVirtualDBTreeColumn }

procedure TVirtualDBTreeColumn.InternalSetFieldName(const AFieldName: string);
var
  Tree   : TCustomVirtualDBGrid;
  DataSet: TDataSet;
begin
  Tree := TCustomVirtualDBGrid(GetOwnerTree);
  DataSet := Tree.LinkedDataSet;
  FFieldName := AFieldName;
  if Assigned(DataSet) then
  begin
    FField := DataSet.FindField(FFieldName);
    if FField <> nil then
      Text := FField.DisplayLabel;
    if Tree.HandleAllocated then
      CalculateWidth(Tree);
  end
  else
    FField := nil;
  //todo improve text setting when changing fieldname
  if Text = '' then
    Text := AFieldName;
end;

procedure TVirtualDBTreeColumn.CalculateWidth(Tree: TCustomVirtualDBGrid);
var
  CalcCanvas  : TCanvas;
  TextSize    : TSize;
  FieldWidth  : Integer;
  CaptionWidth: Integer;
  TestStr     : string;
begin
  if FField <> nil then
  begin
    // Calculate width
    CalcCanvas := TVirtualDBTreeColumns(Owner).HeaderBitmap.Canvas;
    CalcCanvas.Font := Owner.Header.Font;
    // width of column caption

    GetTextExtentPoint32(CalcCanvas.Handle, PChar(Text), Length(Text),
      TextSize);
    CaptionWidth := TextSize.cx + Spacing + Margin * 2;

    //todo: see how to handle DisplayWidth
    //FieldSize := FField.DisplayWidth;

    case FField.DataType of
      ftString:
        TestStr := StringOfChar('w', Min(FField.Size, 40));
      ftInteger, ftWord:
        TestStr := '999999';
      ftBoolean:
        TestStr := 'False';
    else
      TestStr := '99999999999999';
    end;

    CalcCanvas := Tree.Canvas;
    CalcCanvas.Font := Tree.Font;
    GetTextExtentPoint32(CalcCanvas.Handle, PChar(TestStr), Length(TestStr),
      TextSize);
    FieldWidth := TextSize.cx + Tree.Margin * 2;

    // which width FieldSize is greater then set it
    if CaptionWidth > FieldWidth then
    begin
      if CaptionWidth > -1 then
        Width := CaptionWidth;
    end
    else
    begin
      if FieldWidth > -1 then
        Width := FieldWidth;
    end;
  end;
end;

procedure TVirtualDBTreeColumn.SetFieldName(const AFieldName: string);
begin
  if ColumnType = ctDBField then
  begin
    if FFieldName <> AFieldName then
      InternalSetFieldName(AFieldName);
  end
  else
    FFieldName := '';
end;

procedure TVirtualDBTreeColumn.SetColumnType(Value: TColumnType);
var
  OwnerTree: TCustomVirtualDBGrid;
begin
  OwnerTree := nil;
  if Value <> FColumnType then
  begin
    if Value = ctIndicator then
    begin
      OwnerTree := GetOwnerTree;
      OwnerTree.IndicatorBitmapNeeded;
      if OwnerTree.IndicatorColumn <> nil then
        raise Exception.Create('Duplicate Indicator Column');
      //force the first position
      //necessary to set Index to 0 also to work inside Begin/EndUpdate
      Index := 0;
      Position := 0;
      Options := Options + [coFixed];
    end;

    FColumnType := Value;

    if Value <> ctDBField then
    begin
      FFieldName := '';
      if Value = ctIndicator then
      begin
        Text := '';
        if not(csLoading in OwnerTree.ComponentState) then
        begin
          Options := Options - [coDraggable, coResizable, coShowDropMark];
          Color := DefaultIndicatorColor;
        end;
      end;
    end;
    InternalSetFieldName(FFieldName);
  end;
end;

function TVirtualDBTreeColumn.GetOwnerTree: TCustomVirtualDBGrid;
begin
  Result := TCustomVirtualDBGrid(Owner.Header.Treeview);
end;

function TVirtualDBTreeColumn.GetDisplayName: string;
begin
  Result := FFieldName;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

constructor TVirtualDBTreeColumn.Create(Collection: TCollection);
begin
  FColumnType := ctDBField;
  inherited Create(Collection);
end;

procedure TVirtualDBTreeColumn.Assign(Source: TPersistent);
begin
  if Source is TVirtualDBTreeColumn then
  begin
    FieldName := TVirtualDBTreeColumn(Source).FieldName;
    ColumnType := TVirtualDBTreeColumn(Source).ColumnType;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

function TVirtualDBTreeColumn.Equals(OtherColumn: TObject): Boolean;
begin
  Result := (FieldName = TVirtualDBTreeColumn(OtherColumn).FieldName) and
    (ColumnType = TVirtualDBTreeColumn(OtherColumn).ColumnType);
end;

procedure TVirtualDBTreeColumn.LoadFromStream(const Stream: TStream;
  Version: Integer);
var
  Dummy: Integer;
  S    : string;

begin
  with Stream do
  begin
    ReadBuffer(Dummy, SizeOf(Dummy));
    SetLength(S, Dummy);
    ReadBuffer(PChar(S)^, Dummy);
    FieldName := S;
  end;
end;

procedure TVirtualDBTreeColumn.SaveToStream(const Stream: TStream);
var
  Dummy: Integer;
begin
  with Stream do
  begin
    Dummy := Length(FFieldName);
    WriteBuffer(Dummy, SizeOf(Dummy));
    WriteBuffer(PChar(FFieldName)^, Dummy);
  end;
end;

procedure TCustomVirtualDBGrid.RearrangeColumnsRecordData;
var
  Run        : PVirtualNode;
  Data       : PNodeData;
  Index      : Integer;
  Loop       : Integer;
  Maxloop    : Integer;
  RecordData : TRecordData;
  FieldName  : string;
  FieldFlag  : Byte;
  ColumnType : TColumnType;
begin
  ColumnType := ctIndicator;
  BeginUpdate;
  try
    Run := GetFirst;
    while Assigned(Run) do
    begin
      Data := InternalGetNodeData(Run);
      if IsDataOk(Data) then
      begin
        RecordData := Data.RecordData;

        if Assigned(RecordData) then
        begin
          Loop := 0;
          Maxloop := RecordData.FieldsCount;
          repeat
            FieldName := RecordData.Fields[loop].FieldName;
            FieldFlag := RecordData.FieldFlag[loop];
            case (FieldFlag) of
              ffDBField:
                ColumnType := ctDBField;
              ffCalculated:
                ColumnType := ctCalculated;
              ffIndicator:
                ColumnType := ctIndicator;
              ffUndeclared:
                begin
                  Inc(loop);
                  continue;
                end;
            end;

            Index := TVirtualDBTreeColumns(Header.Columns)
              .IndexOf(FieldName, ColumnType);

              // If the column doesnt exist then remove from RecordData
            if (Index = -1) then
            begin
              RecordData.Delete(loop);
              Dec(maxloop);
            end;

            Inc(loop);

          until (loop >= maxloop);
        end;
      end;

      Run := GetNextSibling(Run);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TVirtualDBTreeColumns.CalculateDefaultColumnsWidth;
var
  I     : Integer;
  Column: TVirtualDBTreeColumn;
  Tree  : TCustomVirtualDBGrid;
begin
  Tree := TCustomVirtualDBGrid(Header.Treeview);
  for I := 0 to Count - 1 do
  begin
    Column := TVirtualDBTreeColumn(Items[I]);
    if Column.IsDefault then
      Column.CalculateWidth(Tree);
  end;
end;

procedure TVirtualDBTreeColumns.Update(Item: TCollectionItem);
var
  Header: TVTDBHeader;
  Grid  : TCustomVirtualDBGrid;
begin
  inherited Update(Item);
  if (FLastCount <> Count) then
  begin
    if (Count < FLastCount) then
    begin
      Header := TVTDBHeader(GetOwner);
      Grid := TCustomVirtualDBGrid(Header.GetOwner);
      if (Grid <> nil) then
        Grid.RearrangeColumnsRecordData;
    end;
    FLastCount := Count;
  end;

  if (Item = nil) then
    Exit;

  if (TVirtualDBTreeColumn(Item).ColumnType = ctIndicator) and
    (TVirtualDBTreeColumn(Item).Position <> 0)
  then
  begin
    MessageDlg('Column with column type ctIndicator must have position 0',
      mtError, [mbok], 0);
    TVirtualDBTreeColumn(Item).Position := 0;
  end;
end;

function TVirtualDBTreeColumns.IndexOf(const FieldNameOrIDText: string;
  ColumnType: TColumnType): Integer;
var
  loop  : Integer;
  Column: TVirtualDBTreeColumn;
begin
  Result := -1;

  for loop := 0 to Count - 1 do
  begin
    Column := TVirtualDBTreeColumn(Items[loop]);
    if (Column.ColumnType = ColumnType) then
    begin
      case ColumnType of
        ctDBField:
          begin
            if (UpperCase(Column.FieldName) = UpperCase(FieldNameOrIDText)) then
            begin
              Result := loop;
              Break;
            end;
          end;

        ctCalculated:
          begin
            if (UpperCase(Column.Text) = UpperCase(FieldNameOrIDText)) then
            begin
              Result := loop;
              Break;
            end;
          end;

        ctIndicator:
          begin
            Result := loop;
            Break;
          end;
      end;
    end;
  end;
end;

{ TVTDBHeader }

function TVTDBHeader.GetColumnsClass: TVirtualTreeColumnsClass;
begin
  Result := TVirtualDBTreeColumns;
end;

{ TVTDBOptions }

function TVTDBOptions.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TVTDBOptions.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TVTDBOptions.SetIndicatorImIndex(Value: TImageIndex);
begin
  if Value <> FIndicatorImIndex then
  begin
    FIndicatorImIndex := Value;
    Treeview.Invalidate;
  end;
end;

procedure TVTDBOptions.SetIndicatorAlign(Value: TIndicatorAlign);
begin
  if Value <> FIndicatorAlign then
  begin
    FIndicatorAlign := Value;
    Treeview.Invalidate;
  end;
end;

procedure TVTDBOptions.SetIndicatorVAlign(Value: TIndicatorVAlign);
begin
  if Value <> FIndicatorVAlign then
  begin
    FIndicatorVAlign := Value;
    Treeview.Invalidate;
  end;
end;

procedure TVTDBOptions.SetOddRowColor(Value: TColor);
begin
  if FOddRowColor <> Value then
  begin
    FOddRowColor := Value;
    Treeview.Invalidate;
  end;
end;

procedure TVTDBOptions.SetEvenRowColor(Value: TColor);
begin
  if FEvenRowColor <> Value then
  begin
    FEvenRowColor := Value;
    Treeview.Invalidate;
  end;
end;

procedure TVTDBOptions.SetAdvOptions(Value: TVTDBAdvOptions);
var
  WAutoOptions     : TVTAutoOptions;
  WMiscOptions     : TVTMiscOptions;
  WPaintOptions    : TVTPaintOptions;
  WSelectionOptions: TVTSelectionOptions;
begin
  WAutoOptions := Treeview.TreeOptions.AutoOptions;
  WMiscOptions := Treeview.TreeOptions.MiscOptions;
  WPaintOptions := Treeview.TreeOptions.PaintOptions;
  WSelectionOptions := Treeview.TreeOptions.SelectionOptions;

  FAdvOptions := Value;

  if (aoEditable in FAdvOptions) and (not(aoFullRowSelect in FAdvOptions))
  then
    Include(WMiscOptions, toEditable)
  else
    Exclude(WMiscOptions, toEditable);

  if aoEditOnClick in FAdvOptions
  then
    Include(WMiscOptions, toEditOnClick)
  else
    Exclude(WMiscOptions, toEditOnClick);

  if aoEditOnDblClick in FAdvOptions then
  begin
    Include(WMiscOptions, toEditOnDblClick);
    Exclude(WMiscOptions, toToggleOnDblClick);
  end
  else
  begin
    Exclude(WMiscOptions, toEditOnDblClick);
    Include(WMiscOptions, toToggleOnDblClick);
  end;

  if aoShowHorzLines in FAdvOptions
  then
    Include(WPaintOptions, toShowHorzGridLines)
  else
    Exclude(WPaintOptions, toShowHorzGridLines);

  if aoShowVertLines in FAdvOptions
  then
    Include(WPaintOptions, toShowVertGridLines)
  else
    Exclude(WPaintOptions, toShowVertGridLines);

  if aoCenterScrollIntoView in FAdvOptions
  then
    Include(WSelectionOptions, toCenterScrollIntoView)
  else
    Exclude(WSelectionOptions, toCenterScrollIntoView);

  if aoFullRowSelect in FAdvOptions
  then
    Include(WSelectionOptions, toFullRowSelect)
  else
    Exclude(WSelectionOptions, toFullRowSelect);

  if aoMultiSelect in FAdvOptions
  then
    Include(WSelectionOptions, toMultiSelect)
  else
    Exclude(WSelectionOptions, toMultiSelect);

  Treeview.TreeOptions.AutoOptions := WAutoOptions;
  Treeview.TreeOptions.MiscOptions := WMiscOptions;
  Treeview.TreeOptions.PaintOptions := WPaintOptions;
  Treeview.TreeOptions.SelectionOptions := WSelectionOptions;
  Treeview.Invalidate;
end;

procedure TVTDBOptions.SetSortColumnBgColor(Value: TColor);
begin
  if FSortColumnBgColor <> Value then
  begin
    FSortColumnBgColor := Value;
    Treeview.Invalidate;
  end;
end;

function TVTDBOptions.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

constructor TVTDBOptions.Create(AOwner: TCustomVirtualDBGrid);
begin
  inherited Create;
  FOwner := AOwner;
  FDataLink := TVirtualDBTreeDataLink.Create(AOwner);
  FIndicatorImIndex := -1;
  FIndicatorAlign := aiCenter;
  FIndicatorVAlign := aiMiddle;
  FEvenRowColor := clWindow;
  FOddRowColor := clWhiteSmoke;
  FSortingType := stBuildIn;
  FRecordCountType := rcFromDataset;
  FSortColumnBgColor := clLightYellow;
  AdvOptions := DefaultAdvOptions;
end;

destructor TVTDBOptions.Destroy;
begin
  FDataLink.Destroy;
  inherited Destroy;
end;

procedure TVTDBOptions.Assign(Source: TPersistent);
begin
  if Source is TVTDBOptions then
  begin
    DataSource := TVTDBOptions(Source).DataSource;
    IndicatorImageIndex := TVTDBOptions(Source).IndicatorImageIndex;
    IndicatorAlign := TVTDBOptions(Source).IndicatorAlign;
    IndicatorVAlign := TVTDBOptions(Source).IndicatorVAlign;
    OddRowColor := TVTDBOptions(Source).OddRowColor;
    EvenRowColor := TVTDBOptions(Source).EvenRowColor;
    SortingType := TVTDBOptions(Source).SortingType;
    AdvOptions := TVTDBOptions(Source).AdvOptions;
  end
  else
    inherited Assign(Source);

end;

{ TCustomVirtualDBGrid }

function TCustomVirtualDBGrid.GetHeader: TVTDBHeader;
begin
  Result := TVTDBHeader( inherited Header);
end;

procedure TCustomVirtualDBGrid.AddDefaultColumns;
var
  FieldList: TFields;
  Field    : TField;
  Column   : TVirtualDBTreeColumn;
  I        : Integer;
begin
  FieldList := LinkedDataSet.Fields;
  if FieldList.Count = 0 then
    Exit;
  Header.Columns.BeginUpdate;
  try
    // If aoAutoInsertIndicator is set then automatically add indicator column
    if aoAutoInsertIndicator in DBOptions.AdvOptions then
    begin
      Column := AddColumn(ctIndicator, '', '', 15);
      Column.IsDefault := True;
    end;
    for I := 0 to FieldList.Count - 1 do
    begin
      Field := FieldList[I];
      Column := AddColumn(ctDBField, Field.FieldName, Field.DisplayName, -1);
      Column.IsDefault := True;
    end;
  finally
    Header.Columns.EndUpdate;
  end;
end;

procedure TCustomVirtualDBGrid.IndicatorBitmapNeeded;
begin
  if FIndicatorBMP = nil then
  begin
    FIndicatorBMP := TBitmap.Create;
    FIndicatorBMP.SetSize(10, 10);
    FIndicatorBMP.Canvas.Brush.Color := clFuchsia;
    FIndicatorBMP.Canvas.FillRect(Rect(0, 0, 12, 12));

    FIndicatorBMP.Canvas.Brush.Color := clBlack;
    FIndicatorBMP.Canvas.Pen.Color := clBlack;
    FIndicatorBMP.Canvas.Polygon([Point(0, 0), Point(4, 4), Point(4, 5),
      Point(0, 9)]);
    FIndicatorBMP.Canvas.Polygon([Point(5, 0), Point(9, 4), Point(9, 5),
      Point(5, 9)]);
    FIndicatorBMP.TransparentMode := tmFixed;
    FIndicatorBMP.TransparentColor := clFuchsia;
    FIndicatorBMP.Transparent := True;
  end;
end;

procedure TCustomVirtualDBGrid.RemoveDefaultColumns;
var
  I      : Integer;
  Column : TVirtualDBTreeColumn;
  Columns: TVirtualDBTreeColumns;
begin
  Columns := TVirtualDBTreeColumns(Header.Columns);
  Columns.BeginUpdate;
  try
    I := 0;
    while I < Columns.Count do
    begin
      Column := TVirtualDBTreeColumn(Columns[I]);
      if Column.IsDefault then
        Column.Destroy
      else
        Inc(I);
    end;
  finally
    Columns.EndUpdate;
  end;
end;

procedure TCustomVirtualDBGrid.SetHeader(Value: TVTDBHeader);
begin
  inherited Header := Value;
end;

constructor TCustomVirtualDBGrid.Create(Owner: TComponent);
begin
  inherited;
  DefaultText := '';
  FInternalDataOffset := AllocateInternalDataArea(SizeOf(TNodeData));

  Header.Options := [hoColumnResize, hoDrag, hoHotTrack, hoShowHint,
    hoShowSortGlyphs, hoVisible];
  Header.SortColumn := -1;

  with TreeOptions do
  begin
    //AnimationOptions:= [];
    AutoOptions := [toAutoDropExpand, toAutoScroll, toAutoTristateTracking,
      toAutoDeleteMovedNodes];
    MiscOptions := [toAcceptOLEDrop, toGridExtensions, toInitOnSave,
      toToggleOnDblClick, toWheelPanning];
    PaintOptions := [toHideFocusRect, toShowDropmark, toThemeAware,
      toUseBlendedImages];
    SelectionOptions := [toDisableDrawSelection, toExtendedFocus,
      toMiddleClickSelect,
      toRightClickSelect];
    StringOptions := [toSaveCaptions, toAutoAcceptEditChange];
  end;
  FDBOptions := TVTDBOptions.Create(self);

  if FocusedNode <> nil then
    Selected[FocusedNode] := True;
  TVirtualDBTreeColumns(Header.Columns).CalculateDefaultColumnsWidth;
end;

destructor TCustomVirtualDBGrid.Destroy;
begin
  FDBOptions.Free;
  FIndicatorBMP.Free;
  inherited Destroy;
end;

function TCustomVirtualDBGrid.GetIndicatorColumn: TVirtualDBTreeColumn;
var
  IndColumn: TColumnIndex;
begin
  Result := nil;
  // indicator column is always at position 0
  IndColumn := Header.Columns.ColumnFromPosition(0);
  if IndColumn <> NoColumn then
  begin
    Result := TVirtualDBTreeColumn(Header.Columns[IndColumn]);
    if Result.ColumnType <> ctIndicator then
      Result := nil;
  end;
end;

function TCustomVirtualDBGrid.GetSortingColumn: TVirtualDBTreeColumn;
var
  Index: Integer;
begin
  Result := nil;

  Index := Header.SortColumn;
  if (Index > NoColumn) then
    Result := TVirtualDBTreeColumn(Header.Columns[Index]);
end;

function TCustomVirtualDBGrid.GetHeaderClass: TVTHeaderClass;
begin
  Result := TVTDBHeader;
end;

function TCustomVirtualDBGrid.GetColumnClass: TVirtualTreeColumnClass;
begin
  Result := TVirtualDBTreeColumn;
end;

function TCustomVirtualDBGrid.GetOptionsClass: TTreeOptionsClass;
begin
  Result := TStringTreeOptions;
end;

function TCustomVirtualDBGrid.GetOptions: TStringTreeOptions;
begin
  Result := TStringTreeOptions( inherited TreeOptions);
end;

procedure TCustomVirtualDBGrid.SetOptions(const Value: TStringTreeOptions);
begin
  inherited TreeOptions := Value;
end;

procedure TCustomVirtualDBGrid.DataLinkActiveChanged;
var
  ColumnPosition: Integer;
begin
  if (not(csLoading in ComponentState)) and
    (not IsDataLoading) then
  begin
    IncLoadingDataFlag;
    BeginUpdate;
    try
      if Assigned(LinkedDataSet) and LinkedDataSet.Active then
      begin
        FRecordCount := GetRecordCount;
        FLastRecordCount := FRecordCount;
        if (Header.Columns.Count = 0) or
          (aoAddDefaultColumns in DBOptions.AdvOptions) then
          AddDefaultColumns;
        InternalInitializeDBTree;
        if Header.Columns.Count > 0 then
        begin
          ColumnPosition := IfThen(IndicatorColumn <> nil, 1, 0);
          FocusedColumn := Header.Columns.ColumnFromPosition(ColumnPosition);
        end;
      end
      else
      begin
        FRecordCount := 0;
        FLastRecordCount := 0;
        RemoveDefaultColumns;
        Clear;
      end;
    finally
      EndUpdate;
      DecLoadingDataFlag;
    end;

    if Assigned(LinkedDataSet) and LinkedDataSet.Active then
    begin
       //todo: refactor to call UpdateDBTree earlier
      if (DBOptions.SortingType = stBuildIn) and
        (Header.SortColumn <> NoColumn) then
      begin
         // The sort feature requires that all data be loaded
        UpdateAllRecords(False);
        SortTree(Header.SortColumn, Header.SortDirection);
      end
      else
        UpdateVisibleDBTree(True);

      if FRecordCount > 0 then
        SetFocusToNode(FindNodeByRecNo(1), False);
    end;
  end;
end;

procedure TCustomVirtualDBGrid.DataLinkChanged;
var
  DoSort: Boolean;
begin
  // we can reflect changes in database(like insert or delete record(s))
  // only if DBOptions.RecordCountType = rcFromDataset is set and we know
  // how many records is in the dataset

  if (not(csLoading in ComponentState)) and
    (DBOptions.RecordCountType = rcFromDataset) and
    (not IsDataLoading) then
  begin
    if Assigned(LinkedDataSet) and LinkedDataSet.Active then
    begin
      DoSort := (DBOptions.SortingType = stBuildIn) and
        (Header.SortColumn <> NoColumn);
       //Skip GetRecordCount and retrieve RecordCount directly from dataset
       //since we already know that RecordCountType is rcFromDataset
      FRecordCount := LinkedDataSet.RecordCount;
       // If old record count(fLastRecordCount) <> to new record count
       // then, there was add or remove some record and we want to reflect this changes
       // Otherwise is necessary to sync to dataset RecNo to reflect a call to Last/First
      if (FRecordCount <> FLastRecordCount) then
      begin
        ReInitializeDBGrid;
        FLastRecordCount := FRecordCount;
        if DoSort then
        begin
          UpdateAllRecords;
          SortTree(Header.SortColumn, Header.SortDirection);
        end;
      end
      else
      begin
         //todo track the dataset state to avoid unnecessary calls to UpdateAllRecords
        if LinkedDataSet.State <> dsInsert then
        begin
          UpdateAllRecords;
          if DoSort then
            SortTree(Header.SortColumn, Header.SortDirection);
          SetFocusToActualRecNo;
        end;
      end;
    end;
  end;

end;

procedure TCustomVirtualDBGrid.DataLinkRecordChanged(Field: TField);
begin
  if not(csLoading in ComponentState) and not IsDataLoading and
    Assigned(LinkedDataSet) then
  begin
    IncLoadingDataFlag;
    try
      UpdateCurrentRecord;
       //disable the sort here because it'll be sorted later in response to post
       //check if the changed field is the sort field and do the sort if so
      if (Field <> nil) and (DBOptions.SortingType = stBuildIn)
        and (Header.SortColumn <> NoColumn)
        and (TVirtualDBTreeColumns(Header.Columns).IndexOf(Field.FieldName,
        ctDBField) = Header.SortColumn) then
      begin
        UpdateAllRecords(False);
        SortTree(Header.SortColumn, Header.SortDirection);
      end;

    finally
      DecLoadingDataFlag;
    end;
  end;
end;

function TCustomVirtualDBGrid.GetDataSet: TDataSet;
begin
  Result := FDBOptions.FDataLink.DataSet;
end;

procedure TCustomVirtualDBGrid.DoBeforeItemErase(Canvas: TCanvas;
  Node: PVirtualNode; ItemRect: TRect; var Color: TColor;
  var EraseAction: TItemEraseAction);
begin
  if (not(aoStrippedRows in DBOptions.AdvOptions)) then
    Exit;

  with Canvas do
  begin
    if Odd(Node.Index)
    then
      Color := DBOptions.OddRowColor
    else
      Color := DBOptions.EvenRowColor;

    EraseAction := eaColor;
  end;

  inherited;
end;

procedure TCustomVirtualDBGrid.DoFocusChange(Node: PVirtualNode;
  Column: TColumnIndex);
var
  Data: PNodeData;
begin
  if (LinkedDataSet <> nil) and (LinkedDataSet.Active) and
    not IsDataLoading then
  begin
    Data := InternalGetNodeData(Node);
    if IsDataOk(Data) then
      GotoRecNo(Data.RecordData.RecNo);
  end;
  inherited DoFocusChange(Node, Column);
end;

procedure TCustomVirtualDBGrid.DoBeforeCellPaint(Canvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if (aoHighlightSortColumn in DBOptions.AdvOptions) and
    (Column > NoColumn) then
  begin
    Canvas.Brush.Color := Header.Columns[Column].Color;
    if ((Column <> FocusedColumn) or (Node <> FocusedNode)) and
      (Column = Header.SortColumn)
    then
    begin
      Canvas.Brush.Color := DBOptions.SortColumnBgColor;
      Canvas.FillRect(CellRect);
    end;
  end;

  inherited;
end;

procedure TCustomVirtualDBGrid.DoAfterCellPaint(Canvas: TCanvas;
  Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);
var
  X          : Integer;
  Y          : Integer;
  IconWidth  : Integer;
  IconHeight : Integer;
  ImageIndex : Integer;
  R          : TRect;
  GridColumn : TVirtualDBTreeColumn;
begin
  X := 0;
  Y := 0;
  GridColumn := TVirtualDBTreeColumn(Header.Columns[Column]);
  if GridColumn.ColumnType = ctIndicator then
  begin
    R := CellRect;
    // draw indicator arrow
    with Canvas do
    begin
      if toShowVertGridLines in TreeOptions.PaintOptions then
        Inc(R.Right);
      if toShowHorzGridLines in TreeOptions.PaintOptions then
        Inc(R.Bottom);

      Brush.Color := GridColumn.Color;
      FillRect(R);
      DrawEdge(Handle, R, BDR_RAISEDINNER, BF_RECT {or BF_MIDDLE});

      if Node = FocusedNode then
      begin
        ImageIndex := DBOptions.IndicatorImageIndex;
        // Get Indicator bitmap width
        if (ImageIndex > -1) and (Images <> nil) and
          (ImageIndex < Images.Count) then
        begin
          IconWidth := Images.Width;
          IconHeight := Images.Height;
        end
        else
        begin
          IconWidth := FIndicatorBMP.Width;
          IconHeight := FIndicatorBMP.Height;
          ImageIndex := -1;
        end;

        // Calculate X coordinate
        case DBOptions.IndicatorAlign of
          aiLeft:
            X := 0;
          aiCenter:
            X := ((R.Right - R.Left) - IconWidth) div 2 + 1;
          aiRight:
            X := (R.Right - R.Left) - IconWidth;
        end;

        // Calculate Y coordinate
        case DBOptions.IndicatorVAlign of
          aiTop:
            Y := 0;
          aiMiddle:
            Y := ((R.Bottom - R.Top) - IconHeight) div 2 + 1;
          aiBottom:
            Y := (R.Bottom - R.Top) - IconHeight;
        end;

        if ImageIndex = -1 then
          Canvas.Draw(X, Y, FIndicatorBMP)
        else
          Images.Draw(Canvas, X, Y, ImageIndex);
      end;
    end;
  end
  else
    inherited DoAfterCellPaint(Canvas, Node, Column, CellRect);
end;

procedure TCustomVirtualDBGrid.DoFreeNode(Node: PVirtualNode);
var
  Data: PNodeData;
begin
  Data := InternalGetNodeData(Node);
  if IsDataOk(Data) then
    FreeAndNil(Data.RecordData);
  inherited DoFreeNode(Node);
end;

procedure TCustomVirtualDBGrid.DoGetText(Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var Text: string);
var
  Data      : PNodeData;
  DBColumn  : TVirtualDBTreeColumn;
  FieldValue: Variant;
begin
  if Column <= NoColumn then
    Exit;
  DBColumn := TVirtualDBTreeColumn(Header.Columns[Column]);
  if (DBColumn.ColumnType = ctIndicator) or (DBColumn.Style = vsOwnerDraw) then
    Exit;
  Data := InternalGetNodeData(Node);
  if IsDataOk(Data) then
  begin
    //todo: add a Column > FieldIndex map to avoid using fieldname for the data lookup
    if DBColumn.ColumnType = ctDBField then
      FieldValue := Data.RecordData.FieldValue[DBColumn.FieldName]
    else
      FieldValue := Data.RecordData.FieldValueByIdx[DBColumn.Index];
    Text := VarToStr(FieldValue);
  end;
end;

procedure TCustomVirtualDBGrid.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  NodeData: PNodeData;
begin
  inherited DoInitNode(Parent, Node, InitStates);
  NodeData := InternalGetNodeData(Node);
  NodeData^.RecNo := Node^.Index + 1;
end;

procedure TCustomVirtualDBGrid.DoNewText(Node: PVirtualNode;
  Column: TColumnIndex; const Text: string);
var
  WField     : TField;
  DBColumn   : TVirtualDBTreeColumn;
  Data       : PNodeData;
  PostChanges: Boolean;
  PostText   : string;
begin
  //todo: see if these Column checks are really necessary
  if Column <= NoColumn then
    Exit;

  // if column is ctIndicator then Exit
  DBColumn := TVirtualDBTreeColumn(Header.Columns[Column]);
  if DBColumn.ColumnType = ctIndicator then
    Exit;

  Data := InternalGetNodeData(Node);
  if not IsDataOk(Data) then
    Exit;

  PostText := Text;

  if DBColumn.ColumnType = ctCalculated then
  begin
    PostChanges := True;
    DoPostChanges(DBColumn.Text, Column, DBColumn.ColumnType, Data.RecordData,
      Node.Index,
      PostText, PostChanges);
    if PostChanges then
      Data.RecordData.CalculatedValue[DBColumn.Text] := PostText;
  end
  else
  begin
    PostChanges := False;
    DoPostChanges(DBColumn.FieldName, Column, DBColumn.ColumnType,
      Data.RecordData,
      Node.Index, PostText, PostChanges);
    if PostChanges and Assigned(LinkedDataSet) and LinkedDataSet.CanModify then
    begin
      WField := LinkedDataSet.FindField(DBColumn.FieldName);
      if WField <> nil then
      begin
        //todo: see if is necessary some kind of error handling here
        try
          LinkedDataSet.Edit;
          WField.AsString := PostText;
          LinkedDataSet.Post;
        except
        end;
      end;
    end;
  end;
  inherited DoNewText(Node, Column, Text);
end;

procedure TCustomVirtualDBGrid.DoHeaderClick(const HitInfo: TVTHeaderHitInfo);
begin
  if (DBOptions.SortingType <> stNone) and
    (aoAllowSorting in DBOptions.AdvOptions) then
  begin
    DoSortColumn(HitInfo.Column);
    DoChangeSort(Header.SortColumn, Header.SortDirection);
  end;
  inherited DoHeaderClick(HitInfo);
end;

procedure TCustomVirtualDBGrid.DoHeaderDragged(Column: TColumnIndex;
  OldPosition: TColumnPosition);
var
  loop: Integer;
begin
  if (Column > NoColumn) then
    if (TVirtualDBTreeColumn(Header.Columns[Column]).ColumnType <> ctIndicator)
    then
      inherited DoHeaderDragged(Column, OldPosition);

  with Header do
  begin
    for loop := 0 to Columns.Count - 1 do
    begin
      if (TVirtualDBTreeColumn(Columns[loop]).ColumnType = ctIndicator) then
        if (Columns[loop].Position <> 0) then
        begin
          Columns[loop].Position := 0;
          Invalidate(Columns[loop]);
          Exit;
        end;
    end;
  end;
end;

function TCustomVirtualDBGrid.DoFocusChanging(OldNode, NewNode: PVirtualNode;
  OldColumn, NewColumn: TColumnIndex): Boolean;
begin
  if NewColumn <= NoColumn then
  begin
    Result := False;
    Exit;
  end;

  Result := (TVirtualDBTreeColumn(Header.Columns[NewColumn]).ColumnType <>
    ctIndicator);

  if Result then
    Result := inherited DoFocusChanging(OldNode, NewNode, OldColumn, NewColumn);
end;

function TCustomVirtualDBGrid.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): Integer;
var
  Data1,
    Data2: PNodeData;
  ColType: TColumnType;

begin
  Result := 0;
  try
    //todo: override Sort method and move all these checks to there

    // If Column is out of bounds then do nothing ...
    if (Column < 0) or (Column >= Header.Columns.Count) then
      Exit;

    ColType := TVirtualDBTreeColumn(Header.Columns[Column]).ColumnType;

    // If column is ctIndicator then do nothing ...
    if ColType = ctIndicator then
      Exit;

    // If column is DBField and we dont want to sort this type of column then do nothing ...
    if (ColType = ctDBField) and
      (not(aoSortDBFieldColumns in DBOptions.AdvOptions)) then
      Exit;

    // If column is ctCalculated and we dont want to sort this type of column then do nothing ...
    if (ColType = ctCalculated) and
      (not(aoSortCalculatedColumns in DBOptions.AdvOptions)) then
      Exit;

    Data1 := InternalGetNodeData(Node1);
    Data2 := InternalGetNodeData(Node2);

    if (not IsDataOk(Data1)) or (not IsDataOk(Data2))
    then
      Exit;

    if (Data1^.RecordData.FieldTypeByIdx[Column] <>
      Data2^.RecordData.FieldTypeByIdx[Column])
    then
      Exit;

    if Assigned(FOnCompareRecord) then
      FOnCompareRecord(self, Data1.RecordData, Data2.RecordData, Column, Result)
    else
      Result := CompareRecordData(Data1.RecordData, Data2.RecordData, Column);

  except
    Result := 1;
  end;
end;

procedure TCustomVirtualDBGrid.DoCanEdit(Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
var
  ColumnType: TColumnType;
  Col       : TVirtualDBTreeColumn;
begin
  inherited DoCanEdit(Node, Column, Allowed);
  if not Allowed then
    Exit;
  if (Column > NoColumn) then
  begin
    Col := TVirtualDBTreeColumn(Header.Columns[Column]);
    ColumnType := Col.ColumnType;

    case ColumnType of
      ctIndicator:
        Allowed := False;

      ctDBField:
        Allowed := Allowed and (aoEditDBFieldColumns in DBOptions.AdvOptions);

      ctCalculated:
        Allowed := Allowed and
          (aoEditCalculatedColumns in DBOptions.AdvOptions);
    end;
  end
  else
    Allowed := False;
end;

procedure TCustomVirtualDBGrid.HandleMouseDblClick(var Message: TWMMouse;
  const HitInfo: THitInfo);
var
  Column: TVirtualDBTreeColumn;
  Field : TField;
  Data  : PNodeData;
begin
  inherited HandleMouseDblClick(Message, HitInfo);
  if HitInfo.HitNode <> nil then
  begin
    //toggle Boolean fields
    if (aoAutoToggleBoolean in FDBOptions.AdvOptions) and
      (HitInfo.HitNode = FocusedNode) and
      (HitInfo.HitColumn > NoColumn) then
    begin
      Column := TVirtualDBTreeColumn(Header.Columns[HitInfo.HitColumn]);
      if (Column.ColumnType = ctDBField) and Assigned(LinkedDataSet) then
      begin
        Field := LinkedDataSet.FindField(Column.FieldName);
        if (Field <> nil) and (Field.DataType = ftBoolean) then
        begin
          //todo: implement toggling through single click
          //todo: is necessary also to block edit through another ways (F2)
          if aoEditOnDblClick in FDBOptions.AdvOptions then
            DoCancelEdit;
          LinkedDataSet.Edit;
          Field.AsBoolean := not Field.AsBoolean;
          LinkedDataSet.Post;
        end;
      end;
    end;
    //fire extended dblclick event
    Data := InternalGetNodeData(HitInfo.HitNode);
    if IsDataOk(Data) then
      DoRecordDblClick(HitInfo.HitColumn, Data^.RecordData);
  end;
end;

procedure TCustomVirtualDBGrid.AdjustPaintCellRect(var PaintInfo: TVTPaintInfo;
  var NextNonEmpty: TColumnIndex);
begin
  inherited;
  if (PaintInfo.Column <= NoColumn) then
    Exit;

  with PaintInfo do
  begin
    if TVirtualDBTreeColumn(Header.Columns[Column]).ColumnType = ctIndicator
    then
    begin
      Exclude(PaintOptions, poDrawSelection);
    end;
  end;
end;

procedure TCustomVirtualDBGrid.DoGetRecordCount(var RecordCount: Integer);
begin
  if Assigned(FOnGetRecordCount) then
    FOnGetRecordCount(self, RecordCount);
end;

procedure TCustomVirtualDBGrid.DoCalculateValue(const IDText: string;
  Column: TColumnIndex; RecordData: TRecordData; RowIndex: Cardinal;
  var CalculatedValue: string; var CalculatedValueType: TFieldType);
begin
  if Assigned(FOnCalculateValue) then
    FOnCalculateValue(self, IDText, Column, RecordData, RowIndex,
      CalculatedValue, CalculatedValueType);
end;

procedure TCustomVirtualDBGrid.DoFormatFieldValue(Column: TColumnIndex;
  RecordData: TRecordData; RowIndex: Cardinal; Field: TField;
  var FieldValue: Variant);
begin
  if Assigned(FOnFormatFieldValue) then
    FOnFormatFieldValue(self, Column, RecordData, RowIndex, Field, FieldValue);
end;

procedure TCustomVirtualDBGrid.DoLoadRecord(RecordData: TRecordData;
  RowIndex: Cardinal);
begin
  if Assigned(FOnLoadRecord) then
    FOnLoadRecord(self, RecordData, RowIndex);
end;

procedure TCustomVirtualDBGrid.DoCustomSort(Column: TColumnIndex;
  ColumnType: TColumnType; const SortBy: string; SortDirection: TSortDirection;
  var RefreshGrid: Boolean);
begin
  if Assigned(FOnCustomSort) then
    FOnCustomSort(self, Column, ColumnType, SortBy, SortDirection, RefreshGrid);
end;

procedure TCustomVirtualDBGrid.DoPostChanges(const FieldNameOrIDText: string;
  Column: TColumnIndex; ColumnType: TColumnType; RecordData: TRecordData;
  RowIndex: Cardinal; var NewValue: string; var PostChanges: Boolean);
begin
  if Assigned(FOnPostChanges)
  then
    FOnPostChanges(self, FieldNameOrIDText, Column, ColumnType, RecordData,
      RowIndex, NewValue, PostChanges)
  else
    PostChanges := True;
end;

procedure TCustomVirtualDBGrid.DoScroll(DeltaX, DeltaY: Integer);
begin
//todo: elaborate an algorithm to update only the scrolled nodes ??
  if DeltaY <> 0 then
    UpdateVisibleDBTree(False);
  inherited DoScroll(DeltaX, DeltaY);
end;

procedure TCustomVirtualDBGrid.DoChangeSort(SortColumn: TColumnIndex;
  SortDirection: TSortDirection);
begin
  if Assigned(FOnChangeSort) then
    FOnChangeSort(self, SortColumn, SortDirection);
end;

procedure TCustomVirtualDBGrid.DoRecordDblClick(Column: TColumnIndex;
  RecordData: TRecordData);
begin
  if Assigned(FOnRecordDblClick) then
    FOnRecordDblClick(self, Column, RecordData);
end;

function TCustomVirtualDBGrid.GetRecordCount: Integer;
begin
  Result := 0;
  if DBOptions.RecordCountType = rcFromDataset then
  begin
    if Assigned(LinkedDataSet) and LinkedDataSet.Active then
      Result := LinkedDataSet.RecordCount;
  end
  else
    DoGetRecordCount(Result);
end;

function TCustomVirtualDBGrid.FindNodeByRecNo(ARecNo: Integer): PVirtualNode;
var
  Node: PVirtualNode;
  Data: PNodeData;
begin
  Result := nil;

  Node := GetFirst;
  while Node <> nil do
  begin
    Data := InternalGetNodeData(Node);

    if Data <> nil then
    begin
      if Data^.RecNo = ARecNo then
      begin
        Result := Node;
        Break;
      end;
    end;
    Node := GetNext(Node);
  end;
end;

function TCustomVirtualDBGrid.SetFocusToNode(Node: PVirtualNode;
  Center: Boolean): Boolean;
begin
  Result := Assigned(Node);
  if not Result then
    Exit;

  if Assigned(FocusedNode) then
    Selected[FocusedNode] := False;

  //call DoFocusNode instead of FocusedNode to avoid calling DoFocusChange
  DoFocusNode(Node, False);
  //don't set Selected if Handle is not allocated to avoid premature handle creation
  if HandleAllocated then
    Selected[Node] := True;
  //todo: change ScrollIntoView behavior?
  ScrollIntoView(Node, Center);
end;

procedure TCustomVirtualDBGrid.GotoRecNo(NewRecNo: Integer);
begin
  //Assumes LinkedDataset is <> nil and active
  IncLoadingDataFlag;
  try
    //todo: use LinkedDataset.RecNo directly??
    //Maybe not since some datasets don't support setting RecNo
    LinkedDataSet.MoveBy(NewRecNo - LinkedDataSet.RecNo);
  finally
    DecLoadingDataFlag;
  end;
end;

function TCustomVirtualDBGrid.GetNodeByIndex(Index: Cardinal): PVirtualNode;
begin
  Result := GetFirst;
  while Assigned(Result) and (Result.Index <> Index) do
    Result := GetNextSibling(Result);

  if (Result.Index <> Index) then
    Result := nil;
end;

function TCustomVirtualDBGrid.GetSelectedRecord(Index: Integer): TRecordData;
var
  Run, Node: PVirtualNode;
  Data     : PNodeData;
  I        : Integer;
begin
  Result := nil;

  if (Index > -1) and (Index < SelectedCount)
  then
  begin
    Node := nil;
    Run := GetFirstSelected;
    I := 0;
    while Assigned(Run) and (I < SelectedCount) do
    begin
      if I = Index then
      begin
        Node := Run;
        Break;
      end;

      Run := GetNextSelected(Run);
      Inc(I);
    end;

    if Assigned(Node) then
    begin
      Data := InternalGetNodeData(Node);
      if Data <> nil then
        Result := Data.RecordData;
    end;
  end;
end;

function TCustomVirtualDBGrid.AddColumn(AColumnType: TColumnType;
  const AFieldName, ACaption: string;
  AWidth: Integer = -1): TVirtualDBTreeColumn;
begin
  Result := TVirtualDBTreeColumn(Header.Columns.Add);
  Result.ColumnType := AColumnType;
  Result.Text := ACaption;

  if AColumnType = ctDBField then
    Result.FieldName := AFieldName;

  if AWidth <> -1 then
    Result.Width := AWidth;
end;

procedure TCustomVirtualDBGrid.IncLoadingDataFlag;
begin
  Inc(FLoadingDataFlag);
end;

procedure TCustomVirtualDBGrid.DecLoadingDataFlag;
begin
  Dec(FLoadingDataFlag);
end;

function TCustomVirtualDBGrid.IsDataLoading: Boolean;
begin
  Result := (FLoadingDataFlag <> 0);
end;

procedure TCustomVirtualDBGrid.AddDBColumn(const AFieldName, ACaption: string;
  AWidth: Integer = -1);
begin
  AddColumn(ctDBField, AFieldName, ACaption, AWidth);
end;

procedure TCustomVirtualDBGrid.AddCalcColumn(const IDText: string;
  AWidth: Integer);
begin
  AddColumn(ctCalculated, '', IDText, AWidth);
end;

procedure TCustomVirtualDBGrid.AddIndicatorColumn(AWidth: Integer);
begin
  AddColumn(ctIndicator, '', '', AWidth);
end;

function TCustomVirtualDBGrid.GetNodeRecordData(Node: PVirtualNode)
  : TRecordData;
var
  Data: PNodeData;
begin
  Data := InternalGetNodeData(Node);
  if Data <> nil then
    Result := Data^.RecordData
  else
    Result := nil;
end;

procedure TCustomVirtualDBGrid.SetSortColumn(const ColumnTitle: string;
  Direction: TSortDirection);

var
  I : Integer;
begin
  for I := 0 to Header.Columns.Count - 1 do
  begin
    if (UpperCase(Header.Columns[I].Text) = UpperCase(ColumnTitle)) then
    begin
      DoSortColumn(I, Integer(Direction));
      Break;
    end;
  end;
end;

procedure TCustomVirtualDBGrid.SetDBOptions(const Value: TVTDBOptions);
begin
  FDBOptions.Assign(Value);
end;

function TCustomVirtualDBGrid.Navigate(FromPosition: TNavigateFromPosition;
  Delta: Integer): Boolean;
var
  Node : PVirtualNode;
  Count: Integer;
  Max  : Integer;
begin
  Node := nil;
  case FromPosition of
    nfpBegin:
      Node := GetFirstVisible;

    nfpCurrent:
      Node := FocusedNode;

    nfpEnd:
      Node := GetLastVisible;
  end;

  if (Delta <> 0) then
  begin
    Count := 0;
    Max := Abs(Delta);
    while (Node <> nil) and (Count < Max) do
    begin
      Inc(Count);
      if (Delta > 0)
      then
        Node := GetNextVisibleSibling(Node)
      else
        Node := GetPreviousVisibleSibling(Node);
    end;
  end;

  Result := SetFocusToNode(Node, False);
end;

function TCustomVirtualDBGrid.InternalGetNodeData(Node: PVirtualNode)
  : PNodeData;
begin
  if (Node <> nil) and (Node <> RootNode) then
    Result := PNodeData(PByte(Node) + FInternalDataOffset)
  else
    Result := nil;
end;

function TCustomVirtualDBGrid.IsDataOk(AData: PNodeData): Boolean;
begin
  Result := Assigned(AData) and Assigned(AData.RecordData);
end;

procedure TCustomVirtualDBGrid.InternalInitializeDBTree;
begin
  Clear;
  // Set Nodes count equals to database records count
  RootNodeCount := FRecordCount;
end;

procedure TCustomVirtualDBGrid.UpdateVisibleDBTree(AlwaysUpdate: Boolean;
  UpdateLoadedData: Boolean);
begin
  if not Assigned(LinkedDataSet) or not LinkedDataSet.Active or
    IsDataLoading then
    Exit;
  UpdateDBTree(TopNode, GetFullyVisibleCount, AlwaysUpdate, UpdateLoadedData);
end;

procedure TCustomVirtualDBGrid.ReInitializeDBGrid;
var
  OldOffsetXY: TPoint;
begin
  OldOffsetXY := OffsetXY;

  BeginUpdate;

  // Initialize database tree
  InternalInitializeDBTree;

  // Set back offset X & Y
  OffsetXY := OldOffsetXY;

  // Update database tree
  UpdateVisibleDBTree(True);

  EndUpdate;
  // Set focus
  //Focus must be set outside Begin/EndUpdate calls as a workaround to a VTV bug
  SetFocusToActualRecNo;
end;

procedure TCustomVirtualDBGrid.SetFocusToActualRecNo;
var
  WRecNo: Integer;
begin
  if not(csLoading in ComponentState) and not IsDataLoading then
  begin
    WRecNo := GetCurrentDBRecNo;
    if WRecNo <> 0 then
      SetFocusToNode(FindNodeByRecNo(WRecNo), False);
  end;
end;

procedure TCustomVirtualDBGrid.UpdateCurrentRecord;
var
  Node: PVirtualNode;
begin
  // Assume LinkedDataset <> nil
  Node := FindNodeByRecNo(LinkedDataSet.RecNo);
  if Node <> nil then
  begin
    LoadRecordData(Node, True);
    InvalidateNode(Node);
  end;
end;

procedure TCustomVirtualDBGrid.UpdateAllRecords(UpdateLoadedData
  : Boolean = True);
begin
  if not Assigned(LinkedDataSet) or not LinkedDataSet.Active or
    IsDataLoading then
    Exit;
  BeginUpdate;
  try
    UpdateDBTree(GetFirst, VisibleCount, False, UpdateLoadedData);
  finally
    EndUpdate;
  end;
end;

procedure TCustomVirtualDBGrid.UpdateDBTree(StartNode: PVirtualNode;
  NodeCount: Cardinal;
  AlwaysUpdate: Boolean; UpdateLoadedData: Boolean);

var
  Count: Cardinal;

  OldRecNo,
    DatasetRecNo,
    NodeRecNo,
    MoveCount: Integer;

  Run: PVirtualNode;

  DoLoad, DoCreateData : Boolean;
begin
  //it's up to the caller check for LinkedDataset and IsDataLoading
  Run := StartNode;
  if not Assigned(Run) then
    Exit;

  IncLoadingDataFlag;
  try
    LinkedDataSet.CheckBrowseMode;
    LinkedDataSet.DisableControls;
    DatasetRecNo := LinkedDataSet.RecNo;
    OldRecNo := DatasetRecNo;

    DoLoad := AlwaysUpdate;
    DoCreateData := AlwaysUpdate;
    Count := 0;
    while Assigned(Run) and (Count <= NodeCount) do
    begin
      // Initialize node to ensure RecNo is set
      if not(vsInitialized in Run^.States) then
        InitNode(Run);
      NodeRecNo := InternalGetNodeData(Run)^.RecNo;
      // If we dont want always update data, then we must test that
      // if node has data created, and if not than we can load data from database
      // to node's data
      if not AlwaysUpdate then
      begin
        DoCreateData := not IsDataCreated(Run);
        DoLoad := DoCreateData or UpdateLoadedData;
      end;

      if DoLoad then
      begin
        //Move to the RecNo pointed by the run node
        MoveCount := LinkedDataSet.MoveBy(NodeRecNo - DatasetRecNo);
        //Update the DatasetRecNo
        Inc(DatasetRecNo, MoveCount);

        LoadRecordData(Run, DoCreateData); // load data from database
      end;

      Inc(Count);
      Run := GetNextSibling(Run);
    end;
    if (LinkedDataSet.RecNo <> OldRecNo) then
      GotoRecNo(OldRecNo);
  finally
    LinkedDataSet.EnableControls;
    DecLoadingDataFlag;
  end;
end;

function TCustomVirtualDBGrid.IsDataCreated(ANode: PVirtualNode): Boolean;
var
  Data: PNodeData;
begin
  Data := InternalGetNodeData(ANode);
  Result := IsDataOk(Data);
end;

procedure TCustomVirtualDBGrid.LoadRecordData(ANode: PVirtualNode;
  CreateData: Boolean);
var
  I, Idx, ColIdx: Integer;
  WFieldName    : string;
  WIDText       : string;
  WField        : TField;
  WFieldValue   : Variant;
  WCalcValue    : string;
  WCalcType     : TFieldType;
  ColType       : TColumnType;
  Data          : PNodeData;
  RecordNo      : Integer;

  CalculatedColumns: TIntegerList;
begin
  Data := InternalGetNodeData(ANode);
  if Data = nil then
    Exit;
  if Data^.RecordData = nil then
  begin
    // If AlwaysUpdate is False then we dont want to reload existing values from database
    if not CreateData then
      Exit;
    // If RecordData is nil then create it
    Data.RecordData := GetRecordDataClass.Create;
    // necessary to avoid memory leaks when scrolling to fast
    if not(vsInitialized in ANode^.States) then
      InitNode(ANode);
  end;

  // CalculatedColumns to archive calculated column indexes. Load on demand.
  CalculatedColumns := nil;
  try
    RecordNo := LinkedDataSet.RecNo;
    // If current record number is other than -1 then setup RecordData.RecNo
    if (RecordNo <> -1) then
      Data.RecordData.RecNo := RecordNo;

    // Cycle for columns and load data from database and store to Node data
    for I := 0 to Header.Columns.Count - 1 do
    begin
      ColType := TVirtualDBTreeColumn(Header.Columns[I]).ColumnType;

      case ColType of
        ctDBField:
          begin
            WFieldName := TVirtualDBTreeColumn(Header.Columns[I]).FieldName;
            if RecordNo <> -1 then
              WField := LinkedDataSet.FindField(WFieldName)
            else
              WField := nil;

            Idx := Data.RecordData.IndexOf(WFieldName, ffDBField);

            if WField <> nil then
            begin
              WFieldValue := WField.Value;
              DoFormatFieldValue(Header.Columns[I].Index, Data.RecordData,
                ANode.Index, WField, WFieldValue);

              if Idx = -1 then
                Data.RecordData.Add(WFieldName, WFieldValue, WField.DataType,
                  ffDBField)
              else
              begin
                if Idx <> I then
                  Data.RecordData.Exchange(Idx, I);
                Data.RecordData.Edit(WFieldName, ffDBField, WFieldValue,
                  WField.DataType);
              end;
            end
            else
            begin // if field doesnt exists than add empty values
              if Idx = -1 then
                Data.RecordData.Add(WFieldName, Null, ftUnknown, ffDBField)
              else
              begin
                if Idx <> I then
                  Data.RecordData.Exchange(Idx, I);
                Data.RecordData.Edit(WFieldName, ffDBField, Null);
              end;
            end;
          end;

        ctCalculated:
          begin
            if CalculatedColumns = nil then
              CalculatedColumns := TIntegerList.Create;
            CalculatedColumns.Add(I);
          end;

        ctIndicator:
          begin
            Data.RecordData.Add('', '', ftUnknown, ffIndicator);
          end;
      end;
    end;

    // fillup calculated columns
    if CalculatedColumns <> nil then
    begin
      for I := 0 to CalculatedColumns.Count - 1 do
      begin
        ColIdx := CalculatedColumns[I];
        WCalcType := ftString;
        WIDText := TVirtualDBTreeColumn(Header.Columns[ColIdx]).Text;
        Idx := Data.RecordData.IndexOf(WIDText, ffCalculated);

        WCalcValue := '';
        DoCalculateValue(Header.Columns[ColIdx].Text, Header.Columns[ColIdx].
          Index,
          Data.RecordData, ANode.Index, WCalcValue, WCalcType);
        if (Idx = -1)
        then
          Data.RecordData.Insert(ColIdx, WIDText, WCalcValue, WCalcType,
            ffCalculated)
        else
        begin
          if (Idx <> ColIdx) then
            Data.RecordData.Exchange(Idx, ColIdx);
          Data.RecordData.Edit(WIDText, ffCalculated, WCalcValue);
        end;
      end;
    end;
    // Fireup OnLoadRecord event
    DoLoadRecord(Data.RecordData, ANode.Index);
  finally
    CalculatedColumns.Free;
  end;
end;

procedure TCustomVirtualDBGrid.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateVisibleDBTree(False);
end;

function TCustomVirtualDBGrid.GetFullyVisibleCount: Cardinal;
var
  Node                   : PVirtualNode;
  AHeight, AControlHeight: Integer;
begin
  //todo: see why this is called twice at startup
  Result := 0;
  AControlHeight := ClientHeight;
  //TopNode
  Node := GetNodeAt(0, 0, True, AHeight);
  while Node <> nil do
  begin
    Inc(AHeight, Node.NodeHeight);
    if AHeight < AControlHeight then
      Inc(Result)
    else
      Break;
    Node := GetNextVisibleSibling(Node);
  end;
end;

procedure TCustomVirtualDBGrid.DoSortColumn(AColumn: TColumnIndex;
  ASortDirection: Integer = -1);
var
  sDirection       : TSortDirection;
  OldSortDirection : TSortDirection;
  OldCursor        : TCursor;
  OldSortColumn    : TColumnIndex;
  SortBy           : string;
  RefreshGrid      : Boolean;
  ColumnType       : TColumnType;
begin
  OldCursor := Screen.Cursor;
  if (AColumn > NoColumn) and (AColumn < Header.Columns.Count) then
  begin
    ColumnType := TVirtualDBTreeColumn(Header.Columns[AColumn]).ColumnType;

    if (ColumnType <> ctIndicator) and
      (
      ((ColumnType = ctDBField) and (aoSortDBFieldColumns
      in DBOptions.AdvOptions))
      or
      ((ColumnType = ctCalculated) and (aoSortCalculatedColumns
      in DBOptions.AdvOptions))
      )
    then
    begin
      OldSortDirection := Header.SortDirection;
      OldSortColumn := Header.SortColumn;
      sDirection := sdAscending;
      // if some sort direction was set than get True sortdirection type
      if (ASortDirection > -1) then
      begin
        if (ASortDirection >= Byte(sdAscending)) and
          (ASortDirection <= Byte(sdDescending))
        then
          sDirection := TSortDirection(ASortDirection);
      end;

      if (AColumn = Header.SortColumn) then
      begin
            // Do only if autodetect sortdirection was enabled(setting value -1 to ASortDirection)
        if (ASortDirection = -1) then
          if Header.SortDirection = sdAscending then
            sDirection := sdDescending
          else
            sDirection := sdAscending;
      end
      else
        Header.SortColumn := AColumn;

      // If current sort options are the same as the following one then Exit
      if (OldSortDirection = sDirection) and (OldSortColumn = AColumn) then
        Exit;

      try
         // Set HourGlass cursor if we want that
        if (aoHourGlassCursor in DBOptions.AdvOptions)
        then
        begin
          Screen.Cursor := crHourGlass;
        end;

         // Sort the tree
        Header.SortDirection := sDirection;

        if (DBOptions.SortingType = stBuildIn) or (ColumnType = ctCalculated)
        // Buildin sorting
        then
        begin
         // Loads all record if there wasn't sorted the tree yet
         // We must have all data to use sorting, that it can be very slow
         // to load all data from database. Than it is recommended to do not use
         // autosort feature. Instead of this, sort the database by your way, and
         // then tell VirtualDBGrid to reload data(func. ReInitializeDBGrid) to see changes...
          UpdateAllRecords(False);
          SortTree(Header.SortColumn, Header.SortDirection, False);
        end
        else
        begin // User sorting
          RefreshGrid := True;
          SortBy := '';
          case ColumnType of
            ctDBField:
              SortBy := TVirtualDBTreeColumn(Header.Columns[AColumn]).FieldName;

            ctCalculated:
              SortBy := TVirtualDBTreeColumn(Header.Columns[AColumn]).Text;
          end;

          DoCustomSort(AColumn, ColumnType, SortBy, sDirection, RefreshGrid);

          if (RefreshGrid) then
            ReInitializeDBGrid;
        end;

      finally
         // Set back old cursor
        if (aoHourGlassCursor in DBOptions.AdvOptions)
        then
          Screen.Cursor := OldCursor;
      end;
    end;
  end;
end;

function TCustomVirtualDBGrid.GetRecordDataClass: TRecordDataClass;
begin
  Result := TRecordData;
end;

function TCustomVirtualDBGrid.GetCurrentDBRecNo: Integer;
begin
  if Assigned(LinkedDataSet) and LinkedDataSet.Active then
    Result := LinkedDataSet.RecNo
  else
    Result := 0;
end;

end.
