{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Demo.DDuce.DBGridView;

{ Form demonstrating the TDBGridView component. }

interface

uses
  Winapi.Windows,
  System.SysUtils, System.Classes, System.Actions, System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ComCtrls, Vcl.ImgList,
  Vcl.StdCtrls, Vcl.ActnList, Vcl.ExtCtrls, Vcl.CheckLst,
  Data.DB,

  DDuce.Components.GridView, DDuce.Components.DBGridView,
  DDuce.Components.LogTree;

type
  TfrmDBGridView = class(TForm)
    {$REGION 'designer controls'}
    aclMain                  : TActionList;
    btnAutoSizeDisplayWidths : TButton;
    btnInspectComponent      : TButton;
    btnClearLog              : TButton;
    chkActive                : TCheckBox;
    chkMultiselect           : TCheckBox;
    dscMain                  : TDataSource;
    lbxDataSourceEvents      : TCheckListBox;
    lbxDBGridViewEvents      : TCheckListBox;
    pgcMain                  : TPageControl;
    pnlDataSourceEvents      : TPanel;
    pnlDBGridViewEvents      : TPanel;
    pnlHeader                : TPanel;
    pnlLog                   : TPanel;
    splMain                  : TSplitter;
    tsDataSourceEvents       : TTabSheet;
    tsDBGridView             : TTabSheet;
    tsDBGridViewEvents       : TTabSheet;
    imlMain                  : TImageList;
    actInspectComponent      : TAction;
    actAutoSizeColumns       : TAction;
    actClearLog              : TAction;
    chkConnectEvents         : TCheckBox;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actAutoSizeColumnsExecute(Sender: TObject);
    procedure actClearLogExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure chkActiveClick(Sender: TObject);
    procedure chkConnectEventsClick(Sender: TObject);
    procedure chkMultiselectClick(Sender: TObject);
    procedure dscMainDataChange(Sender: TObject; Field: TField);
    procedure dscMainStateChange(Sender: TObject);
    procedure dscMainUpdateData(Sender: TObject);
    procedure grdDBGVDataActiveChanged(Sender: TObject);
    procedure grdDBGVDataChanged(Sender: TObject);
    {$ENDREGION}

  private
    FDBGV : TDBGridView;
    FVLT  : TLogTree;

    function GetDataSet: TDataSet;

    {$REGION 'event handlers'}
    procedure grdDBGVCellAcceptCursor(Sender: TObject; Cell: TGridCell;
      var Accept: Boolean);
    procedure grdDBGVCellClick(Sender: TObject; Cell: TGridCell;
      Shift: TShiftState; X, Y: Integer);
    procedure grdDBGVCellTips(Sender: TObject; Cell: TGridCell;
      var AllowTips: Boolean);
    procedure grdDBGVChange(Sender: TObject; Cell: TGridCell;
      Selected: Boolean);
    procedure grdDBGVChangeColumns(Sender: TObject);
    procedure grdDBGVChangeEditing(Sender: TObject);
    procedure grdDBGVChangeEditMode(Sender: TObject);
    procedure grdDBGVChangeFixed(Sender: TObject);
    procedure grdDBGVChangeRows(Sender: TObject);
    procedure grdDBGVChanging(Sender: TObject; var Cell: TGridCell;
      var Selected: Boolean);
    procedure grdDBGVCheckClick(Sender: TObject; Cell: TGridCell);
    procedure grdDBGVClick(Sender: TObject);
    procedure grdDBGVColumnAutoSize(Sender: TObject; Column: Integer;
      var Width: Integer);
    procedure grdDBGVColumnResize(Sender: TObject; Column: Integer;
      var Width: Integer);
    procedure grdDBGVColumnResizing(Sender: TObject; Column: Integer;
      var Width: Integer);
    procedure grdDBGVDataDeleteRecord(Sender: TObject;
      var AllowDelete: Boolean);
    procedure grdDBGVDataEditError(Sender: TObject; E: Exception;
      var Action: TDBGridDataAction);
    procedure grdDBGVDataInsertRecord(Sender: TObject;
      var AllowInsert: Boolean);
    procedure grdDBGVDataUpdateError(Sender: TObject; E: Exception;
      var Action: TDBGridDataAction);
    procedure grdDBGVDataUpdateField(Sender: TObject; Field: TField);
    procedure grdDBGVDblClick(Sender: TObject);
    procedure grdDBGVDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure grdDBGVDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure grdDBGVDraw(Sender: TObject; var DefaultDrawing: Boolean);
    procedure grdDBGVDrawCell(Sender: TObject; Cell: TGridCell; var Rect: TRect;
      var DefaultDrawing: Boolean);
    procedure grdDBGVDrawHeader(Sender: TObject; Section: TGridHeaderSection;
      Rect: TRect; var DefaultDrawing: Boolean);
    procedure grdDBGVEditAcceptKey(Sender: TObject; Cell: TGridCell; Key: Char;
      var Accept: Boolean);
    procedure grdDBGVEditButtonPress(Sender: TObject; Cell: TGridCell);
    procedure grdDBGVEditCanceled(Sender: TObject; Cell: TGridCell);
    procedure grdDBGVEditCanModify(Sender: TObject; Cell: TGridCell;
      var CanModify: Boolean);
    procedure grdDBGVEditChange(Sender: TObject; Cell: TGridCell);
    procedure grdDBGVEditCloseUp(Sender: TObject; Cell: TGridCell;
      ItemIndex: Integer; var Accept: Boolean);
    procedure grdDBGVEditSelectNext(Sender: TObject; Cell: TGridCell;
      var Value: string);
    procedure grdDBGVEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure grdDBGVEnter(Sender: TObject);
    procedure grdDBGVExit(Sender: TObject);
    procedure grdDBGVGetCellColors(Sender: TObject; Cell: TGridCell;
      Canvas: TCanvas);
    procedure grdDBGVGetCellHintRect(Sender: TObject; Cell: TGridCell;
      var Rect: TRect);
    procedure grdDBGVGetCellImage(Sender: TObject; Cell: TGridCell;
      var ImageIndex: Integer);
    procedure grdDBGVGetCellImageIndent(Sender: TObject; Cell: TGridCell;
      var Indent: TPoint);
    procedure grdDBGVGetCellReadOnly(Sender: TObject; Cell: TGridCell;
      var CellReadOnly: Boolean);
    procedure grdDBGVGetCellText(Sender: TObject; Cell: TGridCell;
      var Value: string);
    procedure grdDBGVGetCellTextIndent(Sender: TObject; Cell: TGridCell;
      var Indent: TPoint);
    procedure grdDBGVGetCheckAlignment(Sender: TObject; Cell: TGridCell;
      var CheckAlignment: TAlignment);
    procedure grdDBGVGetCheckImage(Sender: TObject; Cell: TGridCell;
      CheckImage: TBitmap);
    procedure grdDBGVGetCheckIndent(Sender: TObject; Cell: TGridCell;
      var Indent: TPoint);
    procedure grdDBGVGetCheckKind(Sender: TObject; Cell: TGridCell;
      var CheckKind: TGridCheckKind);
    procedure grdDBGVGetCheckState(Sender: TObject; Cell: TGridCell;
      var CheckState: TCheckBoxState);
    procedure grdDBGVGetEditList(Sender: TObject; Cell: TGridCell;
      Items: TStrings);
    procedure grdDBGVGetEditListBounds(Sender: TObject; Cell: TGridCell;
      var Rect: TRect);
    procedure grdDBGVGetEditMask(Sender: TObject; Cell: TGridCell;
      var Value: string);
    procedure grdDBGVGetEditStyle(Sender: TObject; Cell: TGridCell;
      var Style: TGridEditStyle);
    procedure grdDBGVGetEditText(Sender: TObject; Cell: TGridCell;
      var Value: string);
    procedure grdDBGVGetHeaderColors(Sender: TObject;
      Section: TGridHeaderSection; Canvas: TCanvas);
    procedure grdDBGVGetHeaderImage(Sender: TObject;
      Section: TGridHeaderSection; var ImageIndex: Integer);
    procedure grdDBGVGetIndicatorImage(Sender: TObject; DataRow: Integer;
      var ImageIndex: Integer);
    procedure grdDBGVGetSortDirection(Sender: TObject;
      Section: TGridHeaderSection; var SortDirection: TGridSortDirection);
    procedure grdDBGVGetSortImage(Sender: TObject; Section: TGridHeaderSection;
      SortImage: TBitmap);
    procedure grdDBGVGetTipsRect(Sender: TObject; Cell: TGridCell;
      var Rect: TRect);
    procedure grdDBGVGetTipsText(Sender: TObject; Cell: TGridCell;
      var Value: string);
    procedure grdDBGVHeaderClick(Sender: TObject; Section: TGridHeaderSection);
    procedure grdDBGVHeaderClicking(Sender: TObject;
      Section: TGridHeaderSection; var AllowClick: Boolean);
    procedure grdDBGVKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure grdDBGVKeyPress(Sender: TObject; var Key: Char);
    procedure grdDBGVKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure grdDBGVMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure grdDBGVMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure grdDBGVMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure grdDBGVMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure grdDBGVMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure grdDBGVSetEditText(Sender: TObject; Cell: TGridCell;
      var Value: string);
    procedure grdDBGVStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure grdDBGVResize(Sender: TObject);
    procedure grdDBGVEditCanShow(Sender: TObject; Cell: TGridCell;
      var CanShow: Boolean);
    procedure grdDBGVDataLayoutChanged(Sender: TObject);
    procedure grdDBGVClearMultiSelect(Sender: TObject);
    procedure grdDBGVRowMultiSelect(Sender: TObject; Row: Integer;
      var Select: Boolean);
    {$ENDREGION}

    procedure ConnectEvents;
    procedure DisconnectEvents;

  protected
    procedure AddToLog(
      const AString : string;
      AColor        : TColor = clBlack;
      AObject       : TObject = nil
      ); overload;
    procedure AddToLog(
      const AString : string;
      const AInfo   : string;
      AColor        : TColor = clBlack;
      AObject       : TObject = nil
      ); overload;
    function IsChecked(
      const AName : string;
      AListBox    : TCheckListBox
      ) : Boolean;
    procedure CreateDBGridView;

  public
    procedure AfterConstruction; override;

    property DataSet: TDataSet
      read GetDataSet;

  end;

implementation

{$R *.dfm}

uses
  System.TypInfo,

  DDuce.Factories.GridView,

  Demo.Data, Demo.Factories;

var
  ProcByLevel: string;

{$REGION 'construction and destruction'}
procedure TfrmDBGridView.AfterConstruction;
begin
  inherited AfterConstruction;
  dscMain.DataSet := TDemoFactories.CreateContactDataSet(Self, 10000);
  CreateDBGridView;

  FVLT := TLogTree.Create(Self);
  FVLT.Parent := pnlLog;
  FVLT.Align := alClient;
  FVLT.AlignWithMargins := True;
  FVLT.HTMLSupport := True;
  FVLT.AutoLogLevelColors := True;
  FVLT.Init;
  FVLT.Images := imlMain;
  FVLT.Log('Application started', llDebug);
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmDBGridView.actAutoSizeColumnsExecute(Sender: TObject);
begin
//  AutoSizeDisplayWidths(DataSet);
end;

procedure TfrmDBGridView.actClearLogExecute(Sender: TObject);
begin
  FVLT.Clear;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmDBGridView.grdDBGVCellAcceptCursor(Sender: TObject; Cell: TGridCell;
  var Accept: Boolean);
var
  S : string;
begin
  S := Format('(%d, %d, %s)', [Cell.Col, Cell.Row, BoolToStr(Accept)]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVCellClick(Sender: TObject; Cell: TGridCell;
  Shift: TShiftState; X, Y: Integer);
var
  S : string;
begin
  S := Format('(%d, %d)', [Cell.Col, Cell.Row]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVCellTips(Sender: TObject; Cell: TGridCell;
  var AllowTips: Boolean);
var
  S : string;
begin
  S := Format('(%d, %d, %s)', [Cell.Col, Cell.Row, BoolToStr(AllowTips)]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVChange(Sender: TObject; Cell: TGridCell;
  Selected: Boolean);
var
  S : string;
begin
  S := Format('(%d, %d, %s)', [Cell.Col, Cell.Row, BoolToStr(Selected)]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVChangeColumns(Sender: TObject);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVChangeEditing(Sender: TObject);
begin
 AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVChangeEditMode(Sender: TObject);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVChangeFixed(Sender: TObject);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVChangeRows(Sender: TObject);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVChanging(Sender: TObject; var Cell: TGridCell;
  var Selected: Boolean);
var
  S : string;
begin
  S := Format('(%d, %d, %s)', [Cell.Col, Cell.Row, BoolToStr(Selected)]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVCheckClick(Sender: TObject; Cell: TGridCell);
var
  S : string;
begin
  S := Format('(%d, %d)', [Cell.Col, Cell.Row]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVClearMultiSelect(Sender: TObject);
begin
 AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVClick(Sender: TObject);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVColumnAutoSize(Sender: TObject; Column: Integer;
  var Width: Integer);
var
  S : string;
begin
  S := Format('(%d, %d)', [Column, Width]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVColumnResize(Sender: TObject; Column: Integer;
  var Width: Integer);
var
  S : string;
begin
  S := Format('(%d, %d)', [Column, Width]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVColumnResizing(Sender: TObject; Column: Integer;
  var Width: Integer);
var
  S : string;
begin
  S := Format('(%d, %d)', [Column, Width]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVDataActiveChanged(Sender: TObject);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVDataChanged(Sender: TObject);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVDataDeleteRecord(Sender: TObject;
  var AllowDelete: Boolean);
var
  S : string;
begin
  S := Format('(%s)', [BoolToStr(AllowDelete)]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVDataEditError(Sender: TObject; E: Exception;
  var Action: TDBGridDataAction);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVDataInsertRecord(Sender: TObject;
  var AllowInsert: Boolean);
var
  S : string;
begin
  S := Format('(%s)', [BoolToStr(AllowInsert)]);
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVDataLayoutChanged(Sender: TObject);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVDataUpdateError(Sender: TObject; E: Exception;
  var Action: TDBGridDataAction);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVDataUpdateField(Sender: TObject; Field: TField);
var
  S : string;
begin
  if Assigned(Field) then
    S := Format('(%s)', [Field.FieldName]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVDblClick(Sender: TObject);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVDraw(Sender: TObject; var DefaultDrawing: Boolean);
var
  S : string;
begin
  S := Format('(%s)', [BoolToStr(DefaultDrawing)]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVDrawCell(Sender: TObject; Cell: TGridCell;
  var Rect: TRect; var DefaultDrawing: Boolean);
var
  S : string;
begin
  S := Format('(%d, %d, %s)', [Cell.Col, Cell.Row, BoolToStr(DefaultDrawing)]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVDrawHeader(Sender: TObject; Section: TGridHeaderSection;
  Rect: TRect; var DefaultDrawing: Boolean);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVEditAcceptKey(Sender: TObject; Cell: TGridCell;
  Key: Char; var Accept: Boolean);
var
  S : string;
begin
  S := Format('(%d, %d, %s, %s)', [Cell.Col, Cell.Row, Key, BoolToStr(Accept)]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVEditButtonPress(Sender: TObject; Cell: TGridCell);
var
  S : string;
begin
  S := Format('(%d, %d)', [Cell.Col, Cell.Row]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVEditCanceled(Sender: TObject; Cell: TGridCell);
var
  S : string;
begin
  S := Format('(%d, %d)', [Cell.Col, Cell.Row]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVEditCanModify(Sender: TObject; Cell: TGridCell;
  var CanModify: Boolean);
var
  S : string;
begin
  S := Format('(%d, %d, %s)', [Cell.Col, Cell.Row, BoolToStr(CanModify)]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVEditCanShow(Sender: TObject; Cell: TGridCell;
  var CanShow: Boolean);
var
  S : string;
begin
  S := Format('(%d, %d, %s)', [Cell.Col, Cell.Row, BoolToStr(CanShow)]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVEditChange(Sender: TObject; Cell: TGridCell);
var
  S : string;
begin
  S := Format('(%d, %d)', [Cell.Col, Cell.Row]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVEditCloseUp(Sender: TObject; Cell: TGridCell;
  ItemIndex: Integer; var Accept: Boolean);
begin
   AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVEditSelectNext(Sender: TObject; Cell: TGridCell;
  var Value: string);
var
  S : string;
begin
  S := Format('(%d, %d, %s)', [Cell.Col, Cell.Row, Value]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVEnter(Sender: TObject);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVExit(Sender: TObject);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVGetCellColors(Sender: TObject; Cell: TGridCell;
  Canvas: TCanvas);
var
  S : string;
begin
  S := Format('(%d, %d, %s)', [Cell.Col, Cell.Row, '<Canvas>']);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVGetCellHintRect(Sender: TObject; Cell: TGridCell;
  var Rect: TRect);
var
  S : string;
begin
  S := Format('(%d, %d, %s)', [Cell.Col, Cell.Row, '<Rect>']);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVGetCellImage(Sender: TObject; Cell: TGridCell;
  var ImageIndex: Integer);
var
  S : string;
begin
  S := Format('(%d, %d, %d)', [Cell.Col, Cell.Row, ImageIndex]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVGetCellImageIndent(Sender: TObject; Cell: TGridCell;
  var Indent: TPoint);
var
  S : string;
begin
  S := Format('(%d, %d, %d, %d)', [Cell.Col, Cell.Row, Indent.X, Indent.Y]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVGetCellReadOnly(Sender: TObject; Cell: TGridCell;
  var CellReadOnly: Boolean);
var
  S : string;
begin
  S := Format('(%d, %d, %s)', [Cell.Col, Cell.Row, BoolToStr(CellReadOnly)]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVGetCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);
var
  S : string;
begin
  S := Format('(%d, %d, %s)', [Cell.Col, Cell.Row, Value]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVGetCellTextIndent(Sender: TObject; Cell: TGridCell;
  var Indent: TPoint);
var
  S : string;
begin
  S := Format('(%d, %d, %d, %d)', [Cell.Col, Cell.Row, Indent.X, Indent.Y]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVGetCheckAlignment(Sender: TObject; Cell: TGridCell;
  var CheckAlignment: TAlignment);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVGetCheckImage(Sender: TObject; Cell: TGridCell;
  CheckImage: TBitmap);
var
  S : string;
begin
  S := Format('(%d, %d, <CheckImage>)', [Cell.Col, Cell.Row]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVGetCheckIndent(Sender: TObject; Cell: TGridCell;
  var Indent: TPoint);
var
  S : string;
begin
  S := Format('(%d, %d, %d, %d)', [Cell.Col, Cell.Row, Indent.X, Indent.Y]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVGetCheckKind(Sender: TObject; Cell: TGridCell;
  var CheckKind: TGridCheckKind);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVGetCheckState(Sender: TObject; Cell: TGridCell;
  var CheckState: TCheckBoxState);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVGetEditList(Sender: TObject; Cell: TGridCell;
  Items: TStrings);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVGetEditListBounds(Sender: TObject; Cell: TGridCell;
  var Rect: TRect);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVGetEditMask(Sender: TObject; Cell: TGridCell;
  var Value: string);
var
  S : string;
begin
  S := Format('(%d, %d, %s)', [Cell.Col, Cell.Row, Value]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVGetEditStyle(Sender: TObject; Cell: TGridCell;
  var Style: TGridEditStyle);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVGetEditText(Sender: TObject; Cell: TGridCell;
  var Value: string);
var
  S : string;
begin
  S := Format('(%d, %d, %s)', [Cell.Col, Cell.Row, Value]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVGetHeaderColors(Sender: TObject;
  Section: TGridHeaderSection; Canvas: TCanvas);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVGetHeaderImage(Sender: TObject;
  Section: TGridHeaderSection; var ImageIndex: Integer);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVGetIndicatorImage(Sender: TObject; DataRow: Integer;
  var ImageIndex: Integer);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVGetSortDirection(Sender: TObject;
  Section: TGridHeaderSection; var SortDirection: TGridSortDirection);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVGetSortImage(Sender: TObject;
  Section: TGridHeaderSection; SortImage: TBitmap);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVGetTipsRect(Sender: TObject; Cell: TGridCell;
  var Rect: TRect);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVGetTipsText(Sender: TObject; Cell: TGridCell;
  var Value: string);
var
  S : string;
begin
  S := Format('(%d, %d, %s)', [Cell.Col, Cell.Row, Value]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVHeaderClick(Sender: TObject;
  Section: TGridHeaderSection);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVHeaderClicking(Sender: TObject;
  Section: TGridHeaderSection; var AllowClick: Boolean);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVKeyPress(Sender: TObject; var Key: Char);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVResize(Sender: TObject);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVRowMultiSelect(Sender: TObject; Row: Integer;
  var Select: Boolean);
begin
 AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.grdDBGVSetEditText(Sender: TObject; Cell: TGridCell;
  var Value: string);
var
  S : string;
begin
  S := Format('(%d, %d, %s)', [Cell.Col, Cell.Row, Value]);
  AddToLog(ProcByLevel, S);
end;

procedure TfrmDBGridView.grdDBGVStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  AddToLog(ProcByLevel);
end;

procedure TfrmDBGridView.dscMainDataChange(Sender: TObject; Field: TField);
var
  S : string;
begin
  if Assigned(Field) then
    S := S + Format('(%s)', [Field.FieldName]);
  AddToLog(ProcByLevel, S, clBlack, Sender);
end;

procedure TfrmDBGridView.dscMainStateChange(Sender: TObject);
var
  S : string;
begin
  S := ' [' + GetEnumName(TypeInfo(TDataSetState), Integer(DataSet.State)) + ']';
  AddToLog(ProcByLevel, S, clBlack, Sender);
end;

procedure TfrmDBGridView.dscMainUpdateData(Sender: TObject);
begin
  AddToLog(ProcByLevel, clBlack, Sender);
end;

procedure TfrmDBGridView.chkActiveClick(Sender: TObject);
begin
  DataSet.Active := chkActive.Checked;
end;

procedure TfrmDBGridView.chkConnectEventsClick(Sender: TObject);
begin
  if chkConnectEvents.Checked then
    ConnectEvents
  else
    DisconnectEvents;
end;

procedure TfrmDBGridView.chkMultiselectClick(Sender: TObject);
begin
  FDBGV.MultiSelect := (Sender as TCheckBox).Checked;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmDBGridView.GetDataSet: TDataSet;
begin
  Result := dscMain.DataSet;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmDBGridView.ConnectEvents;
begin
  with FDBGV do
  begin
    OnCellAcceptCursor   := grdDBGVCellAcceptCursor;
    OnCellClick          := grdDBGVCellClick;
    OnCellTips           := grdDBGVCellTips;
    OnChange             := grdDBGVChange;
    OnChangeColumns      := grdDBGVChangeColumns;
    OnChangeEditing      := grdDBGVChangeEditing;
    OnChangeEditMode     := grdDBGVChangeEditMode;
    OnChangeFixed        := grdDBGVChangeFixed;
    OnChangeRows         := grdDBGVChangeRows;
    OnChanging           := grdDBGVChanging;
    OnCheckClick         := grdDBGVCheckClick;
    OnClick              := grdDBGVClick;
    OnColumnAutoSize     := grdDBGVColumnAutoSize;
    OnColumnResizing     := grdDBGVColumnResizing;
    OnColumnResize       := grdDBGVColumnResize;
    OnDblClick           := grdDBGVDblClick;
    OnDataActiveChanged  := grdDBGVDataActiveChanged;
    OnDataChanged        := grdDBGVDataChanged;
    OnDataDeleteRecord   := grdDBGVDataDeleteRecord;
    OnDataEditError      := grdDBGVDataEditError;
    OnDataInsertRecord   := grdDBGVDataInsertRecord;
    OnDataUpdateField    := grdDBGVDataUpdateField;
    OnDataLayoutChanged  := grdDBGVDataLayoutChanged;
    OnDataUpdateError    := grdDBGVDataUpdateError;
    OnDragDrop           := grdDBGVDragDrop;
    OnDragOver           := grdDBGVDragOver;
    OnDraw               := grdDBGVDraw;
    OnDrawCell           := grdDBGVDrawCell;
    OnDrawHeader         := grdDBGVDrawHeader;
    OnEditAcceptKey      := grdDBGVEditAcceptKey;
    OnEditButtonPress    := grdDBGVEditButtonPress;
    OnEditCanceled       := grdDBGVEditCanceled;
    OnEditCanModify      := grdDBGVEditCanModify;
    OnEditCanShow        := grdDBGVEditCanShow;
    OnEditChange         := grdDBGVEditChange;
    OnEditCloseUp        := grdDBGVEditCloseUp;
    OnEditSelectNext     := grdDBGVEditSelectNext;
    OnEndDrag            := grdDBGVEndDrag;
    OnEnter              := grdDBGVEnter;
    OnExit               := grdDBGVExit;
    OnGetCellColors      := grdDBGVGetCellColors;
    OnGetCellImage       := grdDBGVGetCellImage;
    OnGetCellImageIndent := grdDBGVGetCellImageIndent;
    OnGetCellHintRect    := grdDBGVGetCellHintRect;
    OnGetCellReadOnly    := grdDBGVGetCellReadOnly;
    OnGetCellText        := grdDBGVGetCellText;
    OnGetCellTextIndent  := grdDBGVGetCellTextIndent;
    OnGetCheckAlignment  := grdDBGVGetCheckAlignment;
    OnGetCheckImage      := grdDBGVGetCheckImage;
    OnGetCheckIndent     := grdDBGVGetCheckIndent;
    OnGetCheckKind       := grdDBGVGetCheckKind;
    OnGetCheckState      := grdDBGVGetCheckState;
    OnGetEditList        := grdDBGVGetEditList;
    OnGetEditListBounds  := grdDBGVGetEditListBounds;
    OnGetEditMask        := grdDBGVGetEditMask;
    OnGetEditStyle       := grdDBGVGetEditStyle;
    OnGetEditText        := grdDBGVGetEditText;
    OnGetHeaderColors    := grdDBGVGetHeaderColors;
    OnGetHeaderImage     := grdDBGVGetHeaderImage;
    OnGetIndicatorImage  := grdDBGVGetIndicatorImage;
    OnGetSortDirection   := grdDBGVGetSortDirection;
    OnGetSortImage       := grdDBGVGetSortImage;
    OnGetTipsRect        := grdDBGVGetTipsRect;
    OnGetTipsText        := grdDBGVGetTipsText;
    OnHeaderClick        := grdDBGVHeaderClick;
    OnHeaderClicking     := grdDBGVHeaderClicking;
    OnMouseDown          := grdDBGVMouseDown;
    OnMouseMove          := grdDBGVMouseMove;
    OnMouseUp            := grdDBGVMouseUp;
    OnMouseWheelDown     := grdDBGVMouseWheelDown;
    OnMouseWheelUp       := grdDBGVMouseWheelUp;
    OnKeyDown            := grdDBGVKeyDown;
    OnKeyPress           := grdDBGVKeyPress;
    OnKeyUp              := grdDBGVKeyUp;
    OnResize             := grdDBGVResize;
    OnSetEditText        := grdDBGVSetEditText;
    OnStartDrag          := grdDBGVStartDrag;
    OnRowMultiSelect     := grdDBGVRowMultiSelect;
    OnClearMultiSelect   := grdDBGVClearMultiSelect;
  end;
end;

procedure TfrmDBGridView.DisconnectEvents;
begin
  with FDBGV do
  begin
    OnCellAcceptCursor   := nil;
    OnCellClick          := nil;
    OnCellTips           := nil;
    OnChange             := nil;
    OnChangeColumns      := nil;
    OnChangeEditing      := nil;
    OnChangeEditMode     := nil;
    OnChangeFixed        := nil;
    OnChangeRows         := nil;
    OnChanging           := nil;
    OnCheckClick         := nil;
    OnClick              := nil;
    OnColumnAutoSize     := nil;
    OnColumnResizing     := nil;
    OnColumnResize       := nil;
    OnDblClick           := nil;
    //OnDataActiveChanged  := nil;
    //OnDataChanged        := nil;
    OnDataDeleteRecord   := nil;
    OnDataEditError      := nil;
    OnDataInsertRecord   := nil;
    OnDataUpdateField    := nil;
    //OnDataLayoutChanged  := nil;
    OnDataUpdateError    := nil;
    OnDragDrop           := nil;
    OnDragOver           := nil;
    OnDraw               := nil;
    OnDrawCell           := nil;
    OnDrawHeader         := nil;
    OnEditAcceptKey      := nil;
    OnEditButtonPress    := nil;
    OnEditCanceled       := nil;
    OnEditCanModify      := nil;
    OnEditCanShow        := nil;
    OnEditChange         := nil;
    OnEditCloseUp        := nil;
    OnEditSelectNext     := nil;
    OnEndDrag            := nil;
    OnEnter              := nil;
    OnExit               := nil;
    OnGetCellColors      := nil;
    OnGetCellImage       := nil;
    OnGetCellImageIndent := nil;
    OnGetCellHintRect    := nil;
    OnGetCellReadOnly    := nil;
    OnGetCellText        := nil;
    OnGetCellTextIndent  := nil;
    OnGetCheckAlignment  := nil;
    OnGetCheckImage      := nil;
    OnGetCheckIndent     := nil;
    OnGetCheckKind       := nil;
    OnGetCheckState      := nil;
    OnGetEditList        := nil;
    OnGetEditListBounds  := nil;
    OnGetEditMask        := nil;
    OnGetEditStyle       := nil;
    OnGetEditText        := nil;
    OnGetHeaderColors    := nil;
    OnGetHeaderImage     := nil;
    OnGetIndicatorImage  := nil;
    OnGetSortDirection   := nil;
    OnGetSortImage       := nil;
    OnGetTipsRect        := nil;
    OnGetTipsText        := nil;
    OnHeaderClick        := nil;
    OnHeaderClicking     := nil;
    OnMouseDown          := nil;
    OnMouseMove          := nil;
    OnMouseUp            := nil;
    OnMouseWheelDown     := nil;
    OnMouseWheelUp       := nil;
    OnKeyDown            := nil;
    OnKeyPress           := nil;
    OnKeyUp              := nil;
    OnResize             := nil;
    OnSetEditText        := nil;
    OnStartDrag          := nil;
    OnRowMultiSelect     := nil;
    OnClearMultiSelect   := nil;
  end;
end;

procedure TfrmDBGridView.CreateDBGridView;
begin
  FDBGV := TGridViewFactory.CreateDBGridView(
    Self,
    tsDBGridView,
    dscMain,
    'grdDBGV'
  );
  with FDBGV do
  begin
    AlignWithMargins  := True;
    CursorKeys        := [gkArrows, gkTabs, gkReturn, gkMouse, gkMouseWheel];
    DoubleBuffered    := True;
    ParentFont        := False;
    ParentShowHint    := False;
    ShowCellTips      := True;
    ShowHint          := True;
    TabOrder          := 0;
    FitColsToClient   := False;
    MultiSelect       := False;
    RowSelect         := True;
    Rows.AutoHeight   := True;
    Header.AutoHeight := True;
  end;
end;

procedure TfrmDBGridView.AddToLog(const AString: string; AColor: TColor; AObject: TObject);
begin
  AddToLog(AString, '', AColor, AObject);
end;

procedure TfrmDBGridView.AddToLog(const AString, AInfo: string; AColor: TColor;
  AObject: TObject);
var
  S : string;
  T : string;
  B : Boolean;
begin
//  T := ExtractMethodName(AString);
//  T := StrAfter('.', T);

  if not Assigned(AObject) then
  begin
//    T := StrAfter('grdDBGV', T);
//    T := StrBefore('$', T);
    T := 'On' + T;
    B := IsChecked(T, lbxDBGridViewEvents)
  end
  else
  begin
//    T := StrAfter('dscMain', T);
    T := 'On' + T;
    B := IsChecked(T, lbxDataSourceEvents);
  end;

  T := T + AInfo;

  if B then
  begin
    S := Format('<font-color=%s><b> %s',
      [ColorToString(AColor), T]);

    FVLT.Log(S);
  end;
end;

function TfrmDBGridView.IsChecked(const AName: string; AListBox : TCheckListBox): Boolean;
var
  B : Boolean;
  I : Integer;
begin
  B := False;
  for I := 0 to AListBox.Count - 1 do
  begin
    B := (AName = AListBox.Items[I]) and AListBox.Checked[I];
    if B then
      Break;
  end;
  Result := B;
end;
{$ENDREGION}

end.
