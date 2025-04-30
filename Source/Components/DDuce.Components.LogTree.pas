{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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
  The Original Code is VirtualLogTree.pas. The Initial Developer of the Original
  Code is Paul Thornton. Portions created by the Initial Developer are
  Copyright (C), All Rights Reserved.
}

{$I DDuce.inc}

unit DDuce.Components.LogTree;

interface

uses
  WinApi.Windows,
  System.Classes, System.SysUtils, System.Types,
  Vcl.Graphics, Vcl.ImgList, Vcl.Menus,

  VirtualTrees, VirtualTrees.BaseTree, VirtualTrees.Types,

  DDuce.Components.LogTree.LogItem;

const
  DEFAULT_DATETIMEFORMAT = 'dd-mm-yyyy hh:nn:ss.zzz';

type
  TOnLogEvent = procedure(
    Sender           : TObject;
    var ALogText     : string;
    var ACancelEntry : Boolean;
    ALogLevel        : TLogLevel
  ) of object;
  TOnPopupMenuItemClickEvent = procedure(
    Sender    : TObject;
    AMenuItem : TMenuItem
  ) of object;


  TLogTree = class(TVirtualStringTree)
  private
    FOnBeforeLog             : TOnLogEvent;
    FOnAfterLog              : TNotifyEvent;
    FAutoScroll              : Boolean;
    FAutoLogLevelColors      : Boolean;
    FShowDateColumn          : Boolean;
    FShowImages              : Boolean;
    FMaximumLines            : Cardinal;
    FDateTimeFormat          : string;

    function GetCellText(
      const Node   : PVirtualNode;
      const Column : TColumnIndex
    ) : string;
    procedure SetShowDateColumn(const Value : Boolean);
    procedure SetShowImages(const Value : Boolean);
    procedure AddDefaultColumns(
      const ColumnNames  : array of string;
      const ColumnWidths : array of Integer
    );
    function IfThen(
      Condition : Boolean;
      TrueResult,
      FalseResult : Variant
    ) : Variant;
    function GetDateTimeFormat : string;
    procedure SetDateTimeFormat(const Value : string);

  protected
    procedure DoOnBeforeLog(
      var ALogText     : string;
      var ACancelEntry : Boolean;
      ALogLevel        : TLogLevel
    ); virtual;
    procedure DoOnAfterLog; virtual;
    procedure DoAfterCellPaint(
      ACanvas  : TCanvas;
      ANode    : PVirtualNode;
      Column   : TColumnIndex;
      CellRect : TRect
    ); override;
    procedure DoFreeNode(Node : PVirtualNode); override;
    function DoGetImageIndex(
      Node        : PVirtualNode;
      Kind        : TVTImageKind;
      Column      : TColumnIndex;
      var Ghosted : Boolean;
      var Index   : TImageIndex
    ): TCustomImageList; override;
    procedure DoPaintText(
      Node         : PVirtualNode;
      const Canvas : TCanvas;
      Column       : TColumnIndex;
      TextType     : TVSTTextType
    ); override;
    procedure Loaded; override;
    procedure DoMeasureItem(
      TargetCanvas   : TCanvas;
      Node           : PVirtualNode;
      var NodeHeight : Integer
    ); override;
    procedure DoInitNode(
      Parent         : PVirtualNode;
      Node           : PVirtualNode;
      var InitStates : TVirtualNodeInitStates
    ); override;

  public
    constructor Create(AOwner: TComponent); override;

    procedure Log(
      AValue     : string;
      ALogLevel  : TLogLevel = llInfo;
      ATimestamp : TDateTime = 0
    );
    procedure LogFmt(
      AValue      : string;
      const AArgs : array of const;
      ALogLevel   : TLogLevel = llInfo;
      ATimestamp  : TDateTime = 0
    );
    procedure Init;

  published
    property OnBeforeLog: TOnLogEvent
      read FOnBeforeLog write FOnBeforeLog;

    property OnAfterLog: TNotifyEvent
      read FOnAfterLog write FOnAfterLog;

    property AutoScroll: Boolean
      read FAutoScroll write FAutoScroll;

    property ShowDateColumn: Boolean
      read FShowDateColumn write SetShowDateColumn;

    property ShowImages: Boolean
      read FShowImages write SetShowImages;

    property MaximumLines: Cardinal
      read FMaximumLines write FMaximumLines;

    property DateTimeFormat: string
      read GetDateTimeFormat write SetDateTimeFormat;
  end;

implementation

uses
  System.UITypes,
  Vcl.Dialogs, Vcl.Clipbrd,

  VirtualTrees.Header,

  DDuce.Utils;

resourcestring
  SSaveLog         = '&Save';
  SCopyToClipboard = '&Copy';
  SClear           = 'Clea&r';
  STextFilesTxt    = 'Text files (*.txt)|*.txt|All files (*.*)|*.*';
  SSave            = 'Save';
  SDate            = 'Date';
  SLog             = 'Log';

constructor TLogTree.Create(AOwner: TComponent);
begin
  inherited;
  FDateTimeFormat          := DEFAULT_DATETIMEFORMAT;
  FAutoScroll              := True;
  FShowDateColumn          := True;
  FShowImages              := True;
  Loaded;
end;

procedure TLogTree.DoAfterCellPaint(ACanvas: TCanvas; ANode: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);
var
  ColWidth: Integer;
begin
  inherited;

  if Column = 1 then
  begin
    ColWidth := ACanvas.TextWidth(GetCellText(ANode, Column));

    if not FShowDateColumn then
      ColWidth := ColWidth + 32; // Width of image

    if ColWidth > Header.Columns[1].MinWidth then
      Header.Columns[1].MinWidth := ColWidth;
  end;
end;

procedure TLogTree.DoFreeNode(Node: PVirtualNode);
begin
  inherited;

//  NodeData := GetNodeData(Node);
//
//  if Assigned(NodeData) then
//    NodeData.LogText := '';
end;

function TLogTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean;
  var Index: TImageIndex): TCustomImageList;
//var
//  NodeData: PLogNodeData;
begin
//  if Assigned(Images) then
//  begin
//    if ((FShowImages) and (Kind in [ikNormal, ikSelected])) and
//      (((FShowDateColumn) and (Column <= 0)) or
//      ((not FShowDateColumn) and (Column = 1))) then
//    begin
//      NodeData := GetNodeData(Node);
//
//      if Assigned(NodeData) then
//        case NodeData.LogLevel of
//          llError:
//            Index := 3;
//          llInfo:
//            Index := 2;
//          llWarning:
//            Index := 1;
//          llDebug:
//            Index := 0;
//        else
//          Index := 4;
//        end;
//    end;
//  end;
  Result := inherited DoGetImageIndex(Node, Kind, Column, Ghosted, Index);
end;

procedure TLogTree.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
begin
  inherited;
  Include(InitStates, ivsMultiline);
end;

procedure TLogTree.DoMeasureItem(TargetCanvas: TCanvas;
  Node: PVirtualNode; var NodeHeight: Integer);
var
  I  : Integer;
  H  : Integer;
begin
  inherited;
  if MultiLine[Node] then
  begin
    TargetCanvas.Font := Font;
    NodeHeight := DefaultNodeHeight;
    for I := 0 to Header.Columns.Count - 1 do
    begin
      H := ComputeNodeHeight(TargetCanvas, Node, I);
      if H > NodeHeight then
        NodeHeight := H;
    end;
    if NodeHeight > DefaultNodeHeight then
      NodeHeight := NodeHeight + 4; // needed to avoid multiline text drawing issues
  end;
end;

procedure TLogTree.DoOnAfterLog;
begin
  if Assigned(FOnAfterLog) then
    FOnAfterLog(Self);
end;

procedure TLogTree.DoOnBeforeLog(var ALogText: string; var
  ACancelEntry: Boolean; ALogLevel: TLogLevel);
begin
  if Assigned(FOnAfterLog) then
    FOnBeforeLog(Self, ALogText, ACancelEntry, ALogLevel);
end;

procedure TLogTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas;
  Column: TColumnIndex; TextType: TVSTTextType);
begin
  inherited;
  Canvas.Font.Color := clBlack;
end;

function TLogTree.GetCellText(const Node: PVirtualNode; const
  Column: TColumnIndex): string;
//var
//  NodeData: PLogNodeData;
begin
//  NodeData := GetNodeData(Node);
//
//  if Assigned(NodeData) then
//    case Column of
//      - 1, 0:
//        Result := FormatDateTime(DateTimeFormat, NodeData.Timestamp);
//      1:
//        Result := NodeData.LogText;
//    end;
end;

function TLogTree.GetDateTimeFormat: string;
begin
  Result := FDateTimeFormat;
end;

procedure TLogTree.AddDefaultColumns(
  const ColumnNames: array of string; const ColumnWidths: array of Integer);
var
  I     : Integer;
  Column: TVirtualTreeColumn;
begin
  Header.Columns.Clear;

  if High(ColumnNames) <> High(ColumnWidths) then
    raise Exception.Create
      ('Number of column names must match the number of column widths.')
  else
  begin
    for I := Low(ColumnNames) to High(ColumnNames) do
    begin
      Column := Header.Columns.Add;
      Column.Text := ColumnNames[I];
      if ColumnWidths[I] > 0 then
        Column.Width := ColumnWidths[I]
      else
      begin
        Header.AutoSizeIndex := Column.Index;
        Header.Options := Header.Options + [hoAutoResize];
      end;
    end;
  end;
end;

procedure TLogTree.Loaded;
begin
  inherited Loaded;

  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowRoot,
    toShowTreeLines, toShowButtons] + [toUseBlendedSelection,
    toShowHorzGridLines, toHideFocusRect];
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions +
    [toFullRowSelect, toRightClickSelect];

  AddDefaultColumns([SDate, SLog], [80, 100]);
  Header.AutoSizeIndex := 1;
  Header.Columns[1].MinWidth := 80;
  Header.Options := Header.Options + [hoAutoResize];

  SetShowDateColumn(FShowDateColumn);
end;

function TLogTree.IfThen(Condition: Boolean; TrueResult,
  FalseResult: Variant): Variant;
begin
  if Condition then
    Result := TrueResult
  else
    Result := FalseResult;
end;

procedure TLogTree.Init;
begin
  Loaded;
end;

procedure TLogTree.Log(AValue: string; ALogLevel: TLogLevel;
  ATimestamp: TDateTime);
//var
//  ACancelEntry: Boolean;
//  Node       : PVirtualNode;
  //NodeData   : PLogNodeData;
  //DoScroll   : Boolean;
begin
//  ACancelEntry := False;
//  DoOnBeforeLog(AValue, ACancelEntry, ALogLevel);
//  if not ACancelEntry then
//  begin
//    DoScroll := ((not Focused) or (GetLast = FocusedNode)) and FAutoScroll;
//    Node := AddChild(nil);
//    NodeData := GetNodeData(Node);
//
//    if Assigned(NodeData) then
//    begin
//      NodeData.LogLevel := ALogLevel;
//
//      if ATimestamp = 0 then
//        NodeData.Timestamp := now
//      else
//        NodeData.Timestamp := ATimestamp;
//
//      if FRemoveControlCharacters then
//        AValue := RemoveCtrlChars(AValue);
//
//      if FAutoLogLevelColors then
//      begin
//        case ALogLevel of
//          llError:
//            AValue := Concat('<fc=clRed>', AValue, '</fc>');
//          llInfo:
//            AValue := Concat('<fc=clBlue>', AValue, '</fc>');
//          llWarning:
//            AValue := Concat('<fc=$000067CE>', AValue, '</fc>');
//          llDebug:
//            AValue := Concat('<fc=clGreen>', AValue, '</fc>')
//        end;
//      end;
//
//      NodeData.LogText := AValue;
//      IsVisible[Node] := NodeData.LogLevel in FLogLevels;
//      DoOnAfterLog;
//    end;
//
//    if FMaximumLines <> 0 then
//    begin
//      while RootNodeCount > FMaximumLines do
//        DeleteNode(GetFirst);
//    end;
//
//    if DoScroll then
//    begin
//      ScrollIntoView(GetLast, False);
//    end;
//  end;
end;

procedure TLogTree.LogFmt(AValue: string; const AArgs: Array of
  const; ALogLevel: TLogLevel; ATimestamp: TDateTime);
begin
  Log(Format(AValue, AArgs), ALogLevel, ATimestamp);
end;

procedure TLogTree.SetDateTimeFormat(const Value: string);
begin
  if Value <> DateTimeFormat then
  begin
    FDateTimeFormat := Value;
  end;
end;

procedure TLogTree.SetShowDateColumn(const Value: Boolean);
begin
  FShowDateColumn := Value;

  if Header.Columns.Count > 0 then
  begin
    if FShowDateColumn then
      Header.Columns[0].Options := Header.Columns[0].Options + [coVisible]
    else
      Header.Columns[0].Options := Header.Columns[0].Options - [coVisible]
  end;
end;

procedure TLogTree.SetShowImages(const Value: Boolean);
begin
  FShowImages := Value;
  Invalidate;
end;

end.
