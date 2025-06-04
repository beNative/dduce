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

{$I DDuce.inc}

unit DDuce.Components.LogTree;

interface

uses
  WinApi.Windows,
  System.Classes, System.SysUtils, System.Types,
  Vcl.Graphics, Vcl.ImgList, Vcl.Menus,

  VirtualTrees, VirtualTrees.BaseTree, VirtualTrees.Types,

  DDuce.Components.VirtualTrees.Node, DDuce.Components.LogTree.LogItem;

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

  TLogNode = TVTNode<TLogItem>;

  TLogTree = class(TVirtualStringTree)
  private
    FOnBeforeLog        : TOnLogEvent;
    FOnAfterLog         : TNotifyEvent;
    FAutoScroll         : Boolean;
    FAutoLogLevelColors : Boolean;
    FShowDateColumn     : Boolean;
    FShowImages         : Boolean;
    FMaximumLines       : Cardinal;
    FDateTimeFormat     : string;

    function GetCellText(
      const ANode   : PVirtualNode;
      const AColumn : TColumnIndex
    ) : string;
    procedure SetShowDateColumn(const Value: Boolean);
    procedure SetShowImages(const Value: Boolean);
    procedure AddDefaultColumns(
      const AColumnNames  : array of string;
      const AColumnWidths : array of Integer
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
      AColumn   : TColumnIndex;
      CellRect : TRect
    ); override;
    procedure DoFreeNode(ANode : PVirtualNode); override;
    function DoGetImageIndex(
      ANode        : PVirtualNode;
      Kind        : TVTImageKind;
      AColumn      : TColumnIndex;
      var Ghosted : Boolean;
      var Index   : TImageIndex
    ): TCustomImageList; override;
    procedure DoPaintText(
      ANode         : PVirtualNode;
      const Canvas : TCanvas;
      AColumn       : TColumnIndex;
      TextType     : TVSTTextType
    ); override;
    procedure Loaded; override;
    procedure DoMeasureItem(
      TargetCanvas   : TCanvas;
      ANode           : PVirtualNode;
      var NodeHeight : Integer
    ); override;
    procedure DoInitNode(
      Parent         : PVirtualNode;
      ANode          : PVirtualNode;
      var InitStates : TVirtualNodeInitStates
    ); override;

  public
    constructor Create(AOwner: TComponent); override;

    procedure Log(
      AValue     : string;
      ALogLevel  : TLogLevel = TLogLevel.Info;
      ATimestamp : TDateTime = 0
    ); overload;
    procedure Log(
      AValue      : string;
      const AArgs : array of const;
      ALogLevel   : TLogLevel = TLogLevel.Info;
      ATimestamp  : TDateTime = 0
    ); overload;
    procedure Init;

    function GetNode(const AVNode: PVirtualNode): TLogNode;

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
  FDateTimeFormat := DEFAULT_DATETIMEFORMAT;
  FAutoScroll     := True;
  FShowDateColumn := True;
  FShowImages     := True;
  Loaded;
end;

procedure TLogTree.DoAfterCellPaint(ACanvas: TCanvas; ANode: PVirtualNode;
  AColumn: TColumnIndex; CellRect: TRect);
var
  LColWidth : Integer;
begin
  inherited;

  if AColumn = 1 then
  begin
    LColWidth := ACanvas.TextWidth(GetCellText(ANode, AColumn));

    if not FShowDateColumn then
      LColWidth := LColWidth + 32; // Width of image

    if LColWidth > Header.Columns[1].MinWidth then
      Header.Columns[1].MinWidth := LColWidth;
  end;
end;

procedure TLogTree.DoFreeNode(ANode: PVirtualNode);
begin
  inherited;

//  NodeData := GetNodeData(Node);
//
//  if Assigned(NodeData) then
//    NodeData.LogText := '';
end;

function TLogTree.DoGetImageIndex(ANode: PVirtualNode; Kind: TVTImageKind;
  AColumn: TColumnIndex; var Ghosted: Boolean;
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
  Result := inherited DoGetImageIndex(ANode, Kind, AColumn, Ghosted, Index);
end;

procedure TLogTree.DoInitNode(Parent, ANode: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
begin
  inherited;
  Include(InitStates, ivsMultiline);
end;

procedure TLogTree.DoMeasureItem(TargetCanvas: TCanvas;
  ANode: PVirtualNode; var NodeHeight: Integer);
var
  I  : Integer;
  H  : Integer;
begin
  inherited;
  if MultiLine[ANode] then
  begin
    TargetCanvas.Font := Font;
    NodeHeight := DefaultNodeHeight;
    for I := 0 to Header.Columns.Count - 1 do
    begin
      H := ComputeNodeHeight(TargetCanvas, ANode, I);
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

procedure TLogTree.DoPaintText(ANode: PVirtualNode; const Canvas: TCanvas;
  AColumn: TColumnIndex; TextType: TVSTTextType);
begin
  inherited;
  Canvas.Font.Color := clBlack;
end;

function TLogTree.GetCellText(const ANode: PVirtualNode; const
  AColumn: TColumnIndex): string;
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

function TLogTree.GetNode(const AVNode: PVirtualNode): TLogNode;
begin
  Result := GetNodeData<TLogNode>(AVNode);
end;

procedure TLogTree.AddDefaultColumns(
  const AColumnNames: array of string; const AColumnWidths: array of Integer);
var
  I       : Integer;
  LColumn : TVirtualTreeColumn;
begin
  Header.Columns.Clear;

  if High(AColumnNames) <> High(AColumnWidths) then
    raise Exception.Create
      ('Number of column names must match the number of column widths.')
  else
  begin
    for I := Low(AColumnNames) to High(AColumnNames) do
    begin
      LColumn := Header.Columns.Add;
      LColumn.Text := AColumnNames[I];
      if AColumnWidths[I] > 0 then
        LColumn.Width := AColumnWidths[I]
      else
      begin
        Header.AutoSizeIndex := LColumn.Index;
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
var
  ACancelEntry: Boolean;
  ANode       : PVirtualNode;
  //NodeData   : PLogNodeData;
  NodeData : TLogNode;
  DoScroll   : Boolean;
begin
  ACancelEntry := False;
  DoOnBeforeLog(AValue, ACancelEntry, ALogLevel);
  if not ACancelEntry then
  begin
    DoScroll := ((not Focused) or (GetLast = FocusedNode)) and FAutoScroll;
    ANode := AddChild(nil);

    NodeData := GetNodeData<TLogNode>(ANode);
    //NodeData := Getn NodeData(Node);

    if Assigned(NodeData) then
    begin
      NodeData.Data.Level := ALogLevel;

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
  end;
  end;
end;

procedure TLogTree.Log(AValue: string; const AArgs: Array of
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
