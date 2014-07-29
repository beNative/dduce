{
  Copyright (C) 2013-2014 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{
  The Original Code is VirtualLogTree.pas. The Initial Developer of the Original
  Code is Paul Thornton. Portions created by the Initial Developer are
  Copyright (C), All Rights Reserved.
}

unit DDuce.Components.LogTree;

{$I ..\DDuce.inc}

interface

uses
  System.Classes, System.SysUtils, System.Types,
  WinApi.Windows,
  Vcl.Graphics, Vcl.ImgList, Vcl.Menus,

  VirtualTrees;

type
  TLogLevel = (
    llNone,
    llError,
    llInfo,
    llWarning,
    llDebug
  );

  TLogLevels = set of TLogLevel;

  TLogNodeData = record
    LogLevel: TLogLevel;
    Timestamp: TDateTime;
    LogText: string;
  end;

  PLogNodeData = ^TLogNodeData;

  TOnLog = procedure(Sender: TObject; var LogText: string; var
    CancelEntry: Boolean; LogLevel: TLogLevel) of object;
  TOnPopupMenuItemClick = procedure(Sender: TObject; MenuItem:
    TMenuItem) of object;

  TLogPopupmenu = class(TPopupMenu)
  private
    FOwner                : TComponent;
    FOnPopupMenuItemClick : TOnPopupMenuItemClick;

    procedure OnMenuItemClick(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;

    property OnPopupMenuItemClick: TOnPopupMenuItemClick read
      FOnPopupMenuItemClick write FOnPopupMenuItemClick;
  end;

  TLogTree = class(TVirtualStringTree)
  private
    FOnLog                   : TOnLog;
    FOnAfterLog              : TNotifyEvent;
    FHTMLSupport             : Boolean;
    FAutoScroll              : Boolean;
    FRemoveControlCharacters : Boolean;
    FLogLevels               : TLogLevels;
    FAutoLogLevelColors      : Boolean;
    FShowDateColumn          : Boolean;
    FShowImages              : Boolean;
    FMaximumLines            : Cardinal;

    function DrawHTML(const ARect: TRect; const ACanvas: TCanvas;
      const Text: string; Selected: Boolean): Integer;
    function GetCellText(const Node: PVirtualNode; const Column:
      TColumnIndex): string;
    procedure SetLogLevels(const Value: TLogLevels);
    procedure UpdateVisibleItems;
    procedure OnPopupMenuItemClick(Sender: TObject; MenuItem: TMenuItem);
    procedure SetShowDateColumn(const Value: Boolean);
    procedure SetShowImages(const Value: Boolean);
    procedure AddDefaultColumns(const ColumnNames: array of string;
      const ColumnWidths: array of Integer);
    function IfThen(Condition: Boolean; TrueResult,
      FalseResult: Variant): Variant;
    function StripHTMLTags(const Value: string): string;
    function RemoveCtrlChars(const Value: string): string;

  protected
    procedure DoOnLog(var LogText: string; var CancelEntry: Boolean;
      LogLevel: TLogLevel); virtual;
    procedure DoOnAfterLog; virtual;
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: string); override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer):
      TCustomImageList; override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas;
      Column: TColumnIndex; TextType: TVSTTextType); override;
    procedure Loaded; override;
    procedure DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode;
      var NodeHeight: Integer); override;
    procedure DoInitNode(Parent: PVirtualNode; Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;

  public
    constructor Create(AOwner: TComponent); override;

    procedure Log(Value: string; LogLevel: TLogLevel = llInfo;
      Timestamp: TDateTime = 0);
    procedure LogFmt(Value: string; const Args: array of Const;
      LogLevel: TLogLevel = llInfo; Timestamp: TDateTime = 0);
    procedure SaveToFileWithDialog;
    procedure SaveToFile(const Filename: string);
    procedure SaveToStrings(const Strings: TStrings);
    procedure CopyToClipboard; reintroduce;
    procedure Init;

  published
    property OnLog: TOnLog
      read FOnLog write FOnLog;

    property OnAfterLog: TNotifyEvent
      read FOnAfterLog write FOnAfterLog;

    property HTMLSupport: Boolean
      read FHTMLSupport write FHTMLSupport;

    property AutoScroll: Boolean
      read FAutoScroll write FAutoScroll;

    property RemoveControlCharacters: Boolean
      read FRemoveControlCharacters write FRemoveControlCharacters;

    property LogLevels: TLogLevels
      read FLogLevels write SetLogLevels;

    property AutoLogLevelColors: Boolean
      read FAutoLogLevelColors write FAutoLogLevelColors;

    property ShowDateColumn: Boolean
      read FShowDateColumn write SetShowDateColumn;

    property ShowImages: Boolean
      read FShowImages write SetShowImages;

    property MaximumLines: Cardinal
      read FMaximumLines write FMaximumLines;
  end;

implementation

uses
  System.UITypes,
  Vcl.Dialogs, Vcl.Clipbrd;

resourcestring
  SSaveLog         = '&Save';
  SCopyToClipboard = '&Copy';
  STextFilesTxt    = 'Text files (*.txt)|*.txt|All files (*.*)|*.*';
  SSave            = 'Save';
  SDate            = 'Date';
  SLog             = 'Log';

constructor TLogTree.Create(AOwner: TComponent);
begin
  inherited;

  FAutoScroll              := True;
  FHTMLSupport             := True;
  FRemoveControlCharacters := False;
  FShowDateColumn          := True;
  FShowImages              := True;
  FLogLevels               := [llError, llInfo, llWarning, llDebug];
  NodeDataSize             := SizeOf(TLogNodeData);
  Loaded;
end;

procedure TLogTree.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);
var
  ColWidth: Integer;
begin
  inherited;

  if Column = 1 then
  begin
    if FHTMLSupport then
      ColWidth := DrawHTML(CellRect, Canvas, GetCellText(Node,
        Column), Selected[Node])
    else
      ColWidth := Canvas.TextWidth(GetCellText(Node, Column));

    if not FShowDateColumn then
      ColWidth := ColWidth + 32; // Width of image

    if ColWidth > Header.Columns[1].MinWidth then
      Header.Columns[1].MinWidth := ColWidth;
  end;
end;

procedure TLogTree.DoFreeNode(Node: PVirtualNode);
var
  NodeData: PLogNodeData;
begin
  inherited;

  NodeData := GetNodeData(Node);

  if Assigned(NodeData) then
    NodeData.LogText := '';
end;

function TLogTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
var
  NodeData: PLogNodeData;
begin
  Images.Count;

  if ((FShowImages) and (Kind in [ikNormal, ikSelected])) and
    (((FShowDateColumn) and (Column <= 0)) or
    ((not FShowDateColumn) and (Column = 1))) then
  begin
    NodeData := GetNodeData(Node);

    if Assigned(NodeData) then
      case NodeData.LogLevel of
        llError:
          Index := 3;
        llInfo:
          Index := 2;
        llWarning:
          Index := 1;
        llDebug:
          Index := 0;
      else
        Index := 4;
      end;
  end;

  Result := inherited DoGetImageIndex(Node, Kind, Column, Ghosted, Index);
end;

procedure TLogTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: string);
begin
  inherited;

  if (TextType = ttNormal) and ((Column <= 0) or (not FHTMLSupport)) then
    Text := GetCellText(Node, Column)
  else
    Text := '';
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
    if Cardinal(NodeHeight) > DefaultNodeHeight then
      NodeHeight := NodeHeight + 4; // needed to avoid multiline text drawing issues
  end;
end;

procedure TLogTree.DoOnAfterLog;
begin
  if Assigned(FOnAfterLog) then
    FOnAfterLog(Self);
end;

procedure TLogTree.DoOnLog(var LogText: string; var
  CancelEntry: Boolean; LogLevel: TLogLevel);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, LogText, CancelEntry, LogLevel);
end;

procedure TLogTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas;
  Column: TColumnIndex; TextType: TVSTTextType);
begin
  inherited;
  Canvas.Font.Color := clBlack;
end;

function TLogTree.GetCellText(const Node: PVirtualNode; const
  Column: TColumnIndex): string;
var
  NodeData: PLogNodeData;
begin
  NodeData := GetNodeData(Node);

  if Assigned(NodeData) then
    case Column of
      - 1, 0:
        Result := Concat(DateTimeToStr(NodeData.Timestamp), '.',
          FormatDateTime('zzz', NodeData.Timestamp));
      1:
        Result := NodeData.LogText;
    end;
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
  inherited;

  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowRoot,
    toShowTreeLines, toShowButtons] + [toUseBlendedSelection,
    toShowHorzGridLines, toHideFocusRect];
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions +
    [toFullRowSelect, toRightClickSelect];

  AddDefaultColumns([SDate,
    SLog],
    [170,
    120]);

  Header.AutoSizeIndex := 1;
  Header.Columns[1].MinWidth := 300;
  Header.Options := Header.Options + [hoAutoResize];

  if not Assigned(PopupMenu) and not (csDesigning in ComponentState) then
  begin
    PopupMenu := TLogPopupmenu.Create(Self);
    TLogPopupmenu(PopupMenu).OnPopupMenuItemClick :=
      OnPopupMenuItemClick;
  end;

  SetShowDateColumn(FShowDateColumn);
end;

procedure TLogTree.OnPopupMenuItemClick(Sender: TObject;
  MenuItem: TMenuItem);
begin
  if MenuItem.Tag = 1 then
    SaveToFileWithDialog
  else if MenuItem.Tag = 2 then
    CopyToClipboard;
end;

procedure TLogTree.SaveToFileWithDialog;
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(Self);
  try
    SaveDialog.DefaultExt := '.txt';
    SaveDialog.Title := SSave;
    SaveDialog.Options := SaveDialog.Options + [ofOverwritePrompt];
    SaveDialog.Filter := STextFilesTxt;

    if SaveDialog.Execute then
      SaveToFile(SaveDialog.Filename);
  finally
    FreeAndNil(SaveDialog);
  end;
end;

procedure TLogTree.SaveToFile(const Filename: string);
var
  SaveStrings: TStringList;
begin
  SaveStrings := TStringList.Create;
  try
    SaveToStrings(SaveStrings);

    SaveStrings.SaveToFile(Filename);
  finally
    FreeAndNil(SaveStrings);
  end;
end;

procedure TLogTree.CopyToClipboard;
var
  CopyStrings: TStringList;
begin
  CopyStrings := TStringList.Create;
  try
    SaveToStrings(CopyStrings);

    Clipboard.AsText := CopyStrings.Text;
  finally
    FreeAndNil(CopyStrings);
  end;
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

function TLogTree.StripHTMLTags(const Value: string): string;
var
  TagBegin, TagEnd, TagLength: Integer;
begin
  Result := Value;

  TagBegin := Pos('<', Result); // search position of first <

  while TagBegin > 0 do
  begin
    TagEnd := Pos('>', Result);
    TagLength := TagEnd - TagBegin + 1;

    Delete(Result, TagBegin, TagLength);
    TagBegin := Pos('<', Result);
  end;
end;

procedure TLogTree.SaveToStrings(const Strings: TStrings);
var
  Node: PVirtualNode;
begin
  Node := GetFirst;

  while Assigned(Node) do
  begin
    Strings.Add(Concat(IfThen(FShowDateColumn,
      Concat(GetCellText(Node, 0), #09), ''), IfThen(FHTMLSupport,
      StripHTMLTags(GetCellText(Node, 1)), GetCellText(Node, 1))));

    Node := Node.NextSibling;
  end;
end;

function TLogTree.RemoveCtrlChars(const Value: string): string;
var
  I: Integer;
begin
 // Replace CTRL characters with <whitespace>
  Result := '';

  for I := 1 to Length(Value) do
    if (AnsiChar(Value[I]) in [#0 .. #31, #127]) then
      Result := Result + ' '
    else
      Result := Result + Value[I];
end;

procedure TLogTree.Log(Value: string; LogLevel: TLogLevel;
  Timestamp: TDateTime);
var
  CancelEntry: Boolean;
  Node       : PVirtualNode;
  NodeData   : PLogNodeData;
  DoScroll   : Boolean;
begin
  CancelEntry := False;

  DoOnLog(Value, CancelEntry, LogLevel);

  if not CancelEntry then
  begin
    DoScroll := ((not Focused) or (GetLast = FocusedNode)) and FAutoScroll;

    Node := AddChild(nil);

    NodeData := GetNodeData(Node);

    if Assigned(NodeData) then
    begin
      NodeData.LogLevel := LogLevel;

      if Timestamp = 0 then
        NodeData.Timestamp := now
      else
        NodeData.Timestamp := Timestamp;

      if FRemoveControlCharacters then
        Value := RemoveCtrlChars(Value);

      if FAutoLogLevelColors then
        case LogLevel of
          llError:
            Value := Concat('<font-color=clRed>', Value, '</font-color>');
          llInfo:
            Value := Concat('<font-color=clBlack>', Value, '</font-color>');
          llWarning:
            Value := Concat('<font-color=clBlue>', Value, '</font-color>');
          llDebug:
            Value := Concat('<font-color=clGreen>', Value, '</font-color>')
        end;

      NodeData.LogText := Value;

      IsVisible[Node] := NodeData.LogLevel in FLogLevels;

      DoOnAfterLog;
    end;

    if FMaximumLines <> 0 then
      while RootNodeCount > FMaximumLines do
        DeleteNode(GetFirst);

    if DoScroll then
    begin
      ScrollIntoView(GetLast, False);
    end;
  end;
end;

procedure TLogTree.LogFmt(Value: string; const Args: Array of
  Const; LogLevel: TLogLevel; Timestamp: TDateTime);
begin
  Log(format(Value, Args), LogLevel, Timestamp);
end;

procedure TLogTree.SetLogLevels(const Value: TLogLevels);
begin
  FLogLevels := Value;
  UpdateVisibleItems;
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

procedure TLogTree.UpdateVisibleItems;
var
  Node    : PVirtualNode;
  NodeData: PLogNodeData;
begin
  BeginUpdate;
  try
    Node := GetFirst;

    while Assigned(Node) do
    begin
      NodeData := GetNodeData(Node);

      if Assigned(NodeData) then
        IsVisible[Node] := NodeData.LogLevel in FLogLevels;

      Node := Node.NextSibling;
    end;

    Invalidate;
  finally
    EndUpdate;
  end;
end;

function TLogTree.DrawHTML(const ARect: TRect; const ACanvas:
  TCanvas; const Text: string; Selected: Boolean): Integer;
{
 DrawHTML - Draws text on a canvas using tags based on a simple
 subset of HTML/CSS

 <b> - Bold e.g. <b>This is bold</b>
 <i> - Italic e.g. <i>This is italic</i>
 <u> - Underline e.g. <u>This is underlined</u>
 <font-color=X> - Font color
   examples:
   <font-color=clRed>Delphi red</font-color>
   <font-color=#FFFFFF>Web white</font-color>
   <font-color=$000000>Hex black</font-color>
 <font-size=X> Font size
   example: <font-size=30>This is some big text</font-size>
 <font-family> Font family
   example: <font-family=Arial>This is Arial</font-family>
}

  function CloseTag(const ATag: string): string;
  begin
    Result := Concat('/', ATag);
  end;

  function GetTagValue(const ATag: string): string;
  var
    P: Integer;
  begin
    P := Pos('=', ATag);

    if P = 0 then
      Result := ''
    else
      Result := Copy(ATag, P + 1, MaxInt);
  end;

  function ColorCodeToColor(const Value: string): TColor;
  var
    HexValue: string;
  begin
    Result := 0;

    if Value <> '' then
    begin
      if (Length(Value) >= 2) and (Copy(Uppercase(Value), 1, 2) = 'CL') then
      begin
       // Delphi color
        Result := StringToColor(Value);
      end
      else if Value[1] = '#' then
      begin
       // Web color
        HexValue := Copy(Value, 2, 6);

        Result := RGB(StrToInt('$' + Copy(HexValue, 1, 2)),
          StrToInt('$' + Copy(HexValue, 3, 2)),
          StrToInt('$' + Copy(HexValue, 5, 2)));
      end
      else
       // Hex or decimal color
        Result := StrToIntDef(Value, 0);
    end;
  end;

const
  TagBold       = 'B';
  TagItalic     = 'I';
  TagUnderline  = 'U';
  TagBreak      = 'BR';
  TagFontSize   = 'FONT-SIZE';
  TagFontFamily = 'FONT-FAMILY';
  TagFontColor  = 'FONT-COLOR';
  TagColor      = 'COLOR';

var
  X                  : Integer;
  Y                  : Integer;
  Idx                : Integer;
  CharWidth          : Integer;
  MaxCharHeight      : Integer;
  CurrChar           : Char;
  Tag                : string;
  TagValue           : string;
  PreviousFontColor  : TColor;
  PreviousFontFamily : string;
  PreviousFontSize   : Integer;
  PreviousColor      : TColor;

begin
  ACanvas.Font.Size := Canvas.Font.Size;
  ACanvas.Font.Name := Canvas.Font.Name;
  ACanvas.Font.Color := Canvas.Font.Color;
  ACanvas.Font.Style := Canvas.Font.Style;

  PreviousFontColor := ACanvas.Font.Color;
  PreviousFontFamily := ACanvas.Font.Name;
  PreviousFontSize := ACanvas.Font.Size;
  PreviousColor := ACanvas.Brush.Color;

  X := ARect.Left;
  Y := ARect.Top + 1;
  Idx := 1;

  MaxCharHeight := ACanvas.TextHeight('Ag');

  while Idx <= Length(Text) do
  begin
    CurrChar := Text[Idx];

   // Is this a tag?
    if CurrChar = '<' then
    begin
      Tag := '';
      Inc(Idx);
     // Find the end of then tag
      while (Text[Idx] <> '>') and (Idx <= Length(Text)) do
      begin
        Tag := Concat(Tag, Uppercase(Text[Idx]));
        Inc(Idx);
      end;

     // Simple tags
      if Tag = TagBold then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsBold]
      else if Tag = TagItalic then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsItalic]
      else if Tag = TagUnderline then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline]
      else if Tag = TagBreak then
      begin
        X := ARect.Left;
        Inc(Y, MaxCharHeight);
      end
      // Closing tags
      else if Tag = CloseTag(TagBold) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsBold]
      else if Tag = CloseTag(TagItalic) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsItalic]
      else if Tag = CloseTag(TagUnderline) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsUnderline]
      else if Tag = CloseTag(TagFontSize) then
        ACanvas.Font.Size := PreviousFontSize
      else if Tag = CloseTag(TagFontFamily) then
        ACanvas.Font.Name := PreviousFontFamily
      else if Tag = CloseTag(TagFontColor) then
        ACanvas.Font.Color := PreviousFontColor
      else if Tag = CloseTag(TagColor) then
        ACanvas.Brush.Color := PreviousColor
      else
     // Tags with values
      begin
       // Get the tag value (everything after '=')
        TagValue := GetTagValue(Tag);

        if TagValue <> '' then
        begin
         // Remove the value from the tag
          Tag := Copy(Tag, 1, Pos('=', Tag) - 1);

          if Tag = TagFontSize then
          begin
            PreviousFontSize := ACanvas.Font.Size;
            ACanvas.Font.Size := StrToIntDef(TagValue, ACanvas.Font.Size);
          end
          else if Tag = TagFontFamily then
          begin
            PreviousFontFamily := ACanvas.Font.Name;
            ACanvas.Font.Name := TagValue;
          end;

          if Tag = TagFontColor then
          begin
            PreviousFontColor := ACanvas.Font.Color;
            try
              ACanvas.Font.Color := ColorCodeToColor(TagValue);
            except
             //Just in case the canvas color is invalid
            end;
          end
          else if Tag = TagColor then
          begin
            PreviousColor := ACanvas.Brush.Color;
            try
              ACanvas.Brush.Color := ColorCodeToColor(TagValue);
            except
             //Just in case the canvas color is invalid
            end;
          end;
        end;
      end;
    end
    else if CurrChar >= #32 then // Draw the character if it's not a ctrl char
    begin
      CharWidth := ACanvas.TextWidth(CurrChar);

      if Y + MaxCharHeight < ARect.Bottom then
      begin
        ACanvas.Brush.Style := bsClear;
        ACanvas.TextOut(X, Y, CurrChar);
      end;

      X := X + CharWidth;
    end;

    Inc(Idx);
  end;

  Result := X - ARect.Left;
end;

{ TLogPopupmenu }

constructor TLogPopupmenu.Create(AOwner: TComponent);

  function AddMenuItem(const ACaption: string; ATag: Integer): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);

    Result.Caption := ACaption;
    Result.Tag := ATag;
    Result.OnClick := OnMenuItemClick;

    Items.Add(Result);
  end;

begin
  inherited Create(AOwner);

  FOwner := AOwner;

  AddMenuItem(SSaveLog, 1);
  AddMenuItem('-', -1);
  AddMenuItem(SCopyToClipboard, 2);
end;

procedure TLogPopupmenu.OnMenuItemClick(Sender: TObject);
begin
  if Assigned(FOnPopupMenuItemClick) then
    FOnPopupMenuItemClick(Self, TMenuItem(Sender));
end;

end.
