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

unit DDuce.Components.XmlTree;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Rtti,
  Xml.XMLIntf, // Keep IXMLNode, IXMLDocument here

  Spring,

  VirtualTrees.Types, VirtualTrees.Header, VirtualTrees.BaseTree, VirtualTrees,

  DDuce.Components.SectionTree, DDuce.Settings.TextFormat,
  DDuce.Components.VirtualTrees.Node;

const
  DEFAULT_DATETIMEFORMAT = 'dd-mm-yyyy hh:nn:ss.zzz';

type
  TXmlNode = TVTNode<IXMLNode>;

type
  TXmlTree = class(TSectionTree)
  private type
    { Holds text format settings for different XML node types. }
    TColorSettings = class(TPersistent)
    private
      FElement               : TTextFormatSettings;
      FAttribute             : TTextFormatSettings;
      FTextNode              : TTextFormatSettings;
      FValue             : TTextFormatSettings; // For attribute/text/cdata values
      FComment               : TTextFormatSettings;
      FCData                 : TTextFormatSettings;
      FProcessingInstruction : TTextFormatSettings;
      FDocType               : TTextFormatSettings;
      FDocument              : TTextFormatSettings; // Style for the root document node if displayed
      FEntityRef             : TTextFormatSettings; // Style for entity references if displayed
      FEntity                : TTextFormatSettings; // Style for entities if displayed
      FDocFragment           : TTextFormatSettings; // Style for document fragments if displayed
      FNotation              : TTextFormatSettings; // Style for notations if displayed
      FOther                 : TTextFormatSettings; // Fallback for unstyled types

      FOnChanged            : Event<TNotifyEvent>;

    protected
      function GetOnChanged: IEvent<TNotifyEvent>;
      procedure InitializeObjects;
      procedure Changed;
      procedure FormatSettingsChanged(Sender: TObject);

    public
      procedure AfterConstruction; override;
      destructor Destroy; override;

      property Element: TTextFormatSettings
        read FElement;

      property Attribute: TTextFormatSettings
        read FAttribute;

      property Value: TTextFormatSettings
        read FValue;

      property TextNode: TTextFormatSettings
        read FTextNode;

      property Comment: TTextFormatSettings
        read FComment;

      property CData: TTextFormatSettings
        read FCData;

      property ProcessingInstruction: TTextFormatSettings
        read FProcessingInstruction;

      property DocType: TTextFormatSettings
        read FDocType;

      property Document: TTextFormatSettings
        read FDocument;

      property EntityRef: TTextFormatSettings
        read FEntityRef;

      property Entity: TTextFormatSettings
        read FEntity;

      property DocFragment: TTextFormatSettings
        read FDocFragment;

      property Notation: TTextFormatSettings
        read FNotation;

      property Other: TTextFormatSettings
        read FOther;

      property OnChanged: IEvent<TNotifyEvent>
        read GetOnChanged;
    end;

  private
    FXmlDocument   : IXMLDocument;
    FColorSettings : TColorSettings;

    function IsWhitespaceNode(const AXmlNode: IXMLNode): Boolean;
    // Function to get display name for node types without a standard NodeName
    function GetNodeTypeName(ANodeType: TNodeType): string;

  protected
    procedure BuildTree; override;
    function GetNode(const AVNode: PVirtualNode): TXmlNode;
    procedure ParseNode(AParentNode: TXmlNode; AXmlNode: IXMLNode);

    {$REGION 'event dispatch methods'}
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoGetText(var pEventArgs: TVSTGetCellTextEventArgs); override;
    procedure DoPaintText(
      Node         : PVirtualNode;
      const Canvas : TCanvas;
      Column       : TColumnIndex;
      TextType     : TVSTTextType
    ); override;
    procedure DoBeforeCellPaint(
      Canvas          : TCanvas;
      Node            : PVirtualNode;
      Column          : TColumnIndex;
      CellPaintMode   : TVTCellPaintMode;
      CellRect        : TRect;
      var ContentRect : TRect
    ); override;
    procedure DoNodeDblClick(const HitInfo: THitInfo); override;
    procedure DoNewText(
      Node       : PVirtualNode;
      Column     : TColumnIndex;
      const Text : string
    ); override;
    procedure DoInitNode(
      Parent         : PVirtualNode;
      ANode          : PVirtualNode;
      var InitStates : TVirtualNodeInitStates
    ); override;
    procedure DoMeasureItem(
      TargetCanvas   : TCanvas;
      Node           : PVirtualNode;
      var NodeHeight : Integer
    ); override;
    {$ENDREGION}

    {$REGION 'property access methods'}
    function GetXmlString: string;
    procedure SetXmlString(const Value: string);
    function GetFocusedXmlNode: TXmlNode;
    function GetFocusedValue: string;
    {$ENDREGION}

    procedure FColorSettingsChanged(Sender: TObject);

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property ColorSettings: TColorSettings
      read FColorSettings;

    property FocusedXmlNode: TXmlNode
      read GetFocusedXmlNode;

    property FocusedValue: string
      read GetFocusedValue;

    property XmlString: string
      read GetXmlString write SetXmlString;
  end;

implementation

uses
  Winapi.Windows,    // For DrawText and DT_ constants
  System.Variants, System.Math, System.Character, // For TCharHelper
  Vcl.Graphics,      // For TBitmap, TColor, TCanvas, TFont
  Xml.XMLDoc,        // For LoadXMLData

  DDuce.Logger;

{$REGION 'TXmlTree.TColorSettings'}
procedure TXmlTree.TColorSettings.AfterConstruction;
begin
  inherited;
  InitializeObjects;
end;

destructor TXmlTree.TColorSettings.Destroy;
begin
  FreeAndNil(FElement);
  FreeAndNil(FAttribute);
  FreeAndNil(FTextNode);
  FreeAndNil(FValue);
  FreeAndNil(FComment);
  FreeAndNil(FCData);
  FreeAndNil(FProcessingInstruction);
  FreeAndNil(FDocType);
  FreeAndNil(FDocument);
  FreeAndNil(FEntityRef);
  FreeAndNil(FEntity);
  FreeAndNil(FDocFragment);
  FreeAndNil(FNotation);
  FreeAndNil(FOther);
  inherited;
end;

procedure TXmlTree.TColorSettings.InitializeObjects;
const
  DefaultFontName = 'Consolas';
  DefaultFontSize = 10;
begin
  FElement := TTextFormatSettings.Create;
  FElement.FontColor := clNavy;
  FElement.FontName  := DefaultFontName;
  FElement.FontSize  := DefaultFontSize;
  FElement.FontStyle := [fsBold];
  FElement.BackgroundColor := clWhite;
  FElement.OnChanged.Add(FormatSettingsChanged);

  FAttribute := TTextFormatSettings.Create;
  FAttribute.FontColor := clPurple;
  FAttribute.FontName  := DefaultFontName;
  FAttribute.FontSize  := DefaultFontSize;
  FAttribute.BackgroundColor := clWhite;
  FAttribute.OnChanged.Add(FormatSettingsChanged);

  FValue := TTextFormatSettings.Create;
  FValue.FontColor := clGreen;
  FValue.FontName  := DefaultFontName;
  FValue.FontSize  := DefaultFontSize;
  FValue.BackgroundColor := clWhite;
  FValue.OnChanged.Add(FormatSettingsChanged);

  FTextNode := TTextFormatSettings.Create;
  FTextNode.FontColor := clGray;
  FTextNode.FontName  := DefaultFontName;
  FTextNode.FontSize  := DefaultFontSize;
  FTextNode.BackgroundColor := clWhite;
  FTextNode.OnChanged.Add(FormatSettingsChanged);

  FComment := TTextFormatSettings.Create;
  FComment.FontColor := clGray;
  FComment.FontName  := DefaultFontName;
  FComment.FontSize  := DefaultFontSize;
  FComment.FontStyle := [fsItalic];
  FComment.BackgroundColor := clWhite;
  FComment.OnChanged.Add(FormatSettingsChanged);

  FCData := TTextFormatSettings.Create;
  FCData.FontColor := clTeal;
  FCData.FontName  := DefaultFontName;
  FCData.FontSize  := DefaultFontSize;
  FCData.BackgroundColor := clWhite;
  FCData.OnChanged.Add(FormatSettingsChanged);

  FProcessingInstruction := TTextFormatSettings.Create;
  FProcessingInstruction.FontColor := clOlive;
  FProcessingInstruction.FontName  := DefaultFontName;
  FProcessingInstruction.FontSize  := DefaultFontSize;
  FProcessingInstruction.FontStyle := [fsItalic];
  FProcessingInstruction.BackgroundColor := clWhite;
  FProcessingInstruction.OnChanged.Add(FormatSettingsChanged);

  FDocType := TTextFormatSettings.Create;
  FDocType.FontColor := clMaroon;
  FDocType.FontName  := DefaultFontName;
  FDocType.FontSize  := DefaultFontSize;
  FDocType.FontStyle := [fsBold];
  FDocType.BackgroundColor := clWhite;
  FDocType.OnChanged.Add(FormatSettingsChanged);

  FDocument := TTextFormatSettings.Create;
  FDocument.FontColor := clGray;
  FDocument.FontName  := DefaultFontName;
  FDocument.FontSize  := DefaultFontSize;
  FDocument.BackgroundColor := clWhite;
  FDocument.OnChanged.Add(FormatSettingsChanged);

  FEntityRef := TTextFormatSettings.Create;
  FEntityRef.FontColor := clGray;
  FEntityRef.FontName  := DefaultFontName;
  FEntityRef.FontSize  := DefaultFontSize;
  FEntityRef.BackgroundColor := clWhite;
  FEntityRef.OnChanged.Add(FormatSettingsChanged);

  FEntity := TTextFormatSettings.Create;
  FEntity.FontColor := clGray;
  FEntity.FontName  := DefaultFontName;
  FEntity.FontSize  := DefaultFontSize;
  FEntity.BackgroundColor := clWhite;
  FEntity.OnChanged.Add(FormatSettingsChanged);

  FDocFragment := TTextFormatSettings.Create;
  FDocFragment.FontColor := clGray;
  FDocFragment.FontName  := DefaultFontName;
  FDocFragment.FontSize  := DefaultFontSize;
  FDocFragment.BackgroundColor := clWhite;
  FDocFragment.OnChanged.Add(FormatSettingsChanged);

  FNotation := TTextFormatSettings.Create;
  FNotation.FontColor := clGray;
  FNotation.FontName  := DefaultFontName;
  FNotation.FontSize  := DefaultFontSize;
  FNotation.BackgroundColor := clWhite;
  FNotation.OnChanged.Add(FormatSettingsChanged);

  FOther := TTextFormatSettings.Create;
  FOther.FontColor := clGray;
  FOther.FontName  := DefaultFontName;
  FOther.FontSize  := DefaultFontSize;
  FOther.BackgroundColor := clWhite;
  FOther.OnChanged.Add(FormatSettingsChanged);
end;

function TXmlTree.TColorSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;

procedure TXmlTree.TColorSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;

procedure TXmlTree.TColorSettings.FormatSettingsChanged(Sender: TObject);
begin
  Changed;
end;
{$ENDREGION}

{$REGION 'TXmlTree'}
procedure TXmlTree.AfterConstruction;
begin
  inherited;
  FColorSettings := TColorSettings.Create;
  FColorSettings.OnChanged.Add(FColorSettingsChanged);
  Header.Options := Header.Options + [hoAutoResize];
  with Header.Columns.Add do
  begin
    Color    := clWhite;
    MaxWidth := 1200;
    MinWidth := 100;
    Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
      coParentColor, coResizable, coShowDropMark, coVisible, coSmartResize,
      coAllowFocus, coFixed{, coEditable}];
    Position := 0;
    Width    := 200;
    Text     := 'Name';
  end;
  with Header.Columns.Add do
  begin
    MaxWidth := 1200;
    MinWidth := 100;
    Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
      coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
      coSmartResize, coAllowFocus{, coEditable}];
    Position := 1;
    Width    := 400;
    Text     := 'Value';
  end;
  Header.AutoSizeIndex := 1;
    TreeOptions.MiscOptions := [
    toCheckSupport, toInitOnSave, toWheelPanning, toVariableNodeHeight
    {toEditable, toEditOnDblClick,}
  ];
  //TreeOptions.EditOptions := toVerticalEdit;
end;

destructor TXmlTree.Destroy;
begin
  Clear;
  FXmlDocument := nil;
  FreeAndNil(FColorSettings);
  inherited;
end;

procedure TXmlTree.BuildTree;
begin
  BeginUpdate;
  try
    Clear;
    if Assigned(FXmlDocument) and Assigned(FXmlDocument.DocumentElement) then
      ParseNode(nil, FXmlDocument.DocumentElement);
    FullExpand;
    Header.AutoFitColumns;
  finally
    EndUpdate;
  end;
end;

function TXmlTree.IsWhitespaceNode(const AXmlNode: IXMLNode): Boolean;
var
  S : string;
  C : Char;
begin
  Result := False;
  if Assigned(AXmlNode) and (AXmlNode.NodeType = ntText) then
  begin
    S := VarToStrDef(AXmlNode.NodeValue, '');
    Result := True;
    for C in S do
    begin
      if not C.IsWhiteSpace then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

// Helper function to get a display name for node types
function TXmlTree.GetNodeTypeName(ANodeType: TNodeType): string;
begin
  case ANodeType of
    ntText:
      Result := '#text';
    ntCData:
      Result := '#cdata-section';
    ntComment:
      Result := '#comment';
    ntDocument:
      Result := '#document';
    ntDocType:
      Result := '!DOCTYPE'; // NodeName will be appended later
    ntEntityRef:
      Result := '&'; // NodeName and ';' will be appended later
    ntEntity:
      Result := '#entity';
    ntDocFragment:
      Result := '#document-fragment';
    ntNotation:
      Result := '#notation';
    ntProcessingInstr:
      Result := ''; // Use NodeName (target)
    ntAttribute:
      Result := ''; // Use NodeName
    ntElement:
      Result := ''; // Use NodeName
  else
    Result := '#unknown';
  end;
end;

procedure TXmlTree.ParseNode(AParentNode: TXmlNode; AXmlNode: IXMLNode);
var
  LNode         : TXmlNode;
  LParentVNode  : PVirtualNode;
  I             : Integer;
  LChildXmlNode : IXMLNode;
begin
  if (AXmlNode.NodeType = ntText) and IsWhitespaceNode(AXmlNode) then
  begin
    Exit;
  end;

  if Assigned(AParentNode) then
    LParentVNode := AParentNode.VNode
  else
    LParentVNode := nil;

  LNode := TXmlNode.Create(Self, AXmlNode, False, LParentVNode);

  if AXmlNode.NodeType = ntElement then
  begin
    if Assigned(AXmlNode.AttributeNodes) then
      for I := 0 to AXmlNode.AttributeNodes.Count - 1 do
        TXmlNode.Create(Self, AXmlNode.AttributeNodes[I], False, LNode.VNode);
  end;

  if Assigned(AXmlNode.ChildNodes) then
    for I := 0 to AXmlNode.ChildNodes.Count - 1 do
    begin
      LChildXmlNode := AXmlNode.ChildNodes[I];
      ParseNode(LNode, LChildXmlNode);
    end;
end;

function TXmlTree.GetNode(const AVNode: PVirtualNode): TXmlNode;
begin
  Result := GetNodeData<TXmlNode>(AVNode);
end;

procedure TXmlTree.DoFreeNode(Node: PVirtualNode);
var
  LNode : TXmlNode;
begin
  LNode := GetNodeData<TXmlNode>(Node);
  if Assigned(LNode) then
  begin
    InitNode(Node);
    LNode.Free;
  end;
  inherited DoFreeNode(Node);
end;

procedure TXmlTree.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  LXmlNode: IXMLNode;
  LColor  : TColor;
begin
  inherited DoBeforeCellPaint(Canvas, Node, Column, CellPaintMode, CellRect,
    ContentRect);

  if CellPaintMode = cpmPaint then
  begin
    LXmlNode := GetNode(Node).Data;
    LColor   := ColorSettings.Other.BackgroundColor;

    case LXmlNode.NodeType of
      ntElement:
        LColor := ColorSettings.Element.BackgroundColor;
      ntAttribute:
        LColor := ColorSettings.Attribute.BackgroundColor;
      ntText:
        LColor := ColorSettings.TextNode.BackgroundColor;
      ntCData:
        LColor := ColorSettings.CData.BackgroundColor;
      ntProcessingInstr:
        LColor := ColorSettings.ProcessingInstruction.BackgroundColor;
      ntComment:
        LColor := ColorSettings.Comment.BackgroundColor;
      ntDocument:
        LColor := ColorSettings.Document.BackgroundColor;
      ntDocType:
        LColor := ColorSettings.DocType.BackgroundColor;
      ntEntityRef:
        LColor := ColorSettings.EntityRef.BackgroundColor;
      ntEntity:
        LColor := ColorSettings.Entity.BackgroundColor;
      ntDocFragment:
        LColor := ColorSettings.DocFragment.BackgroundColor;
      ntNotation:
        LColor := ColorSettings.Notation.BackgroundColor;
    end;

    if Column = 1 then
    begin
      if LXmlNode.NodeType in [ntAttribute, ntText, ntCData, ntComment,
        ntProcessingInstr] then
        LColor := ColorSettings.Value.BackgroundColor
      else if LXmlNode.NodeType = ntElement then
        LColor := ColorSettings.Element.BackgroundColor;
    end;

    Canvas.Brush.Color := LColor;
    Canvas.FillRect(CellRect);
  end;
end;

procedure TXmlTree.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);
var
  LNode       : TXmlNode;
  LXmlNode    : IXMLNode;
  LNodeNameStr: string;
begin
  LNode    := GetNode(pEventArgs.Node);
  LXmlNode := LNode.Data;

  // Get the base name representation
  LNodeNameStr := GetNodeTypeName(LXmlNode.NodeType);

  case LXmlNode.NodeType of
    ntElement, ntAttribute, ntProcessingInstr, ntEntity, ntNotation:
      LNodeNameStr := LXmlNode.NodeName; // Use actual name for these types
    ntDocType:
      LNodeNameStr := LNodeNameStr + ' ' + LXmlNode.NodeName;
      // Append name for DocType
    ntEntityRef:
      LNodeNameStr := LNodeNameStr + LXmlNode.NodeName + ';';
      // Append name and ; for EntityRef
  end;

  // Assign text based on column
  if pEventArgs.Column = 0 then
  begin
    pEventArgs.CellText := LNodeNameStr;
  end
  else if pEventArgs.Column = 1 then
  begin
    case LXmlNode.NodeType of
      ntElement:
        if LXmlNode.IsTextElement then
          pEventArgs.CellText := LXmlNode.Text
        else if LXmlNode.HasChildNodes then
          pEventArgs.CellText := Format('{%d}', [LXmlNode.ChildNodes.Count])
        else
          pEventArgs.CellText := '';
      ntAttribute:
        pEventArgs.CellText := VarToStrDef(LXmlNode.NodeValue, '');
      ntText, ntCData, ntComment:
        pEventArgs.CellText := VarToStrDef(LXmlNode.NodeValue, LXmlNode.Text);
        // Use NodeValue or Text
      ntProcessingInstr:
        pEventArgs.CellText := LXmlNode.NodeValue; // Data
      ntDocType, ntEntity, ntNotation:
        pEventArgs.CellText := VarToStrDef(LXmlNode.NodeValue, '');
        // Value is less common here
      ntDocument, ntEntityRef, ntDocFragment:
        pEventArgs.CellText := ''; // Usually no direct value in column 1
    else // Fallback
      pEventArgs.CellText := VarToStrDef(LXmlNode.NodeValue, LXmlNode.Text);
    end;
  end;
end;

procedure TXmlTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  const Text: string);
var
  LXmlNode : IXMLNode;
begin
  LXmlNode := GetNode(Node).Data;

  if Column = 1 then
  begin
    case LXmlNode.NodeType of
      ntAttribute:         LXmlNode.NodeValue := Text;
      ntText, ntCData, ntComment: LXmlNode.NodeValue := Text;
      ntProcessingInstr:   LXmlNode.NodeValue := Text;
    else
      Exit;
    end;
    InvalidateNode(Node);
  end
  else if Column = 0 then
  begin
//     case LXmlNode.NodeType of
//       ntAttribute:          LXmlNode.NodeName := Text;
//       ntProcessingInstr:    LXmlNode.NodeName := Text;
//     else
//       Exit;
//     end;
     InvalidateNode(Node);
  end;

  inherited DoNewText(Node, Column, Text);
end;

procedure TXmlTree.DoNodeDblClick(const HitInfo: THitInfo);
var
  LNode : TXmlNode;
begin
  LNode := GetNode(HitInfo.HitNode);
  if Assigned(LNode) then
    LNode.Expanded := not LNode.Expanded;
  inherited DoNodeDblClick(HitInfo);
end;

procedure TXmlTree.DoInitNode(
  Parent         : PVirtualNode;
  ANode          : PVirtualNode;
  var InitStates : TVirtualNodeInitStates
);
begin
  inherited;
  Include(InitStates, ivsMultiline);
end;

procedure TXmlTree.DoMeasureItem(
  TargetCanvas  : TCanvas;
  Node          : PVirtualNode;
  var NodeHeight: Integer);
var
  I            : Integer;
  H, MaxH      : Integer;
  S            : string;
  LNode        : TXmlNode;
  LXmlNode     : IXMLNode;
  R            : TRect;
  LDrawFormat  : Cardinal;
  LCol         : TVirtualTreeColumn;
  LBitmap      : TBitmap;
  LCanvas      : TCanvas;
  LFontStyles  : TFontStyles;
  LNodeNameStr : string; // To store calculated node name
begin
  MaxH := DefaultNodeHeight;

  if not Assigned(Node) then
  begin
    NodeHeight := MaxH;
    Exit;
  end;

  LNode := GetNode(Node);
  if not Assigned(LNode) or not Assigned(LNode.Data) then
  begin
    NodeHeight := MaxH;
    Exit;
  end;
  LXmlNode := LNode.Data;

  LBitmap := Vcl.Graphics.TBitmap.Create;
  try
    LCanvas := LBitmap.Canvas;
    LCanvas.Font.Assign(Self.Font);

    for I := 0 to Header.Columns.Count - 1 do
    begin
      LCol := Header.Columns[I];
      if Assigned(LCol) and (coVisible in LCol.Options) then
      begin
        // --- Get text based on column ---
        S := '';
        if I = 0 then // Name Column
        begin
          LNodeNameStr := GetNodeTypeName(LXmlNode.NodeType); // Get base name
          case LXmlNode.NodeType of
            ntElement, ntAttribute, ntProcessingInstr, ntEntity, ntNotation:
              LNodeNameStr := LXmlNode.NodeName;
            ntDocType:
              LNodeNameStr := LNodeNameStr + ' ' + LXmlNode.NodeName;
            ntEntityRef:
              LNodeNameStr := LNodeNameStr + LXmlNode.NodeName + ';';
          end;
          S := LNodeNameStr;
        end
        else // Value Column (I = 1)
        begin
          case LXmlNode.NodeType of
            ntElement:
              if LXmlNode.IsTextElement then
                S := LXmlNode.Text
              else if LXmlNode.HasChildNodes then
                S := Format('{%d}', [LXmlNode.ChildNodes.Count])
              else
                S := '';
            ntAttribute:
              S := VarToStrDef(LXmlNode.NodeValue, '');
            ntText, ntCData, ntComment:
              S := VarToStrDef(LXmlNode.NodeValue, LXmlNode.Text);
            ntProcessingInstr:
              S := LXmlNode.NodeValue;
            ntDocType, ntEntity, ntNotation:
              S := VarToStrDef(LXmlNode.NodeValue, '');
            ntDocument, ntEntityRef, ntDocFragment:
              S := '';
          else
            S := VarToStrDef(LXmlNode.NodeValue, LXmlNode.Text);
          end;
        end;
        // --- End Get text ---

        if S <> '' then
        begin
          // Apply appropriate font style for measurement
          LFontStyles := [];
          if I = 0 then // Name column
          begin
            case LXmlNode.NodeType of
              ntElement:
                LFontStyles := ColorSettings.Element.Font.Style;
              ntAttribute:
                LFontStyles := ColorSettings.Attribute.Font.Style;
              ntText:
                LFontStyles := ColorSettings.TextNode.Font.Style;
              ntCData:
                LFontStyles := ColorSettings.CData.Font.Style;
              ntProcessingInstr:
                LFontStyles := ColorSettings.ProcessingInstruction.Font.Style;
              ntComment:
                LFontStyles := ColorSettings.Comment.Font.Style;
              ntDocument:
                LFontStyles := ColorSettings.Document.Font.Style;
              ntDocType:
                LFontStyles := ColorSettings.DocType.Font.Style;
              ntEntityRef:
                LFontStyles := ColorSettings.EntityRef.Font.Style;
              ntEntity:
                LFontStyles := ColorSettings.Entity.Font.Style;
              ntDocFragment:
                LFontStyles := ColorSettings.DocFragment.Font.Style;
              ntNotation:
                LFontStyles := ColorSettings.Notation.Font.Style;
            else
              LFontStyles := ColorSettings.Other.Font.Style;
            end;
          end
          else // Value column (Column 1)
          begin
            case LXmlNode.NodeType of
              ntAttribute, ntText, ntCData, ntComment, ntProcessingInstr:
                LFontStyles := ColorSettings.Value.Font.Style;
              ntElement:
                if LXmlNode.IsTextElement then
                  LFontStyles := ColorSettings.Value.Font.Style
                else
                  LFontStyles := [];
            else
              LFontStyles := ColorSettings.Other.Font.Style;
            end;
          end;
          LCanvas.Font.Style := LFontStyles; // Assign the correct SET of styles

          R           := Rect(0, 0, LCol.Width - TextMargin * 2, 32767);
          LDrawFormat := DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX or
            DT_EDITCONTROL;

          H := Winapi.Windows.DrawText(LCanvas.Handle, PChar(S), Length(S), R,
            LDrawFormat);
          H    := H + GetSystemMetrics(SM_CYBORDER) * 2 + 2;
          MaxH := Max(MaxH, H);
        end;
      end;
    end;
  finally
    LBitmap.Free;
  end;

  NodeHeight := Max(MaxH, DefaultNodeHeight);
end;

procedure TXmlTree.DoPaintText(
  Node        : PVirtualNode;
  const Canvas: TCanvas;
  Column      : TColumnIndex;
  TextType    : TVSTTextType);
var
  LXmlNode: IXMLNode;
begin
  if TextType = ttNormal then
  begin
    LXmlNode := GetNode(Node).Data;

    if Column = 0 then
    begin
      case LXmlNode.NodeType of
        ntElement:
          Canvas.Font.Assign(ColorSettings.Element.Font);
        ntAttribute:
          Canvas.Font.Assign(ColorSettings.Attribute.Font);
        ntText:
          Canvas.Font.Assign(ColorSettings.TextNode.Font);
        ntCData:
          Canvas.Font.Assign(ColorSettings.CData.Font);
        ntProcessingInstr:
          Canvas.Font.Assign(ColorSettings.ProcessingInstruction.Font);
        ntComment:
          Canvas.Font.Assign(ColorSettings.Comment.Font);
        ntDocument:
          Canvas.Font.Assign(ColorSettings.Document.Font);
        ntDocType:
          Canvas.Font.Assign(ColorSettings.DocType.Font);
        ntEntityRef:
          Canvas.Font.Assign(ColorSettings.EntityRef.Font);
        ntEntity:
          Canvas.Font.Assign(ColorSettings.Entity.Font);
        ntDocFragment:
          Canvas.Font.Assign(ColorSettings.DocFragment.Font);
        ntNotation:
          Canvas.Font.Assign(ColorSettings.Notation.Font);
      else
        Canvas.Font.Assign(ColorSettings.Other.Font);
      end;
    end
    else if Column = 1 then
    begin
      case LXmlNode.NodeType of
        ntAttribute, ntText, ntCData, ntComment, ntProcessingInstr:
          Canvas.Font.Assign(ColorSettings.Value.Font);
        ntElement:
          if LXmlNode.IsTextElement then
            Canvas.Font.Assign(ColorSettings.Value.Font)
          else if LXmlNode.HasChildNodes then
            Canvas.Font.Color := clGray
          else
            Canvas.Font.Color := ColorSettings.Value.FontColor;
        ntDocType, ntEntityRef, ntEntity, ntDocFragment, ntNotation, ntDocument:
          Canvas.Font.Assign(ColorSettings.Other.Font);
      else
        Canvas.Font.Assign(ColorSettings.Other.Font);
      end;
    end;
  end;

  inherited DoPaintText(Node, Canvas, Column, TextType);
end;

function TXmlTree.GetFocusedXmlNode: TXmlNode;
begin
  Result := GetNode(FocusedNode);
end;

function TXmlTree.GetFocusedValue: string;
var
  LXmlNode     : IXMLNode;
  LNodeNameStr : string;
begin
  Result := '';
  if Assigned(FocusedXmlNode) then
  begin
    LXmlNode := FocusedXmlNode.Data;

    // Get the base name representation (consistent with DoGetText)
    LNodeNameStr := GetNodeTypeName(LXmlNode.NodeType);
    case LXmlNode.NodeType of
      ntElement, ntAttribute, ntProcessingInstr, ntEntity, ntNotation:
        LNodeNameStr := LXmlNode.NodeName;
      ntDocType:
        LNodeNameStr := LNodeNameStr + ' ' + LXmlNode.NodeName;
      ntEntityRef:
        LNodeNameStr := LNodeNameStr + LXmlNode.NodeName + ';';
    end;

    if FocusedColumn = 0 then
    begin
      Result := LNodeNameStr;
    end
    else if FocusedColumn = 1 then
    begin
      case LXmlNode.NodeType of
        ntElement:
          if LXmlNode.IsTextElement then
            Result := LXmlNode.Text
          else if LXmlNode.HasChildNodes then
            Result := Format('{%d}', [LXmlNode.ChildNodes.Count])
          else
            Result := '';
        ntAttribute:
          Result := VarToStrDef(LXmlNode.NodeValue, '');
        ntText, ntCData, ntComment:
          Result := VarToStrDef(LXmlNode.NodeValue, LXmlNode.Text);
        ntProcessingInstr:
          Result := LXmlNode.NodeValue;
        ntDocType, ntEntity, ntNotation:
          Result := VarToStrDef(LXmlNode.NodeValue, '');
        ntDocument, ntEntityRef, ntDocFragment:
          Result := '';
      else
        Result := VarToStrDef(LXmlNode.NodeValue, LXmlNode.Text);
      end;
    end;
  end;
end;

procedure TXmlTree.FColorSettingsChanged(Sender: TObject);
begin
  Invalidate;
end;

function TXmlTree.GetXmlString: string;
begin
  if Assigned(FXmlDocument) then
    Result := FXmlDocument.XML.Text
  else
    Result := '';
end;

procedure TXmlTree.SetXmlString(const Value: string);
begin
  if Value <> XmlString then
  begin
    BeginUpdate;
    try
      Clear;
      FXmlDocument := nil;
      if Trim(Value) <> '' then
      begin
        try
          FXmlDocument := LoadXMLData(Value);
        except
          on E: Exception do
          begin
            Logger.SendException('Failed to load XML data', E);
            FXmlDocument := nil;
          end;
        end;
      end;
      if Assigned(FXmlDocument) then
        BuildTree;
    finally
      EndUpdate;
    end;
  end;
end;
{$ENDREGION}

end.
