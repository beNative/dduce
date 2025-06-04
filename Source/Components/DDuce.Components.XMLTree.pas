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
  Xml.XMLIntf,

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
      FValue                 : TTextFormatSettings; // For attribute/text/cdata values
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
    // Renamed and logic adjusted: This now processes children/attributes OF AXmlNode
    // and adds them UNDER AParentTreeNode
    procedure ProcessXmlNodeChildren(
      AParentTreeNode : TXmlNode;
      AXmlNode        : IXMLNode
    );

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
    function GetFocusedValue: string; override;
    {$ENDREGION}

    procedure FColorSettingsChanged(Sender: TObject);

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property ColorSettings: TColorSettings
      read FColorSettings;

    property FocusedXmlNode: TXmlNode
      read GetFocusedXmlNode;

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
{$REGION 'construction and destruction'}
procedure TXmlTree.TColorSettings.AfterConstruction;
begin
  inherited AfterConstruction;
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
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TXmlTree.TColorSettings.InitializeObjects;
const
  DEFAULT_FONT_NAME = 'Consolas';
  DEFAULT_FONT_SIZE = 10;
begin
  FElement := TTextFormatSettings.Create;
  FElement.FontColor := clNavy;
  FElement.FontName  := DEFAULT_FONT_NAME;
  FElement.FontSize  := DEFAULT_FONT_SIZE;
  FElement.FontStyle := [fsBold];
  FElement.BackgroundColor := clWhite;
  FElement.OnChanged.Add(FormatSettingsChanged);

  FAttribute := TTextFormatSettings.Create;
  FAttribute.FontColor := clPurple;
  FAttribute.FontName  := DEFAULT_FONT_NAME;
  FAttribute.FontSize  := DEFAULT_FONT_SIZE;
  FAttribute.BackgroundColor := clWhite;
  FAttribute.OnChanged.Add(FormatSettingsChanged);

  FValue := TTextFormatSettings.Create;
  FValue.FontColor := clGreen;
  FValue.FontName  := DEFAULT_FONT_NAME;
  FValue.FontSize  := DEFAULT_FONT_SIZE;
  FValue.BackgroundColor := clWhite;
  FValue.OnChanged.Add(FormatSettingsChanged);

  FTextNode := TTextFormatSettings.Create;
  FTextNode.FontColor := clGray;
  FTextNode.FontName  := DEFAULT_FONT_NAME;
  FTextNode.FontSize  := DEFAULT_FONT_SIZE;
  FTextNode.BackgroundColor := clWhite;
  FTextNode.OnChanged.Add(FormatSettingsChanged);

  FComment := TTextFormatSettings.Create;
  FComment.FontColor := clGray;
  FComment.FontName  := DEFAULT_FONT_NAME;
  FComment.FontSize  := DEFAULT_FONT_SIZE;
  FComment.FontStyle := [fsItalic];
  FComment.BackgroundColor := clWhite;
  FComment.OnChanged.Add(FormatSettingsChanged);

  FCData := TTextFormatSettings.Create;
  FCData.FontColor := clTeal;
  FCData.FontName  := DEFAULT_FONT_NAME;
  FCData.FontSize  := DEFAULT_FONT_SIZE;
  FCData.BackgroundColor := clWhite;
  FCData.OnChanged.Add(FormatSettingsChanged);

  FProcessingInstruction := TTextFormatSettings.Create;
  FProcessingInstruction.FontColor := clOlive;
  FProcessingInstruction.FontName  := DEFAULT_FONT_NAME;
  FProcessingInstruction.FontSize  := DEFAULT_FONT_SIZE;
  FProcessingInstruction.FontStyle := [fsItalic];
  FProcessingInstruction.BackgroundColor := clWhite;
  FProcessingInstruction.OnChanged.Add(FormatSettingsChanged);

  FDocType := TTextFormatSettings.Create;
  FDocType.FontColor := clMaroon;
  FDocType.FontName  := DEFAULT_FONT_NAME;
  FDocType.FontSize  := DEFAULT_FONT_SIZE;
  FDocType.FontStyle := [fsBold];
  FDocType.BackgroundColor := clWhite;
  FDocType.OnChanged.Add(FormatSettingsChanged);

  FDocument := TTextFormatSettings.Create;
  FDocument.FontColor := clGray;
  FDocument.FontName  := DEFAULT_FONT_NAME;
  FDocument.FontSize  := DEFAULT_FONT_SIZE;
  FDocument.BackgroundColor := clWhite;
  FDocument.OnChanged.Add(FormatSettingsChanged);

  FEntityRef := TTextFormatSettings.Create;
  FEntityRef.FontColor := clGray;
  FEntityRef.FontName  := DEFAULT_FONT_NAME;
  FEntityRef.FontSize  := DEFAULT_FONT_SIZE;
  FEntityRef.BackgroundColor := clWhite;
  FEntityRef.OnChanged.Add(FormatSettingsChanged);

  FEntity := TTextFormatSettings.Create;
  FEntity.FontColor := clGray;
  FEntity.FontName  := DEFAULT_FONT_NAME;
  FEntity.FontSize  := DEFAULT_FONT_SIZE;
  FEntity.BackgroundColor := clWhite;
  FEntity.OnChanged.Add(FormatSettingsChanged);

  FDocFragment := TTextFormatSettings.Create;
  FDocFragment.FontColor := clGray;
  FDocFragment.FontName  := DEFAULT_FONT_NAME;
  FDocFragment.FontSize  := DEFAULT_FONT_SIZE;
  FDocFragment.BackgroundColor := clWhite;
  FDocFragment.OnChanged.Add(FormatSettingsChanged);

  FNotation := TTextFormatSettings.Create;
  FNotation.FontColor := clGray;
  FNotation.FontName  := DEFAULT_FONT_NAME;
  FNotation.FontSize  := DEFAULT_FONT_SIZE;
  FNotation.BackgroundColor := clWhite;
  FNotation.OnChanged.Add(FormatSettingsChanged);

  FOther := TTextFormatSettings.Create;
  FOther.FontColor := clGray;
  FOther.FontName  := DEFAULT_FONT_NAME;
  FOther.FontSize  := DEFAULT_FONT_SIZE;
  FOther.BackgroundColor := clWhite;
  FOther.OnChanged.Add(FormatSettingsChanged);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TXmlTree.TColorSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TXmlTree.TColorSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TXmlTree.TColorSettings.FormatSettingsChanged(Sender: TObject);
begin
  Changed;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TXmlTree'}
{$REGION 'construction and destruction'}
procedure TXmlTree.AfterConstruction;
begin
  inherited AfterConstruction;
  NodeDataSize := SizeOf(Pointer); // Store pointer to TXmlNode
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
    toCheckSupport, toInitOnSave, toWheelPanning, toVariableNodeHeight,
    toToggleOnDblClick // Added to allow expand/collapse on dblclick
    {toEditable, toEditOnDblClick,}
  ];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toShowTreeLines, toShowButtons]; // Ensure hierarchy visuals are on
  //TreeOptions.EditOptions := toVerticalEdit;
end;

destructor TXmlTree.Destroy;
begin
  Clear; // This should trigger DoFreeNode for all nodes
  FXmlDocument := nil;
  FreeAndNil(FColorSettings);
  inherited;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TXmlTree.DoFreeNode(Node: PVirtualNode);
var
  LNode : TXmlNode;
begin
  LNode := GetNodeData<TXmlNode>(Node); // Use the generic GetNodeData directly
  if Assigned(LNode) then
  begin
    SetNodeData(Node, nil); // Clear the data pointer in the VST node
    LNode.Free; // Free the TXmlNode object
  end;
  inherited DoFreeNode(Node);
end;

procedure TXmlTree.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  LTreeNode: TXmlNode;
  LXmlNode : IXMLNode;
  LColor   : TColor;
begin
  if CellPaintMode = cpmPaint then
  begin
    LTreeNode := GetNode(Node);
    if not Assigned(LTreeNode) or not Assigned(LTreeNode.Data) then
      Exit; // Safety check
    LXmlNode := LTreeNode.Data;

    // Determine background color based on Node Type (Default to White or Other)
    LColor := ColorSettings.Other.BackgroundColor; // Default

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

    // Override background for Value column based on Node Type
    if Column = 1 then
    begin
      case LXmlNode.NodeType of
        ntAttribute, ntText, ntCData, ntComment, ntProcessingInstr:
          LColor := ColorSettings.Value.BackgroundColor; // Use Value style background
        ntElement:
           // Elements don't have a "value" in col 1, keep element bg
          LColor := ColorSettings.Element.BackgroundColor;
        // For other types in col 1, keep their specific background or 'Other'
      end;
    end;

    // Fill the cell background
    Canvas.Brush.Color := LColor;
    Canvas.FillRect(CellRect);
  end;

  // Call inherited *after* filling background to allow VST to draw selection etc. over it
  inherited DoBeforeCellPaint(Canvas, Node, Column, CellPaintMode, CellRect, ContentRect);
end;

procedure TXmlTree.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);
var
  LNode        : TXmlNode;
  LXmlNode     : IXMLNode;
  LNodeNameStr : UnicodeString; // Ensure it's UnicodeString
  LValueStr    : UnicodeString; // Ensure it's UnicodeString
  LNodeValue   : OleVariant;
begin
  pEventArgs.CellText := ''; // Default to empty

  LNode := GetNode(pEventArgs.Node);
  if not Assigned(LNode) or not Assigned(LNode.Data) then
    Exit;

  LXmlNode := LNode.Data;

  // --- Get Name String (Direct Assignment) ---
  LNodeNameStr := GetNodeTypeName(LXmlNode.NodeType);
  case LXmlNode.NodeType of
    ntElement, ntAttribute, ntProcessingInstr, ntEntity, ntNotation:
      LNodeNameStr := LXmlNode.NodeName; // REMOVED + ''
    ntDocType:
      LNodeNameStr := LNodeNameStr + ' ' + LXmlNode.NodeName; // REMOVED + ''
    ntEntityRef:
      if LXmlNode.NodeName <> '' then
        LNodeNameStr := '&' + LXmlNode.NodeName + ';' // REMOVED + ''
      else
        LNodeNameStr := '&;';
  end;

  // Assign text based on column
  if pEventArgs.Column = 0 then // Name Column
  begin
    pEventArgs.CellText := LNodeNameStr;
  end
  else if pEventArgs.Column = 1 then // Value Column
  begin
    LValueStr := ''; // Default value to empty
    try
      case LXmlNode.NodeType of
        ntAttribute, ntText, ntCData, ntComment, ntProcessingInstr, ntDocType, ntEntity, ntNotation:
           LNodeValue := LXmlNode.NodeValue;
      else
           LNodeValue := Null;
      end;
    except
      LNodeValue := Null;
    end;

    case LXmlNode.NodeType of
      ntElement:
        if LXmlNode.HasChildNodes then
           if LXmlNode.ChildNodes.Count > 0 then
             LValueStr := Format('(%d items)', [LXmlNode.ChildNodes.Count])
           else
             LValueStr := ''
        else
           LValueStr := '';

      ntAttribute:
         LValueStr := VarToStrDef(LNodeValue, '');
      ntText, ntCData, ntComment:
         LValueStr := VarToStrDef(LNodeValue, '').Trim;
      ntProcessingInstr:
        LValueStr := VarToStrDef(LNodeValue, '');
      ntDocType, ntEntity, ntNotation:
        LValueStr := VarToStrDef(LNodeValue, '');
      ntDocument, ntEntityRef, ntDocFragment:
        LValueStr := '';
    else
      LValueStr := VarToStrDef(LNodeValue, '');
    end;
    pEventArgs.CellText := LValueStr;
  end;
  inherited DoGetText(pEventArgs);
end;

procedure TXmlTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  const Text: string);
var
  LTreeNode : TXmlNode;
  LXmlNode  : IXMLNode;
begin
  // Editing is currently disabled by commented-out options, but if enabled:
  LTreeNode := GetNode(Node);
  if not Assigned(LTreeNode) or not Assigned(LTreeNode.Data) then
    Exit;

  LXmlNode := LTreeNode.Data;

  // Only allow editing values in column 1 for specific node types
  if Column = 1 then
  begin
    case LXmlNode.NodeType of
      ntAttribute:
        LXmlNode.NodeValue := Text;
      ntText, ntCData, ntComment:
        LXmlNode.NodeValue := Text;
      ntProcessingInstr:
        LXmlNode.NodeValue := Text; // Edit PI data
    else
      Exit; // Don't allow editing value for other types like elements
    end;
    InvalidateNode(Node); // Refresh the node visually
  end
  else if Column = 0 then // Editing Name column
  begin
     // Generally, editing node names (elements, attributes) is complex
     // as it affects structure and might require DOM manipulation beyond simple assignment.
     // For now, disallow editing names.
     // If needed, implement renaming logic carefully.
     // Example (use with caution):
     // case LXmlNode.NodeType of
     //   ntAttribute: LXmlNode.NodeName := Text; // Requires care with DOM owner document
     //   ntElement: // Renaming elements is very tricky, often requires recreating node
     // else Exit;
     // end;
     Exit; // Disallow name editing for now
     // InvalidateNode(Node);
  end;

  inherited DoNewText(Node, Column, Text); // Call inherited if needed
end;

procedure TXmlTree.DoNodeDblClick(const HitInfo: THitInfo);
var
  LNode : TXmlNode;
begin
  // Use the built-in VST toggle mechanism if toToggleOnDblClick is set
  // LNode := GetNode(HitInfo.HitNode);
  // if Assigned(LNode) then
  //   LNode.Expanded := not LNode.Expanded;
  inherited DoNodeDblClick(HitInfo); // This will handle expand/collapse if option is set
end;

procedure TXmlTree.DoInitNode(Parent: PVirtualNode; ANode: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  LTreeNode           : TXmlNode;
  LXmlNode            : IXMLNode;
  LHasVisibleChildren : Boolean;
  I                   : Integer;
  LChildXmlNode       : IXMLNode;
begin
  inherited DoInitNode(Parent, ANode, InitStates);
  Include(InitStates, ivsMultiline); // Enable multiline support

  LTreeNode := GetNode(ANode);

  // Default to NOT showing an expand button
  Exclude(InitStates, ivsHasChildren);

  if Assigned(LTreeNode) and Assigned(LTreeNode.Data) then
  begin
    LXmlNode            := LTreeNode.Data;

     // If this node represents an XML attribute, it NEVER gets an expand button.
    if LXmlNode.NodeType = ntAttribute then
      Exit; // Stop processing for attributes, button remains hidden.

    LHasVisibleChildren := False; // Now, check for children/attributes for non-attribute nodes

    // Check if the node has attributes (Only relevant for elements now)
    if (LXmlNode.NodeType = ntElement) and Assigned(LXmlNode.AttributeNodes) and
       (LXmlNode.AttributeNodes.Count > 0) then
    begin
      LHasVisibleChildren := True;
    end;

    // Check if the node has actual child nodes (skipping whitespace)
    if not LHasVisibleChildren and Assigned(LXmlNode.ChildNodes) then
    begin
      for I := 0 to LXmlNode.ChildNodes.Count - 1 do
      begin
        LChildXmlNode := LXmlNode.ChildNodes[I];
        // Check if the child is NOT a whitespace-only text node
        if not IsWhitespaceNode(LChildXmlNode) then
        begin
          LHasVisibleChildren := True; // Found a significant child
          Break; // No need to check further children
        end;
      end;
      // --- End of actual loop logic ---
    end;

    // Set the HasChildren flag ONLY IF needed (and it's not an attribute)
    if LHasVisibleChildren then
      Include(InitStates, ivsHasChildren);
   // else: it remains excluded from the start
  end;
 // else: Node has no data, button remains hidden
end;

procedure TXmlTree.DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode;
  var NodeHeight: Integer);
var
  LHeight       : Integer;
  LMaxHeight    : Integer;
  S             : UnicodeString; // Use UnicodeString
  LNode         : TXmlNode;
  LXmlNode      : IXMLNode;
  R             : TRect;
  LDrawFormat   : Cardinal;
  LCol          : TVirtualTreeColumn;
  LBitmap       : TBitmap;
  LCanvas       : TCanvas;
  LOriginalFont : TFont;
  LValueColIdx  : TColumnIndex;
  LNodeValue    : OleVariant; // Variable to hold node value
  LSettings     : TTextFormatSettings;
begin
  LMaxHeight := DefaultNodeHeight; // Start with default height

  if not Assigned(Node) then
  begin
    NodeHeight := LMaxHeight;
    Exit;
  end;

  LNode := GetNode(Node);
  if not Assigned(LNode) or not Assigned(LNode.Data) then
  begin
    NodeHeight := LMaxHeight;
    Exit;
  end;
  LXmlNode := LNode.Data;

  LValueColIdx := -1;
  if Header.Columns.Count > 1 then
     LValueColIdx := 1;

  S := '';
  if LValueColIdx <> -1 then
  begin
    // Only calculate value string if needed for measurement (text/cdata/comment nodes mostly)
    case LXmlNode.NodeType of
      ntText, ntCData, ntComment: // These are most likely to wrap
        begin
          try
            LNodeValue := LXmlNode.NodeValue;
          except
            LNodeValue := Null; // Handle potential exceptions
          end;
          S := VarToStrDef(LNodeValue, '').Trim;
        end;
      // Add other types here ONLY if their value column text can wrap and affect height.
      // For example, attribute values are usually short, but if they *can* be long and wrap:
      // ntAttribute: S := VarToStrDef(LXmlNode.NodeValue, '');
      // Avoid calculating for element structure text like "(x items)" as it usually doesn't wrap significantly
    end;
  end;

  if (S <> '') and (LValueColIdx <> -1) then
  begin
    LCol := Header.Columns.Items[LValueColIdx]; // Get the value column object
    if Assigned(LCol) and (coVisible in LCol.Options) and (LCol.Width > TextMargin * 2) then
    begin
      LBitmap := Vcl.Graphics.TBitmap.Create;
      try
        LCanvas := LBitmap.Canvas;
        LOriginalFont := TFont.Create;
        try
          LOriginalFont.Assign(TargetCanvas.Font); // Save original font
          LCanvas.Font.Assign(LOriginalFont);  // Start with base font

          LSettings := nil;
          // Replicate the logic from DoPaintText specifically for Column = LValueColIdx
          case LXmlNode.NodeType of
            ntAttribute, ntText, ntCData, ntComment, ntProcessingInstr:
              LSettings := ColorSettings.Value;
            ntElement: // Value col for element shows structure or text content
              if LXmlNode.IsTextElement then
                LSettings := ColorSettings.Value
              else
                LSettings := ColorSettings.Other; // Font for "(x items)"
            // Add cases for other node types as needed based on DoPaintText's logic for col 1
            else
              LSettings := ColorSettings.Other; // Fallback
          end;

          if Assigned(LSettings) then
             LCanvas.Font.Assign(LSettings.Font);
          // --- End Apply font style ---

          // Prepare rect and flags for DrawText calculation
          R := Rect(0, 0, LCol.Width - (TextMargin * 2), 32767); // Max height
          LDrawFormat := DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX or
                         DT_EDITCONTROL or DT_LEFT or DT_TOP;

          // Calculate required height
          LHeight := Winapi.Windows.DrawText(LCanvas.Handle, PChar(S), Length(S), R, LDrawFormat);

          // Add some padding (adjust as needed)
          LHeight := LHeight + 4;

          // Update the maximum height found so far
          LMaxHeight := Max(LMaxHeight, LHeight);

        finally
          LOriginalFont.Free;
        end;
      finally
        LBitmap.Free;
      end;
    end; // if Assigned(LCol) and Width > Margin...
  end; // if S <> ''

  // Ensure minimum height is DefaultNodeHeight and assign to output
  NodeHeight := Max(LMaxHeight, DefaultNodeHeight);
end;

procedure TXmlTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas;
  Column: TColumnIndex; TextType: TVSTTextType);
var
  LTreeNode  : TXmlNode;
  LXmlNode   : IXMLNode;
  LSettings  : TTextFormatSettings;
begin
  // Apply font settings based on node type and column
  if TextType = ttNormal then
  begin
    LTreeNode := GetNode(Node);
    if not Assigned(LTreeNode) or not Assigned(LTreeNode.Data) then
    begin
      inherited DoPaintText(Node, Canvas, Column, TextType); // Fallback
      Exit;
    end;
    LXmlNode := LTreeNode.Data;

    // Determine base settings based on node type
    case LXmlNode.NodeType of
      ntElement:
        LSettings := ColorSettings.Element;
      ntAttribute:
        LSettings := ColorSettings.Attribute;
      ntText:
        LSettings := ColorSettings.TextNode;
      ntCData:
        LSettings := ColorSettings.CData;
      ntProcessingInstr:
        LSettings := ColorSettings.ProcessingInstruction;
      ntComment:
        LSettings := ColorSettings.Comment;
      ntDocument:
        LSettings := ColorSettings.Document;
      ntDocType:
        LSettings := ColorSettings.DocType;
      ntEntityRef:
        LSettings := ColorSettings.EntityRef;
      ntEntity:
        LSettings := ColorSettings.Entity;
      ntDocFragment:
        LSettings := ColorSettings.DocFragment;
      ntNotation:
        LSettings := ColorSettings.Notation;
    else
      LSettings := ColorSettings.Other;
    end;

    // Override for Value column
    if Column = 1 then
    begin
      case LXmlNode.NodeType of
        ntAttribute, ntText, ntCData, ntComment, ntProcessingInstr:
          LSettings := ColorSettings.Value; // Use Value style for these in col 1
        ntElement:
          if LXmlNode.IsTextElement then
             LSettings := ColorSettings.Value // Text content of element uses Value style
          else if LXmlNode.HasChildNodes then
             LSettings := ColorSettings.Other // Use 'Other' (e.g., gray) for item count display
          else
             LSettings := ColorSettings.Element; // Keep element style if empty
        // Other node types keep their base style in col 1
      end;
    end;

    // Apply the determined font settings
    Canvas.Font.Assign(LSettings.Font);
  end;

  // Let the default VST painting occur (draws the actual text)
  inherited DoPaintText(Node, Canvas, Column, TextType);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TXmlTree.GetFocusedXmlNode: TXmlNode;
begin
  Result := GetNode(FocusedNode);
end;

function TXmlTree.GetFocusedValue: string;
var
  LTreeNode    : TXmlNode;
  LXmlNode     : IXMLNode;
  LNodeNameStr : string;
begin
  Result := '';
  LTreeNode := GetNode(FocusedNode); // Use the GetNode helper
  if Assigned(LTreeNode) and Assigned(LTreeNode.Data) then
  begin
    LXmlNode := LTreeNode.Data;

    // Get the base name representation (consistent with DoGetText)
    LNodeNameStr := GetNodeTypeName(LXmlNode.NodeType);
    case LXmlNode.NodeType of
      ntElement, ntAttribute, ntProcessingInstr, ntEntity, ntNotation:
        LNodeNameStr := LXmlNode.NodeName;
      ntDocType:
        LNodeNameStr := LNodeNameStr + ' ' + LXmlNode.NodeName;
      ntEntityRef:
         if LXmlNode.NodeName <> '' then
           LNodeNameStr := '&' + LXmlNode.NodeName + ';'
         else
           LNodeNameStr := '&;';
    end;

    if FocusedColumn = 0 then // Name Column
    begin
      Result := LNodeNameStr;
    end
    else if FocusedColumn = 1 then // Value Column
    begin
      case LXmlNode.NodeType of
        ntElement:
          if LXmlNode.IsTextElement then
            Result := LXmlNode.Text
          else if LXmlNode.HasChildNodes then
            Result := Format('(%d items)', [LXmlNode.ChildNodes.Count])
          else
            Result := '';
        ntAttribute:
          Result := VarToStrDef(LXmlNode.NodeValue, '');
        ntText, ntCData, ntComment:
          Result := VarToStrDef(LXmlNode.NodeValue, LXmlNode.Text).Trim;
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

function TXmlTree.GetXmlString: string;
begin
  if Assigned(FXmlDocument) then
    Result := FXmlDocument.XML.Text // Or FXmlDocument.SaveToXML() for potentially better formatting control
  else
    Result := '';
end;

procedure TXmlTree.SetXmlString(const Value: string);
begin
  // Optimization: Check if the new value is actually different
  if Assigned(FXmlDocument) and (FXmlDocument.XML.Text = Value) then
    Exit;
  if not Assigned(FXmlDocument) and (Trim(Value) = '') then
    Exit;

  BeginUpdate;
  try
    Clear; // Clear existing nodes, triggers DoFreeNode
    FXmlDocument := nil; // Release previous document interface

    if Trim(Value) <> '' then
    begin
      try
        FXmlDocument := LoadXMLData(Value);
        FXmlDocument.Options := FXmlDocument.Options + [doNodeAutoIndent];
      except
        on E: Exception do
        begin
          Logger.SendException('Failed to load XML data', E);
          FXmlDocument := nil; // Ensure it's nil on error
        end;
      end;
    end;

    // Rebuild the tree only if loading was successful
    if Assigned(FXmlDocument) then
      BuildTree;

  finally
    EndUpdate;
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
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
      Result := '!DOCTYPE'; // NodeName will be appended later in DoGetText
    ntEntityRef:
      Result := ''; // Special handling in DoGetText ('&...;')
    ntEntity:
      Result := '#entity';
    ntDocFragment:
      Result := '#document-fragment';
    ntNotation:
      Result := '#notation';
    // Let DoGetText use NodeName for these:
    ntProcessingInstr:
      Result := '?'; // Prefix for PI Name
    ntAttribute:
      Result := ''; // Use NodeName directly
    ntElement:
      Result := ''; // Use NodeName directly
  else
    Result := '#unknown';
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
    // Consider a node whitespace if it's empty or contains only whitespace chars
    if S = '' then
    begin
      Result := True;
    end
    else
    begin
      Result := True; // Assume whitespace until proven otherwise
      for C in S do
      begin
        if not C.IsWhiteSpace then
        begin
          Result := False;
          Break; // Found non-whitespace, stop checking
        end;
      end;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TXmlTree.BuildTree;
var
  LRootXmlNode  : IXMLNode;
  LRootTreeNode : TXmlNode;
begin
  BeginUpdate;
  try
    Clear; // Make sure tree is empty before building
    if Assigned(FXmlDocument) then
    begin
      // You might want to show the document node itself, or just the root element
      // Option 1: Show Document node (useful for PIs/Comments outside root element)
      // RootXmlNode := FXmlDocument;
      // Option 2: Show Root Element (most common)
      LRootXmlNode := FXmlDocument.DocumentElement;

      if Assigned(LRootXmlNode) then
      begin
        // Create the TXmlNode for the root. It has no TXmlNode parent.
        // The Create constructor handles adding the PVirtualNode if parent VNode is nil.
        LRootTreeNode := TXmlNode.Create(Self, LRootXmlNode, False, nil); // nil parent VNode signals root

        // Now, process the children and attributes of this root XML node,
        // adding them *under* the RootTreeNode
        ProcessXmlNodeChildren(LRootTreeNode, LRootXmlNode);
      end;
    end;

    FullExpand; // Expand all nodes

    Header.AutoFitColumns;
  finally
    EndUpdate;
  end;
end;

// Processes Attributes and Child Nodes OF AXmlNode, adding them AS CHILDREN of AParentTreeNode
procedure TXmlTree.ProcessXmlNodeChildren(AParentTreeNode: TXmlNode; AXmlNode: IXMLNode);
var
  LAttributeTreeNode : TXmlNode; // TXmlNode for the attribute
  LChildTreeNode     : TXmlNode; // TXmlNode for the child element/text/etc.
  LChildXmlNode      : IXMLNode; // The actual XML child node interface
  i                  : Integer;
begin
  // --- Add Attributes as children in the tree ---
  // Attributes are added *before* child nodes for typical display order
  if (AXmlNode.NodeType = ntElement) and Assigned(AXmlNode.AttributeNodes) then
  begin
    for i := 0 to AXmlNode.AttributeNodes.Count - 1 do
    begin
      // Use the Add method of the PARENT TXmlNode (AParentTreeNode)
      // This creates the child TXmlNode AND adds it to the VST hierarchy
      LAttributeTreeNode := AParentTreeNode.Add(AXmlNode.AttributeNodes[i], False);
      // Attributes don't have children themselves in XML, so no recursive call needed for them.
    end;
  end;

  // --- Add Child Nodes recursively ---
  if Assigned(AXmlNode.ChildNodes) then
  begin
    for i := 0 to AXmlNode.ChildNodes.Count - 1 do
    begin
      LChildXmlNode := AXmlNode.ChildNodes[i];

      // Skip insignificant whitespace text nodes
      if IsWhitespaceNode(LChildXmlNode) then
        Continue;

      // Use the Add method of the PARENT TXmlNode (AParentTreeNode)
      LChildTreeNode := AParentTreeNode.Add(LChildXmlNode, False);

      // Recursively process the children/attributes of THIS child node,
      // adding them under the newly created LChildTreeNode
      ProcessXmlNodeChildren(LChildTreeNode, LChildXmlNode);
    end;
  end;
end;

function TXmlTree.GetNode(const AVNode: PVirtualNode): TXmlNode;
begin
  // Retrieve the TXmlNode pointer stored in the node data
  if Assigned(AVNode) then
    Result := GetNodeData<TXmlNode>(AVNode) // Use the generic version
  else
    Result := nil;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TXmlTree.FColorSettingsChanged(Sender: TObject);
begin
  Invalidate; // Redraw the tree when color settings change
end;
{$ENDREGION}
{$ENDREGION}

end.
