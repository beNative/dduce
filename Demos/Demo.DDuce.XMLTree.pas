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

unit Demo.DDuce.XmlTree;

{ Form demonstrating the TXmlTree component which is a TVirtualStringTree
  descendant. }

interface

uses
  System.SysUtils, System.Classes, System.Actions,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.ActnList,
  Vcl.StdCtrls,

  VirtualTrees, VirtualTrees.Types, VirtualTrees.Header,

  zObjInspector, zObjInspTypes,

  DDuce.Components.PropertyInspector, DDuce.Components.XmlTree;

type
  TfrmXMLTree = class(TForm)
    aclMain            : TActionList;
    actCollapse        : TAction;
    actExpand          : TAction;
    btnCollapse        : TButton;
    btnExpand          : TButton;
    mmoXml             : TMemo;
    pnlEditor          : TPanel;
    pnlObjectInspector : TPanel;
    pnlMain            : TPanel;
    pnlTop             : TPanel;
    pnlXMLTree         : TPanel;
    splVertical        : TSplitter;

    procedure actExpandExecute(Sender: TObject);
    procedure actCollapseExecute(Sender: TObject);
    procedure mmoXmlChange(Sender: TObject);

  private
    FTree            : TXmlTree;
    FXml             : string;
    FObjectInspector : TzObjectInspector;

    procedure InitializeTree;

    function FObjectInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;

    procedure XMLTreeEditing(
      Sender      : TBaseVirtualTree;
      Node        : PVirtualNode;
      Column      : TColumnIndex;
      var Allowed : Boolean
    );
    procedure XMLTreePaintText(
      Sender             : TBaseVirtualTree;
      const TargetCanvas : TCanvas;
      Node               : PVirtualNode;
      Column             : TColumnIndex;
      TextType           : TVSTTextType
    );
    procedure XMLTreeColumnDblClick(
      Sender : TBaseVirtualTree;
      Column : TColumnIndex;
      Shift  : TShiftState
    );
    procedure XMLTreeEdited(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    );

  protected
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  System.Rtti, System.StrUtils,

  DDuce.Components.Factories, DDuce.Factories.zObjInspector;

const
  VISIBLE_PROPERTIES : array of string = [
    'Color',
    'Colors',
    'ColorSettings',
    'DefaultNodeHeight',
    'DefaultText',
    'DragImageKind',
    'DragKind',
    'DragMode',
    'DragOperations',
    'DragType',
    'DragWidth',
    'DrawSelectionMode',
    'EmptyListMessage',
    'Enabled',
    'Font',
    'Header',
    'Hint',
    'HintMode',
    'Indent',
    'LineMode',
    'LineStyle',
    'Margin',
    'NodeAlignment',
    'ShowHint',
    'TextMargin',
    'TreeOptions',
    'Visible'
  ];


{$REGION 'XML string'}
const
   //example XML document to show in the XML-viewer
  XML_STRING =
    '<?xml version="1.0" encoding="utf-8"?>'                                                                                                                                                              + #13#10 +
    '<document version="3.0">'                                                                                                                                                                            + #13#10 +
    '  <patternheader>'                                                                                                                                                                                   + #13#10 +
    '    <upid value="{2004-11-30-7-31-41-777-16959}"/>'                                                                                                                                                  + #13#10 +
    '    <name value="$name"/>'                                                                                                                                                                           + #13#10 +
    '    <description value="$description"/>'                                                                                                                                                             + #13#10 +
    '    <goal value="SourceCode"/>'                                                                                                                                                                      + #13#10 +
    '    <category value="Class"/>'                                                                                                                                                                       + #13#10 +
    '    <language value="delphi"/>'                                                                                                                                                                      + #13#10 +
    '    <default value="n"/>'                                                                                                                                                                            + #13#10 +
    '    <diagrams value="Package Diagram,Class Diagram"/>'                                                                                                                                               + #13#10 +
    '    <containerMetaclasses value="Project,Package"/>'                                                                                                                                                 + #13#10 +
    '  </patternheader>'                                                                                                                                                                                  + #13#10 +
    '  <templet>'                                                                                                                                                                                         + #13#10 +
    '    <input>'                                                                                                                                                                                         + #13#10 +
    '      <parameter name="Visitor" type="string" valid="InterfaceName" description="$Visitor.description" displayname="$Visitor.displayname" defaultvalue="IVisitor"/>'                                 + #13#10 +
    '      <parameter name="ConcreteVisitor" type="string" valid="ClassName" description="$ConcreteVisitor.description" displayname="$ConcreteVisitor.displayname" defaultvalue="TConcreteVisitor"/>'     + #13#10 +
    '      <parameter name="Element" type="string" valid="InterfaceName" description="$Element.description" displayname="$Element.displayname" defaultvalue="IElement"/>'                                 + #13#10 +
    '      <parameter name="ConcreteElement" type="string" valid="{ClassName}1n" description="$ConcreteElement.description" displayname="$ConcreteElement.displayname" defaultvalue="TConcreteElement"/>' + #13#10 +
    '    </input>' + #13#10 +
    '    <output>' + #13#10 +
    '      <parameter name="Result_ConcreteElement" type="element"/>' + #13#10 +
    '      <parameter name="Result_ConcreteVisitor" type="element"/>' + #13#10 +
    '      <parameter name="Result_Element" type="element"/>' + #13#10 +
    '      <parameter name="Result_Visitor" type="element"/>' + #13#10 +
    '      <parameter name="Result_FirstClassCitizen" type="element"/>' + #13#10 +
    '    </output>' + #13#10 +
    '    <locals>' + #13#10 +
    '      <local name="CurrentConcreteElement" type="string"/>' + #13#10 +
    '    </locals>' + #13#10 +
    '    <body>' + #13#10 +
    '      <stmt action="create" number="30" useExistent="true">' + #13#10 +
    '        <in name="parent" value="#Container"/>' + #13#10 +
    '        <in name="shapetype" value="Class"/>' + #13#10 +
    '        <in name="visibility" value="public"/>' + #13#10 +
    '        <in name="name" value="@ConcreteVisitor"/>' + #13#10 +
    '        <in name="positions" value="0,120,166,79"/>' + #13#10 +
    '        <out name="Result" value="@Result_ConcreteVisitor"/>' + #13#10 +
    '      </stmt>' + #13#10 +
    '      <stmt action="create" number="50" useExistent="true">' + #13#10 +
    '        <in name="parent" value="#Container"/>' + #13#10 +
    '        <in name="shapetype" value="Interface"/>' + #13#10 +
    '        <in name="name" value="@Element"/>' + #13#10 +
    '        <in name="visibility" value="public"/>' + #13#10 +
    '        <in name="positions" value="40,240,86,78"/>' + #13#10 +
    '        <out name="Result" value="@Result_Element"/>' + #13#10 +
    '      </stmt>' + #13#10 +
    '      <stmt action="create" number="70" useExistent="true">' + #13#10 +
    '        <in name="parent" value="#Container"/>' + #13#10 +
    '        <in name="shapetype" value="Interface"/>' + #13#10 +
    '        <in name="name" value="@Visitor"/>' + #13#10 +
    '        <in name="visibility" value="public"/>' + #13#10 +
    '        <in name="positions" value="0,0,166,78"/>' + #13#10 +
    '        <out name="Result" value="@Result_Visitor"/>' + #13#10 +
    '      </stmt>' + #13#10 +
    '      <stmt action="create" number="60" useExistent="true">' + #13#10 +
    '        <in name="parent" value="@Result_Element"/>' + #13#10 +
    '        <in name="shapetype" value="Operation"/>' + #13#10 +
    '        <in name="params" value="AVisitor :$@Visitor$"/>' + #13#10 +
    '        <in name="name" value="Accept"/>' + #13#10 +
    '        <in name="typeName" value=""/>' + #13#10 +
    '      </stmt>' + #13#10 +
    '      <stmt action="create" number="100" useExistent="true">' + #13#10 +
    '        <in name="parent" value="@Result_ConcreteVisitor"/>' + #13#10 +
    '        <in name="client" value="@Result_ConcreteVisitor"/>' + #13#10 +
    '        <in name="supplier" value="@Result_Visitor"/>' + #13#10 +
    '        <in name="shapetype" value="Implementation Link"/>' + #13#10 +
    '      </stmt>' + #13#10 +
    '      <stmt action="create" number="110">' + #13#10 +
    '        <in name="parent" value="#Container"/>' + #13#10 +
    '        <in name="shapetype" value="Pattern"/>' + #13#10 +
    '        <in name="name" value="Visitor"/>' + #13#10 +
    '        <in name="description" value="GoF Visitor Pattern as First Class Citizen"/>' + #13#10 +
    '        <in name="patternUPID" value="{2004-11-30-7-31-41-777-16959}"/>' + #13#10 +
    '        <in name="positions" value="220,20,298,133"/>' + #13#10 +
    '        <out name="Result" value="@Result_FirstClassCitizen"/>' + #13#10 +
    '      </stmt>' + #13#10 +
    '      <stmt action="create" number="130">' + #13#10 +
    '        <in name="parent" value="@Result_FirstClassCitizen"/>' + #13#10 +
    '        <in name="client" value="@Result_FirstClassCitizen"/>' + #13#10 +
    '        <in name="supplier" value="@Result_ConcreteVisitor"/>' + #13#10 +
    '        <in name="shapetype" value="Pattern Link"/>' + #13#10 +
    '        <in name="supplierRole" value="_ConcreteVisitor"/>' + #13#10 +
    '      </stmt>' + #13#10 +
    '      <stmt action="create" number="140">' + #13#10 +
    '        <in name="parent" value="@Result_FirstClassCitizen"/>' + #13#10 +
    '        <in name="client" value="@Result_FirstClassCitizen"/>' + #13#10 +
    '        <in name="supplier" value="@Result_Element"/>' + #13#10 +
    '        <in name="shapetype" value="Pattern Link"/>' + #13#10 +
    '        <in name="supplierRole" value="_Element"/>' + #13#10 +
    '      </stmt>' + #13#10 +
    '      <stmt action="create" number="150">' + #13#10 +
    '        <in name="parent" value="@Result_FirstClassCitizen"/>' + #13#10 +
    '        <in name="client" value="@Result_FirstClassCitizen"/>' + #13#10 +
    '        <in name="supplier" value="@Result_Visitor"/>' + #13#10 +
    '        <in name="shapetype" value="Pattern Link"/>' + #13#10 +
    '        <in name="supplierRole" value="_Visitor"/>' + #13#10 +
    '      </stmt>' + #13#10 +
    '      <stmt action="foreach" number="190">' + #13#10 +
    '        <in name="In" value="@ConcreteElement"/>' + #13#10 +
    '        <out name="Current" value="@CurrentConcreteElement"/>' + #13#10 +
    '      </stmt>' + #13#10 +
    '      <stmt action="create" number="200" useExistent="true">' + #13#10 +
    '        <in name="parent" value="#Container"/>' + #13#10 +
    '        <in name="shapetype" value="Class"/>' + #13#10 +
    '        <in name="visibility" value="public"/>' + #13#10 +
    '        <in name="name" value="@CurrentConcreteElement"/>' + #13#10 +
    '        <in name="positions" value="240,240,110,79"/>' + #13#10 +
    '        <out name="Result" value="@Result_ConcreteElement"/>' + #13#10 +
    '      </stmt>' + #13#10 +
    '      <stmt action="create" number="210" useExistent="true">' + #13#10 +
    '        <in name="parent" value="@Result_ConcreteElement"/>' + #13#10 +
    '        <in name="shapetype" value="Operation"/>' + #13#10 +
    '        <in name="params" value="AVisitor :$@Visitor$"/>' + #13#10 +
    '        <in name="name" value="Accept"/>' + #13#10 +
    '        <in name="visibility" value="public"/>' + #13#10 +
    '        <in name="typeName" value=""/>' + #13#10 +
    '        <in name="body">' + #13#10 +
    '          <value>begin' + #13#10 +
    '  AVisitor.Visit$@CurrentConcreteElement$(self);' + #13#10 +
    'end;' + #13#10 +
    '          </value>' + #13#10 +
    '        </in>' + #13#10 +
    '      </stmt>' + #13#10 +
    '      <stmt action="create" number="220" useExistent="true">' + #13#10 +
    '        <in name="parent" value="@Result_ConcreteVisitor"/>' + #13#10 +
    '        <in name="shapetype" value="Operation"/>' + #13#10 +
    '        <in name="params" value="AElement :$@CurrentConcreteElement$"/>' + #13#10 +
    '        <in name="name" value="Visit$@CurrentConcreteElement$"/>' + #13#10 +
    '        <in name="visibility" value="public"/>' + #13#10 +
    '        <in name="typeName" value=""/>' + #13#10 +
    '        <in name="body">' + #13#10 +
    '          <value>begin' + #13#10 +
    '  { provide implementation here }' + #13#10 +
    'end;' + #13#10 +
    '          </value>' + #13#10 +
    '        </in>' + #13#10 +
    '      </stmt>' + #13#10 +
    '      <stmt action="create" number="230" useExistent="true">' + #13#10 +
    '        <in name="parent" value="@Result_Visitor"/>' + #13#10 +
    '        <in name="shapetype" value="Operation"/>' + #13#10 +
    '        <in name="params" value="AElement :$@CurrentConcreteElement$"/>' + #13#10 +
    '        <in name="name" value="Visit$@CurrentConcreteElement$"/>' + #13#10 +
    '        <in name="typeName" value=""/>' + #13#10 +
    '      </stmt>' + #13#10 +
    '      <stmt action="create" number="240" useExistent="true">' + #13#10 +
    '        <in name="parent" value="@Result_ConcreteElement"/>' + #13#10 +
    '        <in name="client" value="@Result_ConcreteElement"/>' + #13#10 +
    '        <in name="supplier" value="@Result_Element"/>' + #13#10 +
    '        <in name="shapetype" value="Implementation Link"/>' + #13#10 +
    '      </stmt>' + #13#10 +
    '      <stmt action="create" number="250">' + #13#10 +
    '        <in name="parent" value="@Result_FirstClassCitizen"/>' + #13#10 +
    '        <in name="client" value="@Result_FirstClassCitizen"/>' + #13#10 +
    '        <in name="supplier" value="@Result_ConcreteElement"/>' + #13#10 +
    '        <in name="shapetype" value="Pattern Link"/>' + #13#10 +
    '        <in name="supplierRole" value="_ConcreteElement"/>' + #13#10 +
    '      </stmt>' + #13#10 +
    '      <stmt action="endforeach" number="500"/>' + #13#10 +
    '    </body>' + #13#10 +
    '  </templet>' + #13#10 +
    '</document>';
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TfrmXMLTree.AfterConstruction;
begin
  inherited;
  FTree := TXmlTree.Create(Self);
  FTree.Parent := pnlXMLTree;
  FXml := XML_STRING;
  mmoXml.Text := XML_STRING;
  InitializeTree;
  FObjectInspector := TzObjectInspectorFactory.Create(
    Self,
    pnlObjectInspector
  );
  FObjectInspector.AlignWithMargins       := True;
  FObjectInspector.ShowReadOnlyProperties := False;
  FObjectInspector.OnBeforeAddItem        := FObjectInspectorBeforeAddItem;
  FObjectInspector.Component              := FTree;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmXMLTree.actCollapseExecute(Sender: TObject);
begin
  FTree.FullCollapse;
end;

procedure TfrmXMLTree.actExpandExecute(Sender: TObject);
begin
  FTree.FullExpand;
end;
{$ENDREGION}

{$REGION 'event handlers'}
function TfrmXMLTree.FObjectInspectorBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
var
  LName : string;
begin
  LName := PItem.QualifiedName;
  LName := LName.Split(['.'], 2)[1];
  Result := not LName.Contains('ComObject')
    and (not (PItem.Prop.PropertyType is TRttiMethodType))
    and MatchText(LName, VISIBLE_PROPERTIES);
end;

procedure TfrmXMLTree.XMLTreeColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
var
  N : PVirtualNode;
begin
  N := Sender.GetFirstSelected;
  if Assigned(N) then
    Sender.EditNode(N, Column);
end;

procedure TfrmXMLTree.XMLTreeEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  FXml := FTree.XmlString;
end;

procedure TfrmXMLTree.XMLTreeEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := True;
end;

procedure TfrmXMLTree.XMLTreePaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if (Node = Sender.FocusedNode) and (Column = Sender.FocusedColumn)  then
  begin
    TargetCanvas.Font.Color := clWhite;
  end;
end;

procedure TfrmXMLTree.mmoXmlChange(Sender: TObject);
begin
  FXml := mmoXML.Text;
  FTree.XmlString := FXml;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmXMLTree.InitializeTree;
begin
  FTree.Parent           := pnlXMLTree;
  FTree.BevelKind        := bkFlat;
  FTree.BevelInner       := bvNone;
  FTree.BevelOuter       := bvLowered;
  FTree.BorderStyle      := bsNone;

  FTree.Align            := alClient;
  FTree.AlignWithMargins := True;
  FTree.Header.Options   := FTree.Header.Options + [hoVisible];
  with FTree.Header do
  begin
    Style := hsPlates;
    AutoSizeIndex := 0;
    Options := Options + [hoAutoResize, hoVisible];
  end;
//  FTree.OnEditing        := XMLTreeEditing;
//  FTree.OnPaintText      := XMLTreePaintText;
//  FTree.OnColumnDblClick := XMLTreeColumnDblClick;
//  FTree.OnEdited         := XMLTreeEdited;
  FTree.LineMode := lmBands;
  FTree.Header.Options := [
    hoAutoResize,
    hoColumnResize,
    hoDrag,
    hoShowSortGlyphs,
    hoVisible
  ];
  FTree.TreeOptions.MiscOptions := [
    toAcceptOLEDrop,
    toEditable,
    toFullRepaintOnResize,
    toGridExtensions,
    toInitOnSave,
    toToggleOnDblClick,
    toWheelPanning
  ];
//  FTree.TreeOptions.PaintOptions := [
//    toShowButtons,
//    toShowDropmark,
//    toShowHorzGridLines,
//    toShowRoot,
//    toShowTreeLines,
//    toShowVertGridLines,
//    toUseBlendedImages,
//    toUseBlendedSelection
//  ];
//  FTree.TreeOptions.AutoOptions := [
//    toAutoSpanColumns
//  ];
  FTree.XmlString := FXml;
  FTree.Colors.GridLineColor := clSilver;
  FTree.Colors.UnfocusedSelectionColor := clGray;
  FTree.DrawSelectionMode := smBlendedRectangle;
  FTree.ButtonStyle := bsTriangle;
  FTree.Header.AutoFitColumns;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmXMLTree.UpdateActions;
begin
  inherited;
  mmoXML.Text := FXml;
end;
{$ENDREGION}

end.
