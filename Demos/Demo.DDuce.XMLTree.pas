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

  DDuce.Components.PropertyInspector, DDuce.Editor.Interfaces,
  DDuce.Components.XmlTree, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList;

type
  TfrmXMLTree = class(TForm)
    {$REGION 'designer controls'}
    aclMain            : TActionList;
    actCollapse        : TAction;
    actExpand          : TAction;
    btnCollapse        : TButton;
    btnExpand          : TButton;
    pnlEditor          : TPanel;
    pnlObjectInspector : TPanel;
    pnlMain            : TPanel;
    pnlTop             : TPanel;
    pnlTree            : TPanel;
    splVertical        : TSplitter;
    actParseDocument   : TAction;
    imlMain            : TVirtualImageList;
    btnParseDocument   : TButton;
    {$ENDREGION}

    procedure actExpandExecute(Sender: TObject);
    procedure actCollapseExecute(Sender: TObject);
    procedure actParseDocumentExecute(Sender: TObject);

  private
    FTree            : TXmlTree;
    FObjectInspector : TzObjectInspector;
    FSettings        : IEditorSettings;
    FEditor          : IEditorView;
    FManager         : IEditorManager;

    procedure InitializeTree;

    function FObjectInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;

    procedure FTreeExpandedCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  System.Rtti, System.StrUtils, System.IOUtils,

  DDuce.Components.Factories, DDuce.Factories.zObjInspector,
  DDuce.Editor.Factories,

  Demo.Resources;

{$REGION 'construction and destruction'}
procedure TfrmXMLTree.AfterConstruction;
begin
  inherited AfterConstruction;
  InitializeTree;
  FObjectInspector := TzObjectInspectorFactory.Create(
    Self,
    pnlObjectInspector
  );
  FObjectInspector.AlignWithMargins       := True;
  FObjectInspector.ShowReadOnlyProperties := False;
  FObjectInspector.OnBeforeAddItem        := FObjectInspectorBeforeAddItem;
  FObjectInspector.Component              := FTree;

  FSettings := TEditorFactories.CreateSettings(Self);
  FManager  := TEditorFactories.CreateManager(Self, FSettings);
  FEditor   := TEditorFactories.CreateView(pnlEditor, FManager);
  if TFile.Exists(TEXTEDITOR_SETTINGS_FILE) then
    FEditor.Editor.Highlighter.Colors.LoadFromFile(TEXTEDITOR_SETTINGS_FILE);
  FEditor.HighlighterName := 'XML';
  FEditor.Text := EXAMPLE_XML_DOCUMENT;
  FTree.XmlString := FEditor.Text;
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
  FTree.Header.AutoFitColumns;
end;

procedure TfrmXMLTree.actParseDocumentExecute(Sender: TObject);
begin
  FTree.XmlString := FEditor.Text;
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
    and MatchText(LName, VT_VISIBLE_PROPERTIES);
end;

procedure TfrmXMLTree.FTreeExpandedCollapsed(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  FTree.Header.AutoFitColumns;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmXMLTree.InitializeTree;
begin
  FTree                      := TXmlTree.Create(Self);
  FTree.Parent               := pnlTree;
  FTree.BorderStyle          := bsNone;
  FTree.Align                := alClient;
  FTree.Font.Name            := 'Consolas';
  FTree.Font.Size            := 10;
  FTree.Colors.GridLineColor := clBtnFace;
  //FTree.PopupMenu            := ppmTree;

  //FTree.TreeOptions.MiscOptions := [
    //toAcceptOLEDrop,
    //toFullRepaintOnResize,
    //toGridExtensions,
//    toInitOnSave,
    //toToggleOnDblClick,
    //toWheelPanning
  //];
  FTree.Header.AutoFitColumns;
  FTree.OnCollapsed          := FTreeExpandedCollapsed;
  FTree.OnExpanded           := FTreeExpandedCollapsed;
end;
{$ENDREGION}

end.
