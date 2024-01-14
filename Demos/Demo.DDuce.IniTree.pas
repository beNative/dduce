{
  Copyright (C) 2013-2024 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Demo.DDuce.IniTree;

interface

uses
  System.SysUtils, System.Classes, System.Actions, System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ImgList, Vcl.Menus,

  VirtualTrees, VirtualTrees.Types, VirtualTrees.Header, VirtualTrees.BaseTree,

  zObjInspector, zObjInspTypes,

  DDuce.Components.VirtualTrees.Node, DDuce.Components.SectionTree,
  DDuce.Editor.Interfaces, DDuce.Components.IniTree;

type
  TfrmIniTree = class(TForm)
    {$REGION 'designer controls'}
    aclMain            : TActionList;
    actCollapse        : TAction;
    actExpand          : TAction;
    actParseDocument   : TAction;
    btnCollapse        : TButton;
    btnExpand          : TButton;
    btnParseDocument   : TButton;
    mmoJson            : TMemo;
    pnlEditor          : TPanel;
    pnlMain            : TPanel;
    pnlObjectInspector : TPanel;
    pnlTop             : TPanel;
    pnlTree            : TPanel;
    splVertical        : TSplitter;
    imlMain            : TImageList;
    ppmTree: TPopupMenu;
    mniCopy: TMenuItem;
    actCopy: TAction;
    {$ENDREGION}

    {$REGION 'event handlers'}
    function FObjectInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;

    procedure FTreeDblClick(Sender: TObject);
    procedure FTreeExpandedCollapsed(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actExpandExecute(Sender: TObject);
    procedure actCollapseExecute(Sender: TObject);
    procedure actParseDocumentExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    {$ENDREGION}

  private
    FTree            : TIniTree;
    FObjectInspector : TzObjectInspector;
    FSettings        : IEditorSettings;
    FEditor          : IEditorView;
    FManager         : IEditorManager;

    procedure InitializeTree;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  System.Rtti, System.StrUtils,
  Vcl.Clipbrd,

  DDuce.Components.Factories, DDuce.Factories.VirtualTrees,
  DDuce.Factories.zObjInspector, DDuce.Editor.Factories,
  DDuce.Logger.Factories, DDuce.Logger.Channels.Winipc, DDuce.Logger;

const
  VISIBLE_PROPERTIES : array of string = [
    'Color',
    'Colors',
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

{$REGION 'construction and destruction'}
procedure TfrmIniTree.AfterConstruction;
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
  FEditor.Editor.Highlighter.Colors.LoadFromFile('settings.texteditor.json');
  FEditor.HighlighterName := 'INI';
  FEditor.Text := mmoJson.Lines.Text;
end;
{$ENDREGION}

{$REGION 'event handlers'}
function TfrmIniTree.FObjectInspectorBeforeAddItem(Sender: TControl;
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

procedure TfrmIniTree.FTreeDblClick(Sender: TObject);
begin
  FTree.Header.AutoFitColumns;
end;

procedure TfrmIniTree.FTreeExpandedCollapsed(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  FTree.Header.AutoFitColumns;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmIniTree.actCollapseExecute(Sender: TObject);
begin
  FTree.FullCollapse;
  FTree.Header.AutoFitColumns;
end;

procedure TfrmIniTree.actCopyExecute(Sender: TObject);
begin
  Clipboard.AsText := FTree.FocusedValue;
end;

procedure TfrmIniTree.actExpandExecute(Sender: TObject);
begin
  FTree.FullExpand;
  FTree.Header.AutoFitColumns;
end;

procedure TfrmIniTree.actParseDocumentExecute(Sender: TObject);
begin
  FTree.IniString:= FEditor.Text;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmIniTree.InitializeTree;
begin
  FTree := TIniTree.Create(Self);
  FTree.Parent               := pnlTree;
  FTree.BorderStyle          := bsNone;
  FTree.Align                := alClient;
  FTree.Font.Name            := 'Consolas';
  FTree.Font.Size            := 10;
  FTree.PopupMenu            := ppmTree;
  FTree.Colors.GridLineColor := clBtnFace;
  FTree.OnCollapsed          := FTreeExpandedCollapsed;
  FTree.OnExpanded           := FTreeExpandedCollapsed;
  FTree.OnDblClick           := FTreeDblClick;
end;
{$ENDREGION}

end.
