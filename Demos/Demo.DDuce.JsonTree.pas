{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Demo.DDuce.JsonTree;

interface

uses
  System.SysUtils, System.Classes, System.Actions, System.JSON,
  System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ImgList, Vcl.Menus,

  VirtualTrees, VirtualTrees.Types, VirtualTrees.Header,

  zObjInspector, zObjInspTypes,

  DDuce.Components.VirtualTrees.Node, DDuce.Components.SectionTree,
  DDuce.Editor.Interfaces, DDuce.Components.JsonTree;

type
  TfrmJsonTree = class(TForm)
    {$REGION 'designer controls'}
    aclMain               : TActionList;
    actCollapse           : TAction;
    actCopy               : TAction;
    actCreateJsonDocument : TAction;
    actExpand             : TAction;
    actParseDocument      : TAction;
    btnCollapse           : TButton;
    btnCreateJsonDocument : TButton;
    btnExpand             : TButton;
    btnParseDocument      : TButton;
    imlMain               : TImageList;
    mmoJson               : TMemo;
    mniCopy               : TMenuItem;
    pnlEditor             : TPanel;
    pnlMain               : TPanel;
    pnlObjectInspector    : TPanel;
    pnlTop                : TPanel;
    pnlTree               : TPanel;
    ppmTree               : TPopupMenu;
    splVertical           : TSplitter;
    {$ENDREGION}

    {$REGION 'event handlers'}
    function FObjectInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;
    procedure pnlTopClick(Sender: TObject);
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actExpandExecute(Sender: TObject);
    procedure actCollapseExecute(Sender: TObject);
    procedure actParseDocumentExecute(Sender: TObject);
    procedure actCreateJsonDocumentExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    {$ENDREGION}

  private
    FTree            : TJsonTree;
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
  System.Rtti, System.StrUtils, Vcl.Clipbrd,

  DDuce.Components.Factories, DDuce.Factories.VirtualTrees,
  DDuce.Factories.zObjInspector, DDuce.Editor.Factories, DDuce.Logger;

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

{$REGION 'construction and destruction'}
procedure TfrmJsonTree.AfterConstruction;
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
  FEditor.HighlighterName := 'JSON';
  FEditor.Text := mmoJson.Lines.Text;
end;
{$ENDREGION}

{$REGION 'event handlers'}
function TfrmJsonTree.FObjectInspectorBeforeAddItem(Sender: TControl;
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
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmJsonTree.actCollapseExecute(Sender: TObject);
begin
  FTree.FullCollapse;
  FTree.Header.AutoFitColumns;
end;

procedure TfrmJsonTree.actCopyExecute(Sender: TObject);
begin
  Clipboard.AsText := FTree.FocusedValue;
end;

procedure TfrmJsonTree.actCreateJsonDocumentExecute(Sender: TObject);
var
  LObject : TJSONObject;
begin
  LObject := TJSONObject.Create;
  try
    var LNestedObject := TJSONObject.Create;
    LObject.AddPair('ObjectName', LNestedObject);
    LNestedObject.AddPair('StringName', 'StringValue');
    var LBool := TJSONBool.Create(True);
    LNestedObject.AddPair('BoolName', LBool);
    var LInteger := TJSONNumber.Create(42);
    LNestedObject.AddPair('IntegerName', LInteger);

    LNestedObject.AddPair('Cloned', LNestedObject.Clone as TJSONValue);
    LObject.AddPair('Clone', LObject.Clone as TJSONValue);

    var LArray := TJSONArray.Create;
    for var I := 0 to 30 do
      LArray.Add(LObject.Clone as TJSONObject);
    LNestedObject.AddPair('ArrayName', LArray);

    FEditor.Text := LObject.Format(2);
  finally
    LObject.Free;
  end;
end;

procedure TfrmJsonTree.actExpandExecute(Sender: TObject);
begin
  FTree.FullExpand;
  FTree.Header.AutoFitColumns;
end;

procedure TfrmJsonTree.actParseDocumentExecute(Sender: TObject);
begin
  FTree.JsonString := FEditor.Text;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmJsonTree.InitializeTree;
begin
  FTree := TJsonTree.Create(Self);
  FTree.Parent      := pnlTree;
  FTree.BorderStyle := bsNone;
  FTree.Align       := alClient;
  FTree.Font.Name   := 'Consolas';
  FTree.Font.Size   := 10;
  FTree.PopupMenu   := ppmTree;
end;

procedure TfrmJsonTree.pnlTopClick(Sender: TObject);
begin
  FEditor.Text := FTree.JsonString;
end;
{$ENDREGION}

end.
