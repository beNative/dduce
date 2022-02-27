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

unit Demo.DDuce.IniTree;

interface

uses
  System.SysUtils, System.Classes, System.Actions,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.ActnList,
  Vcl.StdCtrls,

  VirtualTrees, VirtualTrees.Types, VirtualTrees.Header,

  zObjInspector,

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
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actExpandExecute(Sender: TObject);
    procedure actCollapseExecute(Sender: TObject);
    procedure actParseDocumentExecute(Sender: TObject);
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
  DDuce.Components.Factories, DDuce.Factories.VirtualTrees,
  DDuce.Factories.zObjInspector, DDuce.Editor.Factories,

  DDuce.Logger.Factories, DDuce.Logger.Channels.Winipc, DDuce.Logger;

{$REGION 'construction and destruction'}
procedure TfrmIniTree.AfterConstruction;
begin
  inherited AfterConstruction;
  Logger.Channels.Add(TWinipcChannel.Create(False));
  InitializeTree;
  FObjectInspector := TzObjectInspectorFactory.Create(
    Self,
    pnlObjectInspector,
    FTree
  );
  FObjectInspector.AlignWithMargins := True;
  FSettings := TEditorFactories.CreateSettings(Self);
  FManager  := TEditorFactories.CreateManager(Self, FSettings);
  FEditor   := TEditorFactories.CreateView(pnlEditor, FManager);
  FEditor.Editor.Highlighter.Colors.LoadFromFile('settings.texteditor.json');
  FEditor.HighlighterName := 'INI';
  FEditor.Text := mmoJson.Lines.Text;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmIniTree.actCollapseExecute(Sender: TObject);
begin
  FTree.FullCollapse;
end;

procedure TfrmIniTree.actExpandExecute(Sender: TObject);
begin
  FTree.FullExpand;
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
  FTree.Parent      := pnlTree;
  FTree.BorderStyle := bsNone;
  FTree.Align       := alClient;
  FTree.Font.Name   := 'Consolas';
  FTree.Font.Size   := 10;
end;
{$ENDREGION}

end.
