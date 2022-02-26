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

unit Demo.DDuce.Editor;

{ Form demonstrating the IEditorView module. }

{$REGION 'Documentation'}
{
  Minimum requirements to add an IEditorView instance to your application:

  uses
    DDuce.Editor.Interfaces, DDuce.Editor.Factories

    FSettings := TEditorFactories.CreateSettings(Owner);
    FManager  := TEditorFactories.CreateManager(Owner, FSettings);
    FEditor   := TEditorFactories.CreateView(Parent, FManager)

}
{$ENDREGION}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus,
  Vcl.ComCtrls,

  zObjInspector,

  DDuce.Components.Factories, DDuce.Editor.Interfaces;

type
  TfrmEditor = class(TForm)
    pnlLeft       : TPanel;
    pnlRight      : TPanel;
    sbrMain       : TStatusBar;
    splVertical   : TSplitter;
    pnlLeftTop    : TPanel;
    splHorizontal : TSplitter;
    pnlLeftBottom : TPanel;

    procedure FormResize(Sender: TObject);

  private
    FSettings          : IEditorSettings;
    FEditor            : IEditorView;
    FManager           : IEditorManager;
    FMainToolbar       : TToolbar;
    FSelectionToolbar  : TToolbar;
    //FRightToolbar    : TToolbar;
    FMainMenu          : TMainMenu;
    FSettingsInspector : TzObjectInspector;
    FEditorInspector   : TzObjectInspector;

  public
    procedure AfterConstruction; override;

  end;

implementation

uses
  DDuce.Factories.zObjInspector, DDuce.Editor.Factories;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmEditor.AfterConstruction;
begin
  inherited AfterConstruction;
  FSettings := TEditorFactories.CreateSettings(Self);
  FManager  := TEditorFactories.CreateManager(Self, FSettings);
  FEditor   := TEditorFactories.CreateView(pnlRight, FManager);
  FEditor.Editor.Highlighter.Colors.LoadFromFile('settings.texteditor.json');
  FMainMenu := TEditorFactories.CreateMainMenu(
    Self,
    FManager.Actions,
    FManager.Menus
  );
  FMainToolbar := TEditorFactories.CreateMainToolbar(
    Self,
    pnlRight,
    FManager.Actions,
    FManager.Menus
  );
  FMainToolbar.Color := clWhite;
  FSettingsInspector := TzObjectInspectorFactory.Create(
    Self,
    pnlLeftTop,
    (FSettings as IInterfaceComponentReference).GetComponent
  );
  FSettingsInspector.BorderStyle := bsNone;
  FEditorInspector := TzObjectInspectorFactory.Create(
    Self,
    pnlLeftBottom,
    FEditor.Editor
  );
  FEditorInspector.BorderStyle := bsNone;
//  FRightToolbar := TEditorFactories.CreateTopRightToolbar(
//    Self,
//    pnlRight,
//    FManager.Actions,
//    FManager.Menus
//  );
  FSelectionToolbar := TEditorFactories.CreateSelectionToolbar(
    Self,
    pnlRight,
    FManager.Actions,
    FManager.Menus
  );
  FSelectionToolbar.Align := alRight;
end;
{$ENDREGION}

procedure TfrmEditor.FormResize(Sender: TObject);
begin
  FEditor.Editor.Invalidate;
end;

end.
