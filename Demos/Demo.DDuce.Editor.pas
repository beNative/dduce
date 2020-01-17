{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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
    pnlLeft     : TPanel;
    pnlRight    : TPanel;
    sbrMain     : TStatusBar;
    splVertical : TSplitter;

  private
    FSettings         : IEditorSettings;
    FEditor           : IEditorView;
    FManager          : IEditorManager;
    FMainToolbar      : TToolbar;
    //FSelectionToolbar : TToolbar;
    //FRightToolbar     : TToolbar;
    FMainMenu         : TMainMenu;
    FOI               : TzObjectInspector;

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
  FSettings    := TEditorFactories.CreateSettings(Self);
  FManager     := TEditorFactories.CreateManager(Self, FSettings);
  FEditor      := TEditorFactories.CreateView(pnlRight, FManager);
  FMainMenu    := TEditorFactories.CreateMainMenu(
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
  FOI := TzObjectInspectorFactory.Create(
    Self,
    pnlLeft,
    (FSettings as IInterfaceComponentReference).GetComponent
  );
  FOI.BorderStyle := bsNone;

//  FRightToolbar := TEditorFactories.CreateTopRightToolbar(
//    Self,
//    pnlRight,
//    FManager.Actions,
//    FManager.Menus
//  );
//  FSelectionToolbar := TEditorFactories.CreateSelectionToolbar(
//    Self,
//    pnlRight,
//    FManager.Actions,
//    FManager.Menus
//  );
  //FSelectionToolbar.Align := alRight;
end;
{$ENDREGION}

end.
