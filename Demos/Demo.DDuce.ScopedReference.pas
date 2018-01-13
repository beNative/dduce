{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Demo.DDuce.ScopedReference;

{ Form demonstrating the use of scoped references in Delphi (or smart pointers
  as they are called in C++). }

interface

{$I ..\Source\DDuce.inc}

uses
  System.Actions, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.StdCtrls,
  Vcl.ExtCtrls,

  DDuce.ScopedReference;

type
  TfrmScopedReferences = class(TForm)
    aclMain               : TActionList;
    actShowClassNames     : TAction;
    btnShowClassNames     : TButton;
    lblCode               : TLabel;
    pnlScopedButton       : TPanel;
    btnCreateScopedButton : TButton;
    actCreateScopedButton : TAction;

    procedure actShowClassNamesExecute(Sender: TObject);
    procedure actCreateScopedButtonExecute(Sender: TObject);

  private
    procedure ShowClassNames;
    procedure CreateScopedButton;
  end;

implementation

{$R *.dfm}

{$REGION 'action handlers'}
procedure TfrmScopedReferences.actCreateScopedButtonExecute(Sender: TObject);
begin
  CreateScopedButton;
end;

procedure TfrmScopedReferences.actShowClassNamesExecute(Sender: TObject);
begin
  ShowClassNames;
end;
{$ENDREGION}

{$REGION 'private methods'}
{ Automatically instantiates and displays the class name of the managed objects,
  which are all destroyed when leaving scope. }

procedure TfrmScopedReferences.ShowClassNames;
var
  O: Scoped<TObject>;
  P: Scoped<TPersistent>;
  L: Scoped<TList>;
begin
  ShowMessage(O.Ref.ClassName);
  ShowMessage(P.Ref.ClassName);
  ShowMessage(L.Ref.ClassName);
end;

{ Creates a scoped TButton control by giving a anonymous factory function as
  the argument. It is automatically destroyed when leaving scope. }

procedure TfrmScopedReferences.CreateScopedButton;
var
  B : Scoped<TButton>;
begin
  B.Create(
    function: TButton
    begin
      Result := TButton.Create(Self);
      Result.Parent  := pnlScopedButton;
      Result.Caption := 'I''m a scoped button!';
      Result.Align   := alClient;
      ShowMessage('Scoped button created.');
    end
  );
  ShowMessage('Scoped button will be automatically destroyed.');
end;
{$ENDREGION}

end.

