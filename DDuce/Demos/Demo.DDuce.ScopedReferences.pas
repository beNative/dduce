{
  Copyright (C) 2013-2014 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit Demo.DDuce.ScopedReferences;

{ Form demonstrating the use of scoped references in Delphi (or smart pointers
  as they are called in C++). }

interface

{$I ..\Source\DDuce.inc}

uses
  System.Actions, System.SysUtils, System.Variants, System.Classes,
  Winapi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.StdCtrls,
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
  ShowMessage('Scoped button will be destroyed.');
end;
{$ENDREGION}

end.

