{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Demos.ScopedReferences;

{ Form demonstrating the use of scoped references in Delphi (or smart pointers
  how they are called in C++). }

//*****************************************************************************

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, System.Actions,

  ts.ScopedReference;

type
  TfrmScopedReferences = class(TForm)
    aclMain           : TActionList;
    actShowClassNames : TAction;
    btnShowClassNames : TButton;
    lblCode: TLabel;

    procedure actShowClassNamesExecute(Sender: TObject);

  private
    procedure ShowClassNames;
  end;

//*****************************************************************************

implementation

{$R *.dfm}

//*****************************************************************************
// action handlers                                                       BEGIN
//*****************************************************************************

procedure TfrmScopedReferences.actShowClassNamesExecute(Sender: TObject);
begin
  ShowClassNames;
end;

//*****************************************************************************
// action handlers                                                         END
//*****************************************************************************

//*****************************************************************************
// private methods                                                       BEGIN
//*****************************************************************************

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

//*****************************************************************************
// private methods                                                         END
//*****************************************************************************

end.

