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

unit Demo.DDuce.Reflect;

{$I ..\Source\DDuce.inc}

{ Form demonstrating the Reflect type. }

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms;

type
  TfrmReflect = class(TForm)
    mmoMain      : TMemo;
    lblReflected : TLabel;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Reflect;

procedure TfrmReflect.AfterConstruction;
begin
  inherited;
  mmoMain.Text := Reflect.Properties(Self).ToString;
end;

end.
