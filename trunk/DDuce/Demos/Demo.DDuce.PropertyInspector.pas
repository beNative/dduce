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

unit Demo.DDuce.PropertyInspector;

{$I ..\Source\DDuce.inc}

interface

uses
  System.Classes,
  Vcl.ComCtrls, Vcl.ButtonGroup, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls,
  Vcl.Forms,

  DDuce.Components.PropertyInspector;

type
  TfrmPropertyInspector = class(TForm)
    {$REGION 'designer controls'}
    pnlMain       : TPanel;
    pnlLeft       : TPanel;
    pnlRight      : TPanel;
    btnButton     : TButton;
    chkCheckBox   : TCheckBox;
    edtEdit       : TEdit;
    bgButtonGroup : TButtonGroup;
    cbxControls   : TComboBox;
    sbrStatusBar  : TStatusBar;
    trbTrackBar   : TTrackBar;
    splSplitter   : TSplitter;
    lblLabel      : TLabel;
    {$ENDREGION}

    procedure cbxControlsChange(Sender: TObject);

  private
    FPropertyInspector: TPropertyInspector;

  public
    procedure AfterConstruction; override;

  end;

implementation

uses
  Demo.Helpers;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmPropertyInspector.AfterConstruction;
var
  I: Integer;
  C: TWinControl;
begin
  inherited;
  FPropertyInspector := CreateInspector(Self, pnlLeft, bgButtonGroup);
  FPropertyInspector.Name := 'PropertyInspector';
  for I := 0 to ComponentCount - 1 do
  begin
    if Components[I] is TWinControl then
    begin
      C := TWinControl(Components[I]);
      cbxControls.AddItem(C.Name, C);
    end;
  end;
  cbxControls.ItemIndex := 0;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmPropertyInspector.cbxControlsChange(Sender: TObject);
var
  C: TWinControl;
begin
  C := cbxControls.Items.Objects[cbxControls.ItemIndex] as TWinControl;
  FPropertyInspector.Objects[0] := C;
end;
{$ENDREGION}

end.
