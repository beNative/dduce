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

unit Demo.DDuce.VirtualDBGrid;

{$I ..\Source\DDuce.inc}

{ Form demonstrating the TVirtualDBGrid component connected to a generic
  TListDataSet. }

interface

uses
  System.Actions, System.Classes,
  Vcl.ActnList, Vcl.ImgList, Vcl.Controls, Vcl.DBCtrls, Vcl.ExtCtrls, Vcl.Forms,
  Vcl.ComCtrls,
  Data.DB,

{$IFDEF DSHARP}
  DSharp.Collections,
{$ENDIF}

{$IFDEF SPRING}
  Spring, Spring.Collections,
{$ENDIF}

  DDuce.Components.VirtualDBGrid, DDuce.Components.ListDataSet,
  DDuce.Components.PropertyInspector,

  Demo.Contact;

type
  TfrmVirtualDBGrid = class(TForm)
    splHorizontal       : TSplitter;
    sbrMain             : TStatusBar;
    pnlTop              : TPanel;
    pnlBottom           : TPanel;
    pnlLeft             : TPanel;
    splVertical         : TSplitter;
    pnlRight            : TPanel;
    imlMain             : TImageList;
    aclMain             : TActionList;
    actInspectComponent : TAction;
    dscMain             : TDataSource;
    navMain             : TDBNavigator;

  private
    FVDBG      : TVirtualDBGrid;
    FList      : IList<TContact>;
    FDataSet   : TListDataset<TContact>;
    FInspector : TPropertyInspector;

  public
    procedure AfterConstruction; override;

  end;

implementation

uses
  Demo.Factories;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmVirtualDBGrid.AfterConstruction;
begin
  inherited;
  FList           := TDemoFactories.CreateContactList(1000);
  FDataSet        := TListDataset<TContact>.Create(Self, FList);
  dscMain.DataSet := FDataSet;
  FVDBG           := TDemoFactories.CreateVirtualDBGrid(Self, pnlLeft, dscMain);
  FInspector      := TDemoFactories.CreateInspector(Self, pnlRight, FVDBG);
  FDataSet.Active := True;
  FVDBG.Header.AutoFitColumns;
end;
{$ENDREGION}

end.
