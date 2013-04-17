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

unit DDuce.Demos.VirtualDBGrid;

//*****************************************************************************

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.Actions, Vcl.ActnList, Vcl.ImgList, Vcl.ExtCtrls, Vcl.ComCtrls,

  Data.DB,

  DSharp.Collections,

  DDuce.Components.VirtualDBGrid, DDuce.Components.ListDataSet,
  DDuce.Components.PropertyInspector,

  DDuce.Demos.Contact, Vcl.DBCtrls;

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
    navMain: TDBNavigator;
  private
    FVDBG      : TVirtualDBGrid;
    FList      : IList<TContact>;
    FDataSet   : TListDataset<TContact>;
    FInspector : TPropertyInspector;
  public
    procedure AfterConstruction; override;

  end;

//*****************************************************************************

implementation

uses
  DDuce.Demos.Helpers;

{$R *.dfm}

{$REGION 'construction and destruction'}
//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TfrmVirtualDBGrid.AfterConstruction;
begin
  inherited;
  FList           := CreateContactList(1000);
  FDataSet        := TListDataset<TContact>.Create(Self, FList);
  dscMain.DataSet := FDataSet;
  FVDBG           := CreateVirtualDBGrid(Self, pnlLeft, dscMain);
  FInspector      := CreateInspector(Self, pnlRight, FVDBG);
  FDataSet.Active := True;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************
{$ENDREGION}

end.
