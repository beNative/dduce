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

unit DDuce.Demos.Data;

//*****************************************************************************

interface

uses
  SysUtils, Classes, DB, DBClient;

type
  TdmData = class(TDataModule)
    cdsMain: TClientDataSet;
  private
    function GetDataSet: TDataSet;

  public
    property DataSet: TDataSet
      read GetDataSet;
  end;

function Data: TdmData;

//*****************************************************************************

implementation

{$R *.dfm}

uses
  Forms;

var
  FData: TdmData;

{$REGION 'interfaced routines'}
function Data: TdmData;
begin
  if not Assigned(FData) then
    FData := TdmData.Create(Application);
  Result := FData;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TdmData.GetDataSet: TDataSet;
begin
  Result := cdsMain;
end;
{$ENDREGION}
end.
