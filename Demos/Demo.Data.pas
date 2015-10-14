{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Demo.Data;

interface

uses
  System.Classes, System.ImageList,
  Vcl.ImgList, Vcl.Controls,
  Data.DB, Datasnap.DBClient;

type
  TdmData = class(TDataModule)
    cdsMain : TClientDataSet;
    imlMain : TImageList;

  private
    function GetDataSet: TDataSet;
    function GetImageList: TImageList;

  public
    procedure AfterConstruction; override;

    procedure FillDataSet(ARecordCount: Integer);

    property DataSet: TDataSet
      read GetDataSet;

    property ImageList: TImageList
      read GetImageList;
  end;

function Data: TdmData;

implementation

{$R *.dfm}

uses
  System.Variants,
  Vcl.Forms,

  DDuce.RandomData;

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

{$REGION 'construction and destruction'}
procedure TdmData.AfterConstruction;
begin
  inherited AfterConstruction;
  FillDataSet(10000);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TdmData.GetDataSet: TDataSet;
begin
  Result := cdsMain;
end;

function TdmData.GetImageList: TImageList;
begin
  Result := imlMain;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TdmData.FillDataSet(ARecordCount: Integer);
var
  DS : TClientDataSet;
  I  : Integer;
  S  : string;
begin
  DS := cdsMain;
  DS.Data := Null;
  with DS.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Name     := 'Name';
  end;
  with DS.FieldDefs.AddFieldDef do
  begin
    DataType := ftDate;
    Name     := 'BirthDate';
  end;
  with DS.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Name     := 'Email';
  end;
  with DS.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Name     := 'Address';
  end;
  with DS.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Name     := 'Company';
  end;
  with DS.FieldDefs.AddFieldDef do
  begin
    DataType := ftBoolean;
    Name     := 'Active';
  end;
  DS.CreateDataSet;
  for I := 0 to Pred(ARecordCount) do
  begin
    DS.Append;
    S := RandomData.PersonName;
    DS.FieldByName('Name').AsString := S;
    DS.FieldByName('Email').AsString := RandomData.Email(S, RandomData.Name);
    DS.FieldByName('BirthDate').AsDateTime := RandomData.BirthDate(1920, 1998);
    DS.FieldByName('Company').AsString := RandomData.AlliteratedCompanyName;
    DS.FieldByName('Address').AsString := RandomData.Address;
    DS.FieldByName('Active').AsBoolean := RandomData.Bool;
    DS.Post;
  end;
  DS.First;
end;
{$ENDREGION}

end.



