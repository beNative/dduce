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
    imlMain : TImageList;

  private
    FDataSet: TClientDataSet;

    function GetDataSet: TDataSet;
    function GetImageList: TImageList;

  public
    procedure AfterConstruction; override;

    procedure FillDataSet(const ARecordCount: Integer);

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

const
  DEFAULT_RECORDCOUNT = 10000;

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
  FDataSet := TClientDataSet.Create(Self);
  FillDataSet(DEFAULT_RECORDCOUNT);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TdmData.GetDataSet: TDataSet;
begin
  Result := FDataSet;
end;

function TdmData.GetImageList: TImageList;
begin
  Result := imlMain;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TdmData.FillDataSet(const ARecordCount: Integer);
var
  I  : Integer;
  S  : string;
begin
  FDataSet.Data := Null;
  with FDataSet.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Name     := 'Name';
  end;
  with FDataSet.FieldDefs.AddFieldDef do
  begin
    DataType := ftDate;
    Name     := 'BirthDate';
  end;
  with FDataSet.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Name     := 'Email';
  end;
  with FDataSet.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Name     := 'Address';
  end;
  with FDataSet.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Name     := 'Company';
  end;
  with FDataSet.FieldDefs.AddFieldDef do
  begin
    DataType := ftBoolean;
    Name     := 'Active';
  end;
  FDataSet.CreateDataSet;
  for I := 0 to Pred(ARecordCount) do
  begin
    FDataSet.Append;
    S := RandomData.PersonName;
    FDataSet.FieldByName('Name').AsString := S;
    FDataSet.FieldByName('Email').AsString := RandomData.Email(S, RandomData.Name);
    FDataSet.FieldByName('BirthDate').AsDateTime := RandomData.BirthDate(1920, 1998);
    FDataSet.FieldByName('Company').AsString := RandomData.AlliteratedCompanyName;
    FDataSet.FieldByName('Address').AsString := RandomData.Address;
    FDataSet.FieldByName('Active').AsBoolean := RandomData.Bool;
    FDataSet.Post;
  end;
  FDataSet.First;
end;
{$ENDREGION}

end.



