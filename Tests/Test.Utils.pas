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

{$I Test.DDuce.inc}

unit Test.Utils;

{ Common routines used in our set of unit tests. }

interface

uses
  System.Rtti,
  Data.DB,

  Test.Data;

type
  TTestUtils = record
    class function CreateDataSet(ARecordCount: Integer): TDataSet; static;
    class function CreateTestObject: TTestClass; static;
    class function CreateTestRecord: TTestRecord; static;
  end;

implementation

uses
  System.SysUtils, System.TypInfo, System.StrUtils, System.Variants,
  System.Classes,
  DataSnap.DBClient, MidasLib,

  DDuce.RandomData;

class function TTestUtils.CreateDataSet(ARecordCount: Integer): TDataSet;
var
  DS : TClientDataSet;
  I  : Integer;
begin
  DS := TClientDataSet.Create(nil);
  with DS.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Name     := 'FirstName';
  end;
  with DS.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Name     := 'LastName';
  end;
  with DS.FieldDefs.AddFieldDef do
  begin
    DataType := ftInteger;
    Name     := 'Age';
  end;
  with DS.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Size     := 40;
    Name     := 'CompanyName';
  end;
  with DS.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Size     := 80;
    Name     := 'Address';
  end;
  DS.CreateDataSet;
  for I := 0 to Pred(ARecordCount) do
  begin
    DS.Append;
    DS.FieldByName('FirstName').AsString   := RandomData.FirstName;
    DS.FieldByName('LastName').AsString    := RandomData.LastName;
    DS.FieldByName('Age').AsInteger        := RandomData.Number(20, 68);
    DS.FieldByName('CompanyName').AsString := RandomData.CompanyName;
    DS.FieldByName('Address').AsString     := RandomData.Address;
    DS.Post;
  end;
  DS.First;
  Result := DS;
end;

class function TTestUtils.CreateTestObject: TTestClass;
begin
  Result := TTestClass.Create;
  Result.TestBoolean  := True;
  Result.TestChar     := RandomData.Letter;
  Result.TestDateTime := Now;
  Result.TestDouble   := Pi;
  Result.TestInteger  := RandomData.Number(100);
  Result.TestString   := RandomData.Vegetable;
end;

class function TTestUtils.CreateTestRecord: TTestRecord;
begin
  Result.TestBoolean  := True;
  Result.TestChar     := RandomData.Letter;
  Result.TestDateTime := Now;
  Result.TestDouble   := Pi;
  Result.TestInteger  := RandomData.Number(100);
  Result.TestString   := RandomData.Vegetable;
end;

end.
