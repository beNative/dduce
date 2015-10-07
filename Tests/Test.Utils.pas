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

unit Test.Utils;

{ Common routines used in our set of unit tests. }

interface

uses
  Data.DB;

type
  TTestUtils = record
    class function CreateDataSet(ARecordCount: Integer): TDataSet; static;

  end;

implementation

uses
  DataSnap.DBClient,

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
    Name     := 'Name';
  end;
  with DS.FieldDefs.AddFieldDef do
  begin
    DataType := ftInteger;
    Name     := 'Age';
  end;
  DS.CreateDataSet;
  for I := 0 to Pred(ARecordCount) do
  begin
    DS.Append;
    DS.FieldByName('Name').AsString := RandomData.FirstName(gnMale);
    DS.FieldByName('Age').AsInteger := RandomData.Number(50);
    DS.Post;
  end;
  DS.First;
  Result := DS;
end;

end.
