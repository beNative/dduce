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

unit Demo.Factories;

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.DBGrids,
  Data.DB,

  VirtualTrees,

  Spring, Spring.Collections, Spring.Collections.Extensions,
  Spring.Collections.Lists,

  DSharp.Windows.TreeViewPresenter,
  DSharp.Core.DataTemplates,

  DDuce.Components.PropertyInspector, DDuce.Components.LogTree,
  DDuce.Components.GridView, DDuce.Components.DBGridView,
  DDuce.Components.Inspector,

  Demo.Contact;

type
  TDemoFactories = record
  public
    class procedure FillListWithContacts(
      AList  : IList<TContact>;
      ACount : Integer
    ); static;

    { Create standard VCL DB Grid. }
    class function CreateDBGrid(
      AOwner      : TComponent;
      AParent     : TWinControl;
      ADataSource : TDataSource = nil;
      const AName : string = ''
    ): TDBGrid; static;

    class function CreateContactDataSet(
      AOwner      : TComponent;
      ACount      : Integer = 0;
      const AName : string = ''
    ): TDataSet; static;

    class procedure FillDataSetWithContacts(
      ADataSet : TDataSet;
      ACount   : Integer = 0
    ); static;

    class function CreateRandomContact: TContact; static;

    class function CreateContactList(
      const ACount: Integer = 0
    ): IList<TContact>; static;
  end;

implementation

uses
  System.Rtti,
  Vcl.Forms, Vcl.Graphics,
  Datasnap.DBClient,

  DSharp.Windows.ColumnDefinitions.ControlTemplate,

  DDuce.RandomData;

{$REGION 'TDemoFactories'}
class function TDemoFactories.CreateContactDataSet(AOwner: TComponent;
  ACount: Integer; const AName: string): TDataSet;
var
  CDS : TClientDataSet;
begin
  CDS := TClientDataSet.Create(AOwner);
  if AName <> '' then
    CDS.Name := AName;
  with CDS.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Size     := 20;
    Name     := 'Name';
  end;
  with CDS.FieldDefs.AddFieldDef do
  begin
    DataType := ftDate;
    Name     := 'BirthDate';
  end;
  with CDS.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Size     := 20;
    Name     := 'Email';
  end;
  with CDS.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Size     := 80;
    Name     := 'Address';
  end;
  with CDS.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Size     := 50;
    Name     := 'Company';
  end;
  with CDS.FieldDefs.AddFieldDef do
  begin
    DataType := ftBoolean;
    Name     := 'Active';
  end;
  CDS.CreateDataSet;
  FillDataSetWithContacts(CDS, ACount);
  Result := CDS;
end;

class function TDemoFactories.CreateContactList(const ACount: Integer):
  IList<TContact>;
begin
  Result := TObjectList<TContact>.Create;
  FillListWithContacts(Result, ACount);
end;

class function TDemoFactories.CreateDBGrid(AOwner: TComponent;
  AParent: TWinControl; ADataSource: TDataSource; const AName: string): TDBGrid;
var
  DBG : TDBGrid;
begin
  DBG                  := TDBGrid.Create(AOwner);
  DBG.AlignWithMargins := True;
  DBG.Parent           := AParent;
  DBG.Align            := alClient;
  DBG.DataSource       := ADataSource;
  Result := DBG;
end;

class function TDemoFactories.CreateRandomContact: TContact;
var
  C : TContact;
begin
  C := TContact.Create;
  with C do
  begin
    FirstName   := RandomData.FirstName;
    LastName    := RandomData.LastName;
    CompanyName := RandomData.CompanyName;
    Email       := RandomData.Email(FirstName, LastName);
    Address     := RandomData.Address;
    Number      := RandomData.Number(100);
    BirthDate   := RandomData.BirthDate(1928, 1987);
    Country     := 'USA';
    Active      := RandomData.Bool;
  end;
  Result := C;
end;

class procedure TDemoFactories.FillDataSetWithContacts(ADataSet: TDataSet;
  ACount: Integer);
var
  I : Integer;
  S : string;
begin
  ADataSet.Active := True;
  for I := 0 to Pred(ACount) do
  begin
    ADataSet.Append;
    S := RandomData.FullName;
    ADataSet.FieldByName('Name').AsString := S;
    ADataSet.FieldByName('Email').AsString := RandomData.Email(S, RandomData.Name);
    ADataSet.FieldByName('BirthDate').AsDateTime := RandomData.BirthDate(1920, 1998);
    ADataSet.FieldByName('Company').AsString := RandomData.AlliteratedCompanyName;
    ADataSet.FieldByName('Address').AsString := RandomData.Address;
    ADataSet.FieldByName('Active').AsBoolean := RandomData.Bool;
    ADataSet.Post;
  end;
  ADataSet.First;
end;

class procedure TDemoFactories.FillListWithContacts(AList: IList<TContact>;
  ACount: Integer);
var
  I : Integer;
begin
  if Assigned(AList) then
  begin
    AList.Clear;
    for I := 0 to ACount - 1 do
    begin
      AList.Add(CreateRandomContact);
    end;
  end;
end;
{$ENDREGION}

end.
