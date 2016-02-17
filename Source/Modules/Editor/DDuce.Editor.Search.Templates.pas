{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Editor.Search.Templates;

{ Data templates used to display the search results hierarchically in a
  TVirtualStringTree using a TTreeViewPresenter. }

interface

uses
  System.Classes, System.SysUtils, System.Contnrs,

  Spring.Collections,

  DSharp.Windows.TreeViewPresenter, DSharp.Windows.ColumnDefinitions,
  DSharp.Core.DataTemplates, DSharp.Windows.ColumnDefinitions.ControlTemplate;

type
  TSearchResultGroupTemplate = class(TColumnDefinitionsControlTemplate, IDataTemplate)
  public
    function GetItemTemplate(const Item: TObject): IDataTemplate; override;
    function GetTemplateDataClass: TClass; override;

    function GetItemCount(const Item: TObject): Integer; override;

    function GetItems(const Item: TObject): IObjectList; override;
    function GetItem(const Item: TObject; const Index: Integer): TObject;
      override;

    procedure AfterConstruction; override;
  end;

  { TSearchResultLineTemplate }

  TSearchResultLineTemplate = class(TColumnDefinitionsControlTemplate, IDataTemplate)
  public
    function GetItemTemplate(const Item: TObject): IDataTemplate; override;
    function GetTemplateDataClass: TClass; override;

    function GetItemCount(const Item: TObject): Integer; override;
    function GetItems(const Item: TObject): IObjectList; override;
    function GetItem(const Item: TObject; const Index: Integer): TObject;
      override;

    procedure AfterConstruction; override;
  end;

  { TSearchResultTemplate }

  TSearchResultTemplate = class(TColumnDefinitionsControlTemplate, IDataTemplate)
    function GetItemTemplate(const Item: TObject): IDataTemplate; override;
    function GetTemplateDataClass: TClass; override;

  end;

implementation

uses
  DDuce.Editor.Search.Data;

{$REGION 'TSearchResultTemplate'}
function TSearchResultTemplate.GetItemTemplate(
  const Item: TObject): IDataTemplate;
begin
  if Item is TSearchResult then
    Result := Self
  else
    Result := inherited GetItemTemplate(Item);
end;

function TSearchResultTemplate.GetTemplateDataClass: TClass;
begin
  Result := TSearchResult;
end;
{$ENDREGION}

{$REGION 'TSearchResultLineTemplate'}
procedure TSearchResultLineTemplate.AfterConstruction;
begin
  inherited AfterConstruction;
  RegisterDataTemplate(TSearchResultTemplate.Create(FColumnDefinitions));
end;

function TSearchResultLineTemplate.GetItemTemplate(
  const Item: TObject): IDataTemplate;
begin
  if Item is TSearchResultLine then
    Result := Self
  else
    Result := inherited GetItemTemplate(Item);
end;

function TSearchResultLineTemplate.GetTemplateDataClass: TClass;
begin
  Result := TSearchResultLine;
end;

function TSearchResultLineTemplate.GetItemCount(const Item: TObject): Integer;
begin
  if Item is TSearchResultLine then
    Result := TSearchResultLine(Item).List.Count
  else
    Result := inherited GetItemCount(Item);
end;

function TSearchResultLineTemplate.GetItems(const Item: TObject): IObjectList;
begin
  if Item is TSearchResultLine then
    Result := TSearchResultLine(Item).List
  else
    Result := inherited GetItems(Item);
end;

function TSearchResultLineTemplate.GetItem(const Item: TObject;
  const Index: Integer): TObject;
begin
  if Item is TSearchResultLine then
    Result := TSearchResultLine(Item).List[Index]
  else
    Result := inherited GetItem(Item, Index);
end;
{$ENDREGION}

{$REGION 'TSearchResultGroupTemplate'}
function TSearchResultGroupTemplate.GetItemCount(const Item: TObject): Integer;
begin
  if Item is TSearchResultGroup then
    Result := TSearchResultGroup(Item).Lines.Count
  else
    Result := inherited GetItemCount(Item);
end;

function TSearchResultGroupTemplate.GetItems(const Item: TObject): IObjectList;
begin
  if Item is TSearchResultGroup then
    Result := TSearchResultGroup(Item).Lines
  else
    Result := inherited GetItems(Item);
end;

function TSearchResultGroupTemplate.GetItemTemplate(
  const Item: TObject): IDataTemplate;
begin
  if Item is TSearchResultGroup then
    Result := Self
  else
    Result := inherited GetItemTemplate(Item);
end;

function TSearchResultGroupTemplate.GetItem(const Item: TObject;
  const Index: Integer): TObject;
begin
  if Item is TSearchResultGroup then
    Result := TSearchResultGroup(Item).Lines[Index]
  else
    Result := inherited GetItem(Item, Index);
end;

function TSearchResultGroupTemplate.GetTemplateDataClass: TClass;
begin
  Result := TSearchResultGroup;
end;

procedure TSearchResultGroupTemplate.AfterConstruction;
begin
  RegisterDataTemplate(TSearchResultLineTemplate.Create(FColumnDefinitions));
  inherited AfterConstruction;
end;
{$ENDREGION}

end.

