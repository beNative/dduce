{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit DDuce.Editor.Search.Templates;

{ Data templates used to display the search results hierarchically in a
  TVirtualStringTree using a TTreeViewPresenter. }

interface

uses
  Classes, SysUtils, Contnrs,

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

