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

unit DDuce.Editor.ActionList.Templates;

interface

uses
  System.Classes, System.SysUtils, System.Actions,
  Vcl.ActnList,

  Spring.Collections,

  DSharp.Core.DataTemplates, DSharp.Windows.ColumnDefinitions.ControlTemplate;

type

  { TActionTemplate }

  TActionTemplate = class(TColumnDefinitionsControlTemplate)
    function GetItemTemplate(const Item: TObject): IDataTemplate; override;
    function GetTemplateDataClass: TClass; override;

    function GetText(const Item: TObject; const ColumnIndex: Integer): string;
      override;
  end;

  { TActionCategoryTemplate }

  TActionCategoryTemplate = class(TColumnDefinitionsControlTemplate)
  private
    FActionTemplate : IDataTemplate;

  public
    function GetItemTemplate(const Item: TObject): IDataTemplate; override;
    function GetTemplateDataClass: TClass; override;

    function GetItemCount(const Item: TObject): Integer; override;
    function GetItems(const Item: TObject): IObjectList; override;
    function GetItem(const Item: TObject; const Index: Integer): TObject;
       override;

    function GetText(const Item: TObject; const ColumnIndex: Integer): string;
      override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{$REGION 'TActionTemplate'}
function TActionTemplate.GetItemTemplate(const Item: TObject): IDataTemplate;
begin
  if Item is TContainedAction then
    Result := Self
  else
    Result := inherited GetItemTemplate(Item);
end;

function TActionTemplate.GetTemplateDataClass: TClass;
begin
  Result := TContainedAction;
end;

function TActionTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  if Item is TContainedAction then
  begin
    if ColumnIndex = 1 then
      Result := (Item as TAction).Caption;
  end
  else
    Result := inherited GetText(Item, ColumnIndex);
end;
{$ENDREGION}

{$REGION 'TActionCategoryTemplate'}
function TActionCategoryTemplate.GetItemTemplate(
  const Item: TObject): IDataTemplate;
begin
//  if Item is IObjectList then
//    Result := Self
//  else
//    Result := inherited GetItemTemplate(Item);
end;

function TActionCategoryTemplate.GetTemplateDataClass: TClass;
begin
  //Result := TObjectList;
end;

function TActionCategoryTemplate.GetItemCount(const Item: TObject): Integer;
begin
//  if Item is TObjectList then
//  begin
//    Result := 1;
//  end
//  else
//    Result := inherited GetItemCount(Item);
end;

function TActionCategoryTemplate.GetItems(const Item: TObject): IObjectList;
begin
//  if Item is TObjectList then
//  begin
//    Result := Item as TObjectList;
//  end
//  else
    Result := inherited GetItems(Item);
end;

function TActionCategoryTemplate.GetItem(const Item: TObject;
  const Index: Integer): TObject;
begin
//  if Item is TObjectList then
//  begin
//    Result := Item;
//  end
//  else
    Result := inherited GetItem(Item, Index);
end;

function TActionCategoryTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  if Item is TContainedAction then
  begin
    if ColumnIndex = 0 then
      Result := 'Actions'
    else
      Result := '';
  end
  else
    Result := inherited GetText(Item, ColumnIndex);
end;

procedure TActionCategoryTemplate.AfterConstruction;
begin
  inherited AfterConstruction;
  FActionTemplate := TActionTemplate.Create(FColumnDefinitions);
  RegisterDataTemplate(FActionTemplate);
end;

procedure TActionCategoryTemplate.BeforeDestruction;
begin
  FActionTemplate := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

end.

