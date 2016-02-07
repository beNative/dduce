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

unit DDuce.Editor.Tools.Settings;

interface

uses
  System.Classes, System.SysUtils;

type
  TEditorToolSettings = class(TComponent)
  private
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TComponent;
    function GetItemsByClass(AClass: TComponentClass): TComponent;
    function GetItemsByName(const AName: string): TComponent;

  public
    function RegisterSettings(
      ASettingsClass : TComponentClass;
      const AName    : string
    ) : TComponent;

  public
    property Count: Integer
      read GetCount;

    property Items[AIndex: Integer]: TComponent
      read GetItems;

    property ItemsByClass[AClass: TComponentClass]: TComponent
      read GetItemsByClass;

    property ItemsByName[const AName: string]: TComponent
      read GetItemsByName; default;
  end;

implementation

uses
  DDuce.Logger;

{$REGION 'property access mehods'}
function TEditorToolSettings.GetItemsByClass(
  AClass: TComponentClass): TComponent;
var
  I : Integer;
  B : Boolean;
begin
  Result := nil;
  I := 0;
  B := False;
  if ComponentCount > 0 then
  begin
    while (I < ComponentCount) and not B do
    begin
      B := Components[I] is AClass;
      if not B then
        Inc(I);
    end;
    if B then
      Result := Components[I];
  end;
end;

function TEditorToolSettings.GetCount: Integer;
begin
  Result := ComponentCount;
end;

function TEditorToolSettings.GetItems(AIndex: Integer): TComponent;
begin
  Result := Components[AIndex];
end;

function TEditorToolSettings.GetItemsByName(
  const AName: string): TComponent;
var
  I : Integer;
  B : Boolean;
begin
  Result := nil;
  I := 0;
  B := False;
  if ComponentCount > 0 then
  begin
    while (I < ComponentCount) and not B do
    begin
      B := Components[I].Name = AName;
      if not B then
        Inc(I);
    end;
    if B then
      Result := Components[I];
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TEditorToolSettings.RegisterSettings(
  ASettingsClass: TComponentClass; const AName: string): TComponent;
var
  ESI : TComponent;
begin
  ESI := ItemsByClass[ASettingsClass];
  if not Assigned(ESI) then
  begin
    ESI := ASettingsClass.Create(Self);
    ESI.Name := AName;
  end;
  Result := ESI;
end;
{$ENDREGION}

initialization
  RegisterClass(TEditorToolSettings);

end.
