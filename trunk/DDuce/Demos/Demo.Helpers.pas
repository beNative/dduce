{
  Copyright (C) 2013-2014 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Demo.Helpers;

{$I ..\Source\DDuce.inc}

interface

uses
{$IFDEF DSHARP}
  DSharp.Collections, DSharp.Bindings,
  DSharp.Core.Events,
  DSharp.Core.Validations,
{$ENDIF}

{$IFDEF SPRING}
  Spring, Spring.Collections, Spring.Collections.Lists,
{$ENDIF}

  Vcl.Controls;

{$IFDEF DSHARP}
procedure AddControlBinding(
        ABindingGroup   : TBindingGroup;
        ASource         : TObject;
  const ASourceProp     : string;
        AControl        : TControl;
  const AControlProp    : string = 'Text';
        AValidationRule : IValidationRule = nil
);
{$ENDIF}

//procedure FillObjectListWithContacts(
//  AList  : TObjectList;
//  ACount : Integer
//);

implementation

{$IFDEF DSHARP}
procedure AddControlBinding(ABindingGroup: TBindingGroup; ASource: TObject;
  const ASourceProp: string; AControl: TControl; const AControlProp: string;
  AValidationRule : IValidationRule);
begin
  with ABindingGroup.Bindings.Add do
  begin
    Source                := ASource;
    SourcePropertyName    := ASourceProp;
    Target                := AControl;
    TargetPropertyName    := AControlProp;
    BindingMode           := bmTwoWay;

    NotifyOnSourceUpdated := False;
    NotifyOnTargetUpdated := False;
    if Assigned(AValidationRule) then
      ValidationRules.Add(AValidationRule);
  end;
end;
{$ENDIF}

(*
procedure FillObjectListWithContacts(AList: TObjectList; ACount : Integer);
var
  C : TContact;
//  P : TPhone;
  I : Integer;
begin
  if Assigned(AList) then
  begin
    AList.Clear;
    for I := 0 to ACount - 1 do
    begin
      C := TContact.Create;
      with C do
      begin
        FirstName   := RandomData.FirstName(gnMale);
        LastName    := RandomData.LastName;
        CompanyName := RandomData.CompanyName;
        Email       := RandomData.Email(FirstName, LastName);
        Address     := RandomData.Address;
        Number      := RandomData.Number(100);
//        if RandomData.Letter = 'a' then
//        begin
//          P := TPhone.Create;
//          P.Phone := RandomData.NumberString(8);
//          P.Kind  := RandomData.City;
//          C.Phones.Add(P);
//          P := TPhone.Create;
//          P.Phone := RandomData.NumberString(8);
//          P.Kind  := RandomData.City;
//          C.Phones.Add(P);
//          P := TPhone.Create;
//          P.Phone := RandomData.NumberString(8);
//          P.Kind  := RandomData.City;
//          C.Phones.Add(P);
//        end;
      end;
      AList.Add(C);
    end;
  end;
end;
    *)
end.
