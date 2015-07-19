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

unit Demo.Helpers;

{$I ..\Source\DDuce.inc}

interface

uses
{$IFDEF DSHARP}
  //DSharp.Collections,
  DSharp.Bindings,
  //DSharp.Core.Events,
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
