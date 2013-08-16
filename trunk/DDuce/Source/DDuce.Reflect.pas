{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Reflect;

{$I DDuce.inc}

{
  Reflect provides some static routines to expose the contents of an object
  using the RTTI.
}

//*****************************************************************************

interface

uses
  Rtti,

  DDuce.DynamicRecord;

type
  Reflect = record
  class var
    FRttiContext: TRttiContext;

    class function EnumName<T>(const AArg: T): string; static;
    class function OrdValue<T>(const AArg: T): Integer; static;
    class function SetElementNames<T>(const AArg: T): string; static;

    class function Fields<T>(const AArg: T): IDynamicRecord; static;
//    class function Properties<T: class, constructor>(const AArg: T): IDynamicRecord; overload; static;
    class function Properties<T: class, constructor>(const AArg: T): IDynamicRecord<T>; overload; static;
  end;

//*****************************************************************************

implementation

uses
  TypInfo, Dialogs;

{ Returns the type name of a given enumerated type instance. }

class function Reflect.EnumName<T>(const AArg: T): string;
begin
  Result := GetEnumName(TypeInfo(T), OrdValue(AArg));
end;

// fields are read-only for the moment.

{ Returns the fields of the given instance (record or object). }

class function Reflect.Fields<T>(const AArg: T): IDynamicRecord;
var
  R : TRecord;
  V : TValue;
begin
  V := TValue.From<T>(AArg);
  R.Assign(V, False, True, True, []);
  Result := R;
end;

class function Reflect.OrdValue<T>(const AArg: T): Integer;
var
  V: TValue;
begin
  Result := 0;
  V := TValue.From<T>(AArg);
  Result := V.AsOrdinal;
end;

{ Returns the properties of the given instance (record or object). }

class function Reflect.Properties<T>(const AArg: T): IDynamicRecord<T>;
var
  R : IDynamicRecord<T>;
begin
  R := TDynamicRecord<T>.Create(AArg);
  Result := R;
end;

//class function Reflect.Properties<T>(const AArg: T): IDynamicRecord;
//var
//  R : IDynamicRecord<T>;
//begin
//  R := TDynamicRecord<T>.Create(AArg);
//  Result := R;
//
//
////  var
////  R : TRecord;
////  V : TValue;
////begin
////  V := TValue.From<T>(AArg);
////  R.Assign(V, True, False, True, []);
////  Result := R;
//end;

class function Reflect.SetElementNames<T>(const AArg: T): string;
begin

end;

end.

{
function AsPropString(AValue: TValue): string;
var
  P             : TRttiProperty;
  S             : string;
  V             : TValue;
  V2            : TValue;
  N             : Integer;
  ExcludedTypes : TTypeKinds;
begin
  Result := '';
  if not AValue.IsEmpty then
  begin
    ExcludedTypes := [
      tkClassRef, tkMethod, tkInterface, tkPointer, tkUnknown, tkArray,
      tkDynArray, tkClass
    ];
    N := 0;
    for P in FRtti.GetType(AValue.TypeInfo).GetProperties do
    begin
      if not (P.PropertyType.TypeKind in ExcludedTypes) and P.IsReadable then
      begin
        if Length(P.Name) > N then
          N := Length(P.Name);
      end;
    end;
    for P in FRtti.GetType(AValue.TypeInfo).GetProperties do
    begin
      if not (P.PropertyType.TypeKind in ExcludedTypes) and P.IsReadable then
      begin
        try
        if AValue.IsObject then
          V := P.GetValue(AValue.AsObject)
        else
          V := P.GetValue(AValue.GetReferenceToRawData);
        except
          S := '<error reading value>';
          AppendLine(Result, Format('%-*s = %s', [N, P.Name, S]));
          Continue;
        end;
        try
        if V.Kind = tkClass then
        begin
          S := P.Name + ': ' + V.AsObject.ClassName;
        end
        else if V.Kind = tkVariant then
          S := VarToStrDef(V.AsVariant, '')
        // don't append #0 characters to the result.
        else if (V.Kind in [tkChar, tkWChar]) and (V.ToString = #0) then
          S := ''
        else if V.Kind = tkRecord then
        begin
          if TryGetUnderlyingValue(V, V2) then
          begin
            if (V2.Kind in [tkChar, tkWChar]) and (V.ToString = #0) then
              S := ''
            else
              S := V2.ToString;
          end
          else
          begin
              S := '<error while executing TryGetUnderlyingValue>'
          end
        end
        else
          S := V.ToString;
        except
          S := '<error reading value>';
          AppendLine(Result, Format('%-*s = %s', [N, P.Name, S]));
          Continue;
        end;
        if S <> '' then
          AppendLine(Result, Format('%-*s = %s', [N, P.Name, S]));
      end;
    end;
  end;
end;
}
