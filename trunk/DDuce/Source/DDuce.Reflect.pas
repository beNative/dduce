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

unit DDuce.Reflect;

{$I DDuce.inc}

{
  Reflect provides some static routines to expose the contents of an object
  using the RTTI.
}

interface

uses
  System.Rtti,

  DDuce.DynamicRecord;

type
  Reflect = record
  class var
    FRttiContext: TRttiContext;

    class function EnumName<T>(const AArg: T): string; static;
    class function OrdValue<T>(const AArg: T): Integer; static;
    class function SetElementNames<T>(const AArg: T): string; static;

    class function Fields<T>(const AArg: T): IDynamicRecord; static;
    class function Properties<T: class, constructor>(
      const AArg: T
    ): IDynamicRecord<T>; overload; static;
  end;

implementation

uses
  System.TypInfo;

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

class function Reflect.SetElementNames<T>(const AArg: T): string;
begin
  // TODO
end;

end.


