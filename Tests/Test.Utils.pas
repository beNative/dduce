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

unit Test.Utils;

{ Common routines used in our set of unit tests. }

{$I Test.DDuce.inc}

interface

uses
  System.Rtti,
  Data.DB,

  Test.Data;

type
  TTestUtils = record
    class function CreateDataSet(ARecordCount: Integer): TDataSet; static;
    class function CreateTestObject: TTestClass; static;
    class function CreateTestRecord: TTestRecord; static;

  end;

function AsPropString(AValue: TValue): string;

function AsFieldString(AValue: TValue): string;

implementation

uses
  System.SysUtils, System.TypInfo, System.StrUtils, System.Variants,
  System.Classes,
  DataSnap.DBClient,

  DDuce.RandomData;

var
  FRtti: TRttiContext;

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

class function TTestUtils.CreateTestObject: TTestClass;
begin
  Result := TTestClass.Create;
  Result.TestBoolean  := True;
  Result.TestChar     := RandomData.Letter;
  Result.TestDateTime := Now;
  Result.TestDouble   := Pi;
  Result.TestInteger  := RandomData.Number(100);
  Result.TestString   := RandomData.Vegetable;
end;

class function TTestUtils.CreateTestRecord: TTestRecord;
begin
  Result.TestBoolean  := True;
  Result.TestChar     := RandomData.Letter;
  Result.TestDateTime := Now;
  Result.TestDouble   := Pi;
  Result.TestInteger  := RandomData.Number(100);
  Result.TestString   := RandomData.Vegetable;
end;

procedure AppendLine(var AToString : string; const ALine : string); overload;
begin
  if ALine <> '' then
  begin
    AToString := IfThen(AToString = '', ALine, AToString + #13#10 + ALine);
  end;
end;

procedure AppendLine(var AToString : string; const ALine : string;
  const AArgs : array of const ); overload;
begin
  AppendLine(AToString, Format(ALine, AArgs));
end;

function TryGetUnderlyingValue(const AValue: TValue; out AInnerValue: TValue)
  : Boolean;
var
  T : TRttiType;
  F : TRttiField;
begin
  T := FRtti.GetType(AValue.TypeInfo);
  F := T.GetField('FValue');
  Result := False;

  if Assigned(F) then
  begin
    AInnerValue := F.GetValue(AValue.GetReferenceToRawData);
    Result := True;
  end
  else
  begin
    AInnerValue := TValue.Empty;
  end;
end;

{ Returns all property values of the instance in a string. The instance can
  be an object or a record. A record can be passed as a TValue using
  TValue.From<record-type>(record-instance). For an object you just pass the
  object instance as the argument.

  Example of a TValue variable holding a TRect record and an object:

    var
      V: TValue;
      R: TRect;
      S: string;
      O: TButton;
    begin
      ...
      V := V.From<TRect>(R);     // record types need to be casted to TValue
      S := AsPropString(V);
      ...
      O := TButton.Create(nil);
      try
        S := AsPropString(O);    // object types can be passed directly
      finally
        FreeAndNil(O);
      end;
    end;
}

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
    // REMARK: GetProperties only works for class types, not record types!
    // This is a known bug by Embarcadero and will be fixed in future releases.
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

{ Returns all field values of the given instance in a string. The instance can
  be an object or a record. A record can be passed as a TValue using
  TValue.From<record-type>(record-instance). For an object you just pass the
  object instance as the argument.

  Example of a TValue variable holding a TRect record an object:

    var
      V: TValue;
      R: TRect;
      S: string;
      O: TButton;
    begin
      ...
      V := V.From<TRect>(R);      // record types need to be casted to TValue
      S := AsFieldString(V);
      ...
      O := TButton.Create(nil);
      try
        S := AsFieldString(O);    // object types can be passed directly
      finally
        FreeAndNil(O);
      end;
    end;
}

function AsFieldString(AValue : TValue): string;
var
  F             : TRttiField;
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
      tkDynArray
    ];
    N := 0;
    for F in FRtti.GetType(AValue.TypeInfo).GetFields do
    begin
      if not (F.FieldType.TypeKind in ExcludedTypes) then
      begin
        if Length(F.Name) > N then
          N := Length(F.Name);
      end;
    end;
    for F in FRtti.GetType(AValue.TypeInfo).GetFields do
    begin
      if not (F.FieldType.TypeKind in ExcludedTypes) then
      begin
        if AValue.IsObject then
          V := F.GetValue(AValue.AsObject)
        else
          V := F.GetValue(AValue.GetReferenceToRawData);
        if V.Kind = tkClass then
        begin
          if V.AsObject is TComponent then
            S := TComponent(V.AsObject).Name + ': ' + V.AsObject.ClassName;
        end
        // don't append #0 characters to the result.
        else if (V.Kind in [tkChar, tkWChar]) and (V.ToString = #0) then
          S := ''
        else if V.Kind = tkVariant then
          S := VarToStrDef(V.AsVariant, '')
        else if V.Kind = tkRecord then
        begin
          if TryGetUnderlyingValue(V, V2) then
          begin
            if (V2.Kind in [tkChar, tkWChar]) and (V2.ToString = #0) then
              S := ''
            else
               S := V2.ToString;
          end
          else
          begin
            //raise Exception.Create('no TryGetUnderlyingValue');
          end
        end
        else
          S := V.ToString;
        AppendLine(Result, Format('%-*s = %s', [N, F.Name, S]));
      end;
    end;
  end;
end;

end.
