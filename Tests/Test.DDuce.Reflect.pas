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

unit Test.DDuce.Reflect;

{$I Test.DDuce.inc}

{
  TODO : test value assignments to reflected members. Assigned values are
  not assigned to the wrapped instance => bug
}

interface

uses
  System.SysUtils, System.Classes,

  DDuce.Reflect,

  TestFramework,

  Test.Data;

type
  TestReflect = class(TTestCase)
  private
    FTestObject : TTestClass;
    FTestRecord : TTestRecord;

  public
    procedure SetUp; override;
    procedure TearDown; override;

    { For the Real48 legacy type no RTTI is generated. }
    procedure Test_TypeName_method_for_Real48_argument;

  published
    procedure Test_TypeName_method_for_Boolean_argument;
    procedure Test_TypeName_method_for_Byte_argument;
    procedure Test_TypeName_method_for_Cardinal_argument;
    procedure Test_TypeName_method_for_Currency_argument;
    procedure Test_TypeName_method_for_Double_argument;
    procedure Test_TypeName_method_for_Extended_argument;
    procedure Test_TypeName_method_for_Int64_argument;
    procedure Test_TypeName_method_for_Integer_argument;
    procedure Test_TypeName_method_for_LongInt_argument;
    procedure Test_TypeName_method_for_LongWord_argument;
    procedure Test_TypeName_method_for_NativeInt_argument;
    procedure Test_TypeName_method_for_NativeUInt_argument;
    procedure Test_TypeName_method_for_Real_argument;
    procedure Test_TypeName_method_for_ShortInt_argument;
    procedure Test_TypeName_method_for_Single_argument;
    procedure Test_TypeName_method_for_string_argument;
    procedure Test_TypeName_method_for_Variant_argument;
    procedure Test_TypeName_method_for_Word_argument;

    procedure Test_Fields_method_for_class_argument;
    procedure Test_Fields_method_for_record_argument;
    procedure Test_Fields_method_for_TValue_argument;

    procedure Test_Properties_method_for_class_argument;

  end;

implementation

uses
  System.Rtti,

  Test.Utils,

  DDuce.Logger, DDuce.DynamicRecord;

{$REGION 'SetUp and TearDown methods'}
procedure TestReflect.SetUp;
begin
  inherited SetUp;
  FTestObject := TTestUtils.CreateTestObject;
  FTestRecord := TTestUtils.CreateTestRecord;
end;

procedure TestReflect.TearDown;
begin
  inherited TearDown;
  FTestObject.Free;
end;
{$ENDREGION}

{$REGION 'Test Fields method'}
procedure TestReflect.Test_Fields_method_for_class_argument;
var
  DR : IDynamicRecord;
begin
  DR := Reflect.Fields(FTestObject);
  CheckEqualsString(FTestObject.TestString, DR['FTestString'].AsString);
  CheckEquals(FTestObject.TestBoolean, DR['FTestBoolean'].AsBoolean);
  CheckEquals(FTestObject.TestChar, DR['FTestChar'].AsType<Char>);
  CheckEquals(FTestObject.TestDateTime, DR['FTestDateTime'].AsType<TDateTime>);
  CheckEquals(FTestObject.TestDouble, DR['FTestDouble'].AsExtended);
  CheckEquals(FTestObject.TestInteger, DR['FTestInteger'].AsInteger);
end;

procedure TestReflect.Test_Fields_method_for_record_argument;
var
  DR : IDynamicRecord;
begin
  DR := Reflect.Fields(FTestRecord);
  CheckEqualsString(FTestRecord.TestString, DR['FTestString'].AsString);
  CheckEquals(FTestRecord.TestBoolean, DR['FTestBoolean'].AsBoolean);
  CheckEquals(FTestRecord.TestChar, DR['FTestChar'].AsType<Char>);
  CheckEquals(FTestRecord.TestDateTime, DR['FTestDateTime'].AsType<TDateTime>);
  CheckEquals(FTestRecord.TestDouble, DR['FTestDouble'].AsExtended);
  CheckEquals(FTestRecord.TestInteger, DR['FTestInteger'].AsInteger);
end;

procedure TestReflect.Test_Fields_method_for_TValue_argument;
var
  DR : IDynamicRecord;
  V  : TValue;
begin
  V := TValue.From(FTestRecord);
  DR := Reflect.Fields(V);
  CheckEqualsString(FTestRecord.TestString, DR['FTestString'].AsString);
  CheckEquals(FTestRecord.TestBoolean, DR['FTestBoolean'].AsBoolean);
  CheckEquals(FTestRecord.TestChar, DR['FTestChar'].AsType<Char>);
  CheckEquals(FTestRecord.TestDateTime, DR['FTestDateTime'].AsType<TDateTime>);
  CheckEquals(FTestRecord.TestDouble, DR['FTestDouble'].AsExtended);
  CheckEquals(FTestRecord.TestInteger, DR['FTestInteger'].AsInteger);
end;
{$ENDREGION}

{$REGION 'Test Properties method'}
procedure TestReflect.Test_Properties_method_for_class_argument;
var
  DR : IDynamicRecord;
begin
  DR := Reflect.Properties(FTestObject);
  CheckEqualsString(FTestObject.TestString, DR['TestString'].AsString);
  CheckEquals(FTestObject.TestBoolean, DR['TestBoolean'].AsBoolean);
  CheckEquals(FTestObject.TestChar, DR['TestChar'].AsType<Char>);
  CheckEquals(FTestObject.TestDateTime, DR['TestDateTime'].AsType<TDateTime>);
  CheckEquals(FTestObject.TestDouble, DR['TestDouble'].AsExtended);
  CheckEquals(FTestObject.TestInteger, DR['TestInteger'].AsInteger);

  // the same, but using the Data member to access the values
  CheckEquals(FTestObject.TestBoolean, DR.Data.TestBoolean);
  CheckEquals(FTestObject.TestChar, DR.Data.TestChar);
  CheckEquals(FTestObject.TestDateTime, DR.Data.TestDateTime);
  CheckEquals(FTestObject.TestDouble, DR.Data.TestDouble);
  CheckEquals(FTestObject.TestInteger, DR.Data.TestInteger);
end;
{$ENDREGION}

{$REGION 'TypeName method'}
procedure TestReflect.Test_TypeName_method_for_Boolean_argument;
var
  T : Boolean;
begin
  T := True;
  CheckEqualsString('Boolean', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Byte_argument;
var
  T : Byte;
begin
  T := 128;
  CheckEqualsString('Byte', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Cardinal_argument;
var
  T : Cardinal;
begin
  T := 525;
  CheckEqualsString('Cardinal', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Currency_argument;
var
  T : Currency;
begin
  T := 0;
  CheckEqualsString('Currency', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Double_argument;
var
  T : Double;
begin
  T := 0;
  CheckEqualsString('Double', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Extended_argument;
var
  T : Extended;
begin
  T := 0;
  CheckEqualsString('Extended', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Int64_argument;
var
  T : Int64;
begin
  T := 0;
  CheckEqualsString('Int64', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Integer_argument;
var
  T : Integer;
begin
  T := 0;
  CheckEqualsString('Integer', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_LongInt_argument;
var
  T : LongInt;
begin
  T := 0;
  // will return Integer on all 32 bit and Win 64 bit and Int64 on 64bit iOS
  CheckEqualsString('Integer', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_LongWord_argument;
var
  T : LongWord;
begin
  T := 0;
  // will return Cardinal on all 32 bit and Win 64 bit and UInt64 on 64bit iOS
  CheckEqualsString('Cardinal', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_NativeInt_argument;
var
  T : NativeInt;
begin
  T := 0;
  CheckEqualsString('NativeInt', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_NativeUInt_argument;
var
  T : NativeUInt;
begin
  T := 0;
  CheckEqualsString('NativeUInt', Reflect.TypeName(T));
end;

{ For the Real48 legacy type no RTTI is generated. }

procedure TestReflect.Test_TypeName_method_for_Real48_argument;
var
  T : Real48;
begin
  T := 0;
  // no type info for Real48
  CheckEqualsString('Real48', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Real_argument;
var
  T : Real;
begin
  T := 0;
  CheckEqualsString('Real', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_ShortInt_argument;
var
  T : ShortInt;
begin
  T := 0;
  CheckEqualsString('ShortInt', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Single_argument;
var
  T : Single;
begin
  T := 0;
  CheckEqualsString('Single', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_string_argument;
var
  T : string;
begin
  T := '';
  CheckEqualsString('string', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Variant_argument;
var
  T : Variant;
begin
  T := '';
  CheckEqualsString('Variant', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Word_argument;
var
  T : Word;
begin
  T := 0;
  CheckEqualsString('Word', Reflect.TypeName(T));
end;
{$ENDREGION}

end.
