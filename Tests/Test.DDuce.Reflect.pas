{
  Copyright (C) 2013-2024 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I Test.DDuce.inc}

unit Test.DDuce.Reflect;

{ TODO : test value assignments to reflected members. Assigned values are
  not assigned to the wrapped instance => bug }

interface

uses
  System.SysUtils, System.Classes,

  DDuce.Reflect,

  DUnitX.TestFramework,

  Test.Data;

type
  [Testfixture]
  TestReflect = class
  private
    FTestObject : TTestClass;
    FTestRecord : TTestRecord;

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { For the Real48 legacy type no RTTI is generated. }
    [Test][Ignore('For the Real48 legacy type no RTTI is generated.')]
    procedure Test_TypeName_method_for_Real48_argument;
    [Test]
    procedure Test_TypeName_method_for_Boolean_argument;
    [Test]
    procedure Test_TypeName_method_for_Byte_argument;
    [Test]
    procedure Test_TypeName_method_for_Cardinal_argument;
    [Test]
    procedure Test_TypeName_method_for_Currency_argument;
    [Test]
    procedure Test_TypeName_method_for_Double_argument;
    [Test]
    procedure Test_TypeName_method_for_Extended_argument;
    [Test]
    procedure Test_TypeName_method_for_Int64_argument;
    [Test]
    procedure Test_TypeName_method_for_Integer_argument;
    [Test]
    procedure Test_TypeName_method_for_LongInt_argument;
    [Test]
    procedure Test_TypeName_method_for_LongWord_argument;
    [Test]
    procedure Test_TypeName_method_for_NativeInt_argument;
    [Test]
    procedure Test_TypeName_method_for_NativeUInt_argument;
    [Test]
    procedure Test_TypeName_method_for_Real_argument;
    [Test]
    procedure Test_TypeName_method_for_ShortInt_argument;
    [Test]
    procedure Test_TypeName_method_for_Single_argument;
    [Test]
    procedure Test_TypeName_method_for_string_argument;
    [Test]
    procedure Test_TypeName_method_for_Variant_argument;
    [Test]
    procedure Test_TypeName_method_for_Word_argument;

    [Test]
    procedure Test_Fields_method_for_class_argument;
    [Test]
    procedure Test_Fields_method_for_record_argument;
    [Test]
    procedure Test_Fields_method_for_TValue_argument;

    [Test]
    procedure Test_Properties_method_for_class_argument;

  end;

implementation

uses
  System.Rtti,

  Test.Utils,

  DDuce.DynamicRecord;

{$REGION 'SetUp and TearDown methods'}
procedure TestReflect.Setup;
begin
  FTestObject := TTestUtils.CreateTestObject;
  FTestRecord := TTestUtils.CreateTestRecord;
end;

procedure TestReflect.TearDown;
begin
  FTestObject.Free;
end;
{$ENDREGION}

{$REGION 'Test Fields method'}
procedure TestReflect.Test_Fields_method_for_class_argument;
var
  DR : IDynamicRecord;
begin
  DR := Reflect.Fields(FTestObject);
  Assert.AreEqual(FTestObject.TestString, DR['FTestString'].AsString);
  Assert.AreEqual(FTestObject.TestBoolean, DR['FTestBoolean'].AsBoolean);
  Assert.AreEqual(FTestObject.TestChar, DR['FTestChar'].AsType<Char>);
  Assert.AreEqual(FTestObject.TestDateTime, DR['FTestDateTime'].AsType<TDateTime>);
  Assert.AreEqual(FTestObject.TestDouble, Double(DR['FTestDouble'].AsExtended));
  Assert.AreEqual(FTestObject.TestInteger, DR['FTestInteger'].AsInteger);
end;

procedure TestReflect.Test_Fields_method_for_record_argument;
var
  DR : IDynamicRecord;
begin
  DR := Reflect.Fields(FTestRecord);
  Assert.AreEqual(FTestRecord.TestString, DR['FTestString'].AsString);
  Assert.AreEqual(FTestRecord.TestBoolean, DR['FTestBoolean'].AsBoolean);
  Assert.AreEqual(FTestRecord.TestChar, DR['FTestChar'].AsType<Char>);
  Assert.AreEqual(FTestRecord.TestDateTime, DR['FTestDateTime'].AsType<TDateTime>);
  Assert.AreEqual(FTestRecord.TestDouble, Double(DR['FTestDouble'].AsExtended));
  Assert.AreEqual(FTestRecord.TestInteger, DR['FTestInteger'].AsInteger);
end;

procedure TestReflect.Test_Fields_method_for_TValue_argument;
var
  DR : IDynamicRecord;
  V  : TValue;
begin
  V := TValue.From(FTestRecord);
  DR := Reflect.Fields(V);
  Assert.AreEqual(FTestRecord.TestString, DR['FTestString'].AsString);
  Assert.AreEqual(FTestRecord.TestBoolean, DR['FTestBoolean'].AsBoolean);
  Assert.AreEqual(FTestRecord.TestChar, DR['FTestChar'].AsType<Char>);
  Assert.AreEqual(FTestRecord.TestDateTime, DR['FTestDateTime'].AsType<TDateTime>);
  Assert.AreEqual(FTestRecord.TestDouble,Double(DR['FTestDouble'].AsExtended));
  Assert.AreEqual(FTestRecord.TestInteger, DR['FTestInteger'].AsInteger);
end;
{$ENDREGION}

{$REGION 'Test Properties method'}
procedure TestReflect.Test_Properties_method_for_class_argument;
var
  DR : IDynamicRecord;
begin
  DR := Reflect.Properties(FTestObject);
  Assert.AreEqual(FTestObject.TestString, DR['TestString'].AsString);
  Assert.AreEqual(FTestObject.TestBoolean, DR['TestBoolean'].AsBoolean);
  Assert.AreEqual(FTestObject.TestChar, DR['TestChar'].AsType<Char>);
  Assert.AreEqual(FTestObject.TestDateTime, DR['TestDateTime'].AsType<TDateTime>);
  Assert.AreEqual(FTestObject.TestDouble, Double(DR['TestDouble'].AsExtended));
  Assert.AreEqual(FTestObject.TestInteger, DR['TestInteger'].AsInteger);

  // the same, but using the Data member to access the values
  Assert.AreEqual(FTestObject.TestBoolean, Boolean(DR.Data.TestBoolean));
  //Assert.AreEqual(FTestObject.TestChar, WideChar(DR.Data.TestChar));
  Assert.AreEqual(FTestObject.TestDateTime, TDateTime(DR.Data.TestDateTime));
  Assert.AreEqual(FTestObject.TestDouble, Double(DR.Data.TestDouble));
  Assert.AreEqual(FTestObject.TestInteger, Integer(DR.Data.TestInteger));
end;
{$ENDREGION}

{$REGION 'TypeName method'}
procedure TestReflect.Test_TypeName_method_for_Boolean_argument;
var
  T : Boolean;
begin
  T := True;
  Assert.AreEqual('Boolean', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Byte_argument;
var
  T : Byte;
begin
  T := 128;
  Assert.AreEqual('Byte', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Cardinal_argument;
var
  T : Cardinal;
begin
  T := 525;
  Assert.AreEqual('Cardinal', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Currency_argument;
var
  T : Currency;
begin
  T := 0;
  Assert.AreEqual('Currency', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Double_argument;
var
  T : Double;
begin
  T := 0;
  Assert.AreEqual('Double', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Extended_argument;
var
  T : Extended;
begin
  T := 0;
  Assert.AreEqual('Extended', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Int64_argument;
var
  T : Int64;
begin
  T := 0;
  Assert.AreEqual('Int64', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Integer_argument;
var
  T : Integer;
begin
  T := 0;
  Assert.AreEqual('Integer', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_LongInt_argument;
var
  T : LongInt;
begin
  T := 0;
  // will return Integer on all 32 bit and Win 64 bit and Int64 on 64bit iOS
  Assert.AreEqual('Integer', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_LongWord_argument;
var
  T : LongWord;
begin
  T := 0;
  // will return Cardinal on all 32 bit and Win 64 bit and UInt64 on 64bit iOS
  Assert.AreEqual('Cardinal', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_NativeInt_argument;
var
  T : NativeInt;
begin
  T := 0;
  Assert.AreEqual('NativeInt', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_NativeUInt_argument;
var
  T : NativeUInt;
begin
  T := 0;
  Assert.AreEqual('NativeUInt', Reflect.TypeName(T));
end;

{ For the Real48 legacy type no RTTI is generated. }

procedure TestReflect.Test_TypeName_method_for_Real48_argument;
var
  T : Real48;
begin
  T := 0;
  // no type info for Real48
  Assert.AreEqual('Real48', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Real_argument;
var
  T : Real;
begin
  T := 0;
  Assert.AreEqual('Real', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_ShortInt_argument;
var
  T : ShortInt;
begin
  T := 0;
  Assert.AreEqual('ShortInt', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Single_argument;
var
  T : Single;
begin
  T := 0;
  Assert.AreEqual('Single', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_string_argument;
var
  T : string;
begin
  T := '';
  Assert.AreEqual('string', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Variant_argument;
var
  T : Variant;
begin
  T := '';
  Assert.AreEqual('Variant', Reflect.TypeName(T));
end;

procedure TestReflect.Test_TypeName_method_for_Word_argument;
var
  T : Word;
begin
  T := 0;
  Assert.AreEqual('Word', Reflect.TypeName(T));
end;
{$ENDREGION}

end.
