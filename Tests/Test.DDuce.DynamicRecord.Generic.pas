{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Test.DDuce.DynamicRecord.Generic;

interface

{
  Current limitations of DynamicRecord<T>:

    - FromStrings should cast to the field type

   In a standard DynamicRecord the data is stored in a collection of named TValue
   variables.

   In the generic version the data is stored in (a copy of the instance of)
   the provided type.
}

uses
  Winapi.Windows, Winapi.Messages,
  System.Variants, System.SysUtils, System.Contnrs, System.Classes,
  Data.DB,
  Datasnap.DBClient,
  Vcl.Controls, Vcl.Dialogs, Vcl.Forms, Vcl.Buttons, Vcl.ActnList, Vcl.StdCtrls,
  Vcl.Graphics,

  DDuce.DynamicRecord,

  DUnitX.TestFramework,

  Test.Data;

type
  {$M+}
  [TestFixture]
  TestGenericDynamicRecord = class
  private
    FObject        : TTestClass;
    FRecord        : DynamicRecord<TTestClass>;
    FDynamicRecord : IDynamicRecord<TTestClass>;

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test][Ignore('Not implemented yet')]
    procedure Test_FromStrings_method;

    [Test]
    procedure Test_ContainsField_method;
    [Test]
    procedure Test_IsEmpty_method;

    // test methods with automatic conversion to destination type
    [Test]
    procedure Test_ToFloat_Method;
    [Test]
    procedure Test_ToInteger_method;
    [Test]
    procedure Test_ToString_method;
    [Test]
    procedure Test_ToBoolean_method;
    [Test]
    procedure Test_IsBlank_method;

    [Test]
    procedure Test_ToStrings_method;

    [Test]
    procedure Test_assignment_operator_for_generic_DynamicRecord_to_DynamicRecord;
    [Test]
    procedure Test_assignment_operator_for_DynamicRecord_to_generic_DynamicRecord;
    [Test]
    procedure Test_assignment_operator_for_generic_DynamicRecord_to_generic_DynamicRecord;
    [Test]
    procedure Test_assignment_operator_for_generic_IDynamicRecord_to_generic_IDynamicRecord;
    [Test]
    procedure Test_assignment_operator_for_IDynamicRecord_to_generic_IDynamicRecord;

    [Test]
    procedure Test_Assign_method_for_IDynamicRecord_argument;
    [Test]
    procedure Test_Assign_method_for_DynamicRecord_argument;
    [Test]
    procedure Test_Assign_method_for_generic_IDynamicRecord_argument;
    [Test]
    procedure Test_Assign_method_for_generic_DynamicRecord_argument;
    [Test]
    procedure Test_Assign_method_for_generic_argument;

    [Test]
    procedure Test_Count_Property;

  end;

implementation

uses
  System.Math, System.Types, System.Rtti,

  Spring;

const
  TEST_INTEGER            = 'TestInteger';
  TEST_DOUBLE             = 'TestDouble';
  TEST_BOOLEAN            = 'TestBoolean';
  TEST_STRING             = 'TestString';
  TEST_TVALUE             = 'TestTValue';
  TEST_VARIANT            = 'TestVariant';
  TEST_NON_EXISTENT_VALUE = 'TestNonExistentValue';

{$REGION 'public methods'}
{$REGION 'SetUp and TearDown methods'}
procedure TestGenericDynamicRecord.Setup;
begin
  FObject := TTestClass.Create;
  FObject.TestBoolean := True;
  FObject.TestString  := 'Test';
  FObject.TestInteger := 5;
  FObject.TestDouble  := 3.14;

  FRecord.Data.TestBoolean := True;
  FRecord.Data.TestString  := 'Test';
  FRecord.Data.TestInteger := 5;
  FRecord.Data.TestDouble  := 3.14;
  FRecord.Data.TestDateTime := Now;
  FRecord.Data.TestChar     := 'C';

  FDynamicRecord := DynamicRecord<TTestClass>.CreateDynamicRecord;
  FDynamicRecord.Data.TestBoolean := True;
  FDynamicRecord.Data.TestString  := 'Test';
  FDynamicRecord.Data.TestInteger := 5;
  FDynamicRecord.Data.TestDouble  := 3.14;
  FDynamicRecord.Data.TestDateTime := Now;
  FDynamicRecord.Data.TestChar     := 'C';
end;

procedure TestGenericDynamicRecord.TearDown;
begin
  FreeAndNil(FObject);
  FDynamicRecord := nil;
  FRecord.Clear;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'Test methods'}
procedure TestGenericDynamicRecord.Test_ContainsField_method;
begin
  Assert.IsTrue(FRecord.ContainsField('TestChar'));
end;

procedure TestGenericDynamicRecord.Test_Count_Property;
begin
  Assert.AreEqual(6, FRecord.Count);
end;

procedure TestGenericDynamicRecord.Test_FromStrings_method;
var
  SL : TStrings;
begin
  SL := TStringList.Create;
  try
    SL.Values[TEST_INTEGER] := '5';
    SL.Values[TEST_STRING] := 'Test';
    FRecord.FromStrings(SL, True);
    Assert.AreEqual(FRecord.ToInteger(TEST_INTEGER), 5);
    Assert.AreEqual(FRecord.ToString(TEST_STRING), 'Test');
  finally
    SL.Free;
  end;
end;

procedure TestGenericDynamicRecord.Test_IsBlank_method;
begin
  FRecord.Data.TestString := '';
  Assert.IsTrue(FRecord.IsBlank(TEST_STRING));
end;

{ TODO: IsEmpty has little meaning in the generic version. }

procedure TestGenericDynamicRecord.Test_IsEmpty_method;
begin
  Assert.IsFalse(FRecord.IsEmpty);
end;

procedure TestGenericDynamicRecord.Test_ToBoolean_method;
begin
  Assert.IsTrue(FRecord.ToBoolean(TEST_STRING, True), TEST_STRING);
  Assert.IsTrue(FRecord.ToBoolean(TEST_DOUBLE, True), TEST_DOUBLE);
  Assert.IsTrue(FRecord.ToBoolean(TEST_BOOLEAN, True), TEST_BOOLEAN);
  Assert.IsTrue(FRecord.ToBoolean(TEST_INTEGER, True), TEST_INTEGER);
end;

procedure TestGenericDynamicRecord.Test_ToFloat_Method;
begin
  Assert.AreEqual(Double(5), FRecord.ToFloat(TEST_INTEGER), TEST_INTEGER);
  Assert.IsTrue(IsZero(FRecord.ToFloat(TEST_STRING)), TEST_STRING);
  Assert.IsTrue(IsZero(FRecord.ToFloat(TEST_BOOLEAN)), TEST_BOOLEAN);
  Assert.AreEqual(3.14, FRecord.ToFloat(TEST_DOUBLE), 0.001);
end;

procedure TestGenericDynamicRecord.Test_ToInteger_method;
begin
  Assert.AreEqual(1, FRecord.ToInteger(TEST_BOOLEAN), TEST_BOOLEAN);
  Assert.AreEqual(5, FRecord.ToInteger(TEST_INTEGER), TEST_INTEGER);
  Assert.AreEqual(0, FRecord.ToInteger(TEST_STRING), TEST_STRING);
  // float values are not rounded or trunked when converting. Conversion fails.
  Assert.AreEqual(0, FRecord.ToInteger(TEST_DOUBLE), TEST_DOUBLE);
end;

procedure TestGenericDynamicRecord.Test_ToString_method;
begin
  Assert.AreEqual('True', FRecord.ToString(TEST_BOOLEAN), TEST_BOOLEAN);
  Assert.AreEqual('5', FRecord.ToString(TEST_INTEGER), TEST_INTEGER);
  Assert.AreEqual('Test', FRecord.ToString(TEST_STRING), TEST_STRING);
  Assert.AreEqual('3,14', FRecord.ToString(TEST_DOUBLE), TEST_DOUBLE);
end;

procedure TestGenericDynamicRecord.Test_ToStrings_method;
var
  SL : TStrings;
begin
  SL := TStringList.Create;
  try
    FRecord.ToStrings(SL);
    Assert.AreEqual('True', SL.Values[TEST_BOOLEAN], TEST_BOOLEAN);
    Assert.AreEqual('5', SL.Values[TEST_INTEGER], TEST_INTEGER);
    Assert.AreEqual('Test', SL.Values[TEST_STRING], TEST_STRING);
    Assert.AreEqual('3,14', SL.Values[TEST_DOUBLE], TEST_DOUBLE);
  finally
    SL.Free;
  end;
end;
{$ENDREGION}

{$REGION 'Test assignment operator'}
procedure TestGenericDynamicRecord.Test_assignment_operator_for_generic_DynamicRecord_to_DynamicRecord;
var
  R : DynamicRecord;
  F : IDynamicField;
begin
  R := FRecord;
  for F in FRecord do
  begin
    Assert.IsTrue(F.Value.Equals(R[F.Name]));
  end;
end;

procedure TestGenericDynamicRecord.Test_assignment_operator_for_DynamicRecord_to_generic_DynamicRecord;
var
  R : DynamicRecord;
  F : IDynamicField;
begin
  R.Data.TestBoolean  := True;
  R.Data.TestChar     := 'C';
  R.Data.TestDateTime := Now;
  R.Data.TestDouble   := Pi;
  R.Data.TestInteger  := 5;
  R.Data.TestString   := 'Test';
  FRecord := R;
  for F in FRecord do
  begin
    Assert.IsTrue(F.Value.Equals(R[F.Name]), F.Name);
  end;
  R[TEST_INTEGER] := 125;
  Assert.IsFalse(R.Data.TestInteger = FRecord.Data.TestInteger);
end;

procedure TestGenericDynamicRecord.Test_assignment_operator_for_generic_DynamicRecord_to_generic_DynamicRecord;
var
  R : DynamicRecord<TTestClass>;
  F : IDynamicField;
begin
  R := FRecord;
  for F in FRecord do
  begin
    Assert.IsTrue(F.Value.Equals(R[F.Name]), F.Name);
  end;
  R.Data.TestInteger := 0;
  Assert.AreNotEqual(R.Data.TestInteger, FRecord.Data.TestInteger);
end;

procedure TestGenericDynamicRecord.Test_assignment_operator_for_generic_IDynamicRecord_to_generic_IDynamicRecord;
var
  DR : IDynamicRecord<TTestClass>;
  F  : IDynamicField;
begin
  DR := DynamicRecord<TTestClass>.CreateDynamicRecord;
  DR := FDynamicRecord; // reference is copied
  for F in FDynamicRecord do
  begin
    Assert.IsTrue(F.Value.Equals(DR[F.Name]), F.Name);
  end;
  DR.Data.TestInteger := 78;
  Assert.AreEqual(DR.Data.TestInteger, FDynamicRecord.Data.TestInteger);
end;

procedure TestGenericDynamicRecord.Test_assignment_operator_for_IDynamicRecord_to_generic_IDynamicRecord;
var
  DR : IDynamicRecord;
  F  : IDynamicField;
begin
  DR := DynamicRecord.CreateDynamicRecord; // is not needed, but added for clarity

  DR := FDynamicRecord; // reference is copied
  for F in FDynamicRecord do
  begin
    Assert.IsTrue(F.Value.Equals(DR[F.Name]), F.Name);
  end;
  DR.Data.TestInteger := 78;
  Assert.AreEqual(Integer(DR.Data.TestInteger), Integer(FDynamicRecord.Data.TestInteger));
end;
{$ENDREGION}

{$REGION 'Test Assign method'}
procedure TestGenericDynamicRecord.Test_Assign_method_for_IDynamicRecord_argument;
var
  DR : IDynamicRecord;
begin
  DR := DynamicRecord.CreateDynamicRecord;
  DR.Data.TestInteger := 5;
  DR.Data.TestString  := 'Test';
  FRecord.Assign(DR);
  Assert.AreEqual(DR.ToInteger(TEST_INTEGER), FRecord.Data.TestInteger);
  Assert.AreEqual(DR.ToString(TEST_STRING), FRecord.Data.TestString);
  DR.Data.TestInteger := 48;
  Assert.AreNotEqual(DR.ToInteger(TEST_INTEGER), FRecord.Data.TestInteger);
end;

procedure TestGenericDynamicRecord.Test_Assign_method_for_DynamicRecord_argument;
var
  R : DynamicRecord;
begin
  R.Data.TestInteger := 8;
  R.Data.TestString  := 'Foo';
  FRecord.Assign(R);
  Assert.AreEqual(R.ToInteger(TEST_INTEGER), FRecord.Data.TestInteger);
  Assert.AreEqual(R.ToString(TEST_STRING), FRecord.Data.TestString);
  R.Data.TestString  := 'Value';
  Assert.AreNotEqual(R.ToString(TEST_STRING), FRecord.Data.TestString);
end;

procedure TestGenericDynamicRecord.Test_Assign_method_for_generic_IDynamicRecord_argument;
var
  DR : IDynamicRecord<TTestClass>;
begin
  DR := DynamicRecord<TTestClass>.CreateDynamicRecord;
  DR.Data.TestInteger := 7;
  DR.Data.TestString  := 'Some test';
  FRecord.Assign(DR);
  Assert.AreEqual(DR.Data.TestInteger, FRecord.Data.TestInteger);
  Assert.AreEqual(DR.Data.TestString, FRecord.Data.TestString);
end;

procedure TestGenericDynamicRecord.Test_Assign_method_for_generic_DynamicRecord_argument;
var
  R : DynamicRecord<TTestClass>;
begin
  R.Data.TestInteger := 1;
  R.Data.TestString  := 'Another test';
  FRecord.Assign(R);
  Assert.AreEqual(R.Data.TestInteger, FRecord.Data.TestInteger);
  Assert.AreEqual(R.Data.TestString, FRecord.Data.TestString);
  R.Data.TestInteger := 2;
  Assert.AreNotEqual(R.Data.TestInteger, FRecord.Data.TestInteger);
end;

procedure TestGenericDynamicRecord.Test_Assign_method_for_generic_argument;
begin
  FRecord.Assign(FObject);
  Assert.AreEqual(FObject.TestInteger, FRecord.Data.TestInteger);
  FObject.TestInteger := 0;
  Assert.AreNotEqual(FObject.TestInteger, FRecord.Data.TestInteger);
end;
{$ENDREGION}

end.

