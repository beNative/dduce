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

unit Test.DDuce.DynamicRecord.Generic;

{$I Test.DDuce.inc}

interface

{
  Current limitations of TRecord<T>:

    - FromStrings should cast to the field type


   In a standard TRecord the data is stored in a collection of named TValue
   variables.

   In the generic version the data is stored in the provided type.
}

uses
  Winapi.Windows, Winapi.Messages,
  System.Variants, System.SysUtils, System.Contnrs, System.Classes,
  Data.DB,
  Datasnap.DBClient,
  Vcl.Controls, Vcl.Dialogs, Vcl.Forms, Vcl.Buttons, Vcl.ActnList, Vcl.StdCtrls,
  Vcl.Graphics,

  DDuce.DynamicRecord,

  TestFramework,

  Test.Data;

type
  TestGenericTRecord = class(TTestCase)
  private
    FObject        : TTestClass;
    FRecord        : TRecord<TTestClass>;
    FDynamicRecord : IDynamicRecord<TTestClass>;

  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure Test_FromStrings_method; // fails, not implemented

  published
    procedure Test_ContainsField_method;
    procedure Test_IsEmpty_method;

    // test methods with automatic conversion to destination type
    procedure Test_ToFloat_Method;
    procedure Test_ToInteger_method;
    procedure Test_ToString_method;
    procedure Test_ToBoolean_method;
    procedure Test_IsBlank_method;

    procedure Test_ToStrings_method;

    procedure Test_assignment_operator_for_generic_TRecord_to_TRecord;
    procedure Test_assignment_operator_for_TRecord_to_generic_TRecord;
    procedure Test_assignment_operator_for_generic_TRecord_to_generic_TRecord;
    procedure Test_assignment_operator_for_generic_IDynamicRecord_to_generic_IDynamicRecord;
    procedure Test_assignment_operator_for_IDynamicRecord_to_generic_IDynamicRecord;

    procedure Test_Assign_method_for_IDynamicRecord_argument;
    procedure Test_Assign_method_for_TRecord_argument;
    procedure Test_Assign_method_for_generic_IDynamicRecord_argument;
    procedure Test_Assign_method_for_generic_TRecord_argument;
    procedure Test_Assign_method_for_generic_argument;

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
procedure TestGenericTRecord.SetUp;
begin
  inherited SetUp;
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

  FDynamicRecord := TRecord<TTestClass>.CreateDynamicRecord;
  FDynamicRecord.Data.TestBoolean := True;
  FDynamicRecord.Data.TestString  := 'Test';
  FDynamicRecord.Data.TestInteger := 5;
  FDynamicRecord.Data.TestDouble  := 3.14;
  FDynamicRecord.Data.TestDateTime := Now;
  FDynamicRecord.Data.TestChar     := 'C';
end;

procedure TestGenericTRecord.TearDown;
begin
  FreeAndNil(FObject);
  FDynamicRecord := nil;
  inherited TearDown;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'Test methods'}
procedure TestGenericTRecord.Test_ContainsField_method;
begin
  CheckTrue(FRecord.ContainsField('TestChar'));
end;

procedure TestGenericTRecord.Test_Count_Property;
begin
  CheckEquals(6, FRecord.Count);
end;

procedure TestGenericTRecord.Test_FromStrings_method;
var
  SL : TStrings;
begin
  SL := TStringList.Create;
  try
    SL.Values[TEST_INTEGER] := '5';
    SL.Values[TEST_STRING] := 'Test';
    FRecord.FromStrings(SL);
    CheckEquals(FRecord.Data.TestInteger, 5);
    CheckEquals(FRecord.Data.TestString, 'Test');
  finally
    SL.Free;
  end;
end;

procedure TestGenericTRecord.Test_IsBlank_method;
begin
  FRecord.Data.TestString := '';
  CheckTrue(FRecord.IsBlank(TEST_STRING));
end;

{ TODO: IsEmpty has little meaning in the generic version. }

procedure TestGenericTRecord.Test_IsEmpty_method;
begin
  CheckFalse(FRecord.IsEmpty);
end;

procedure TestGenericTRecord.Test_ToBoolean_method;
begin
  CheckTrue(FRecord.ToBoolean(TEST_STRING, True), TEST_STRING);
  CheckTrue(FRecord.ToBoolean(TEST_DOUBLE, True), TEST_DOUBLE);
  CheckTrue(FRecord.ToBoolean(TEST_BOOLEAN, True), TEST_BOOLEAN);
  CheckTrue(FRecord.ToBoolean(TEST_INTEGER, True), TEST_INTEGER);
end;

procedure TestGenericTRecord.Test_ToFloat_Method;
begin
  CheckEquals(5, FRecord.ToFloat(TEST_INTEGER), TEST_INTEGER);
  CheckTrue(IsZero(FRecord.ToFloat(TEST_STRING)), TEST_STRING);
  CheckTrue(IsZero(FRecord.ToFloat(TEST_BOOLEAN)), TEST_BOOLEAN);
  CheckEquals(3.14, FRecord.ToFloat(TEST_DOUBLE), 0.001);
end;

procedure TestGenericTRecord.Test_ToInteger_method;
begin
  CheckEquals(1, FRecord.ToInteger(TEST_BOOLEAN), TEST_BOOLEAN);
  CheckEquals(5, FRecord.ToInteger(TEST_INTEGER), TEST_INTEGER);
  CheckEquals(0, FRecord.ToInteger(TEST_STRING), TEST_STRING);
  // float values are not rounded or trunked when converting. Conversion fails.
  CheckEquals(0, FRecord.ToInteger(TEST_DOUBLE), TEST_DOUBLE);
end;

procedure TestGenericTRecord.Test_ToString_method;
begin
  CheckEquals('True', FRecord.ToString(TEST_BOOLEAN), TEST_BOOLEAN);
  CheckEquals('5', FRecord.ToString(TEST_INTEGER), TEST_INTEGER);
  CheckEquals('Test', FRecord.ToString(TEST_STRING), TEST_STRING);
  CheckEquals('3,14', FRecord.ToString(TEST_DOUBLE), TEST_DOUBLE);
end;

procedure TestGenericTRecord.Test_ToStrings_method;
var
  SL : TStrings;
begin
  SL := TStringList.Create;
  try
    FRecord.ToStrings(SL);
    CheckEquals('True', SL.Values[TEST_BOOLEAN], TEST_BOOLEAN);
    CheckEquals('5', SL.Values[TEST_INTEGER], TEST_INTEGER);
    CheckEquals('Test', SL.Values[TEST_STRING], TEST_STRING);
    CheckEquals('3,14', SL.Values[TEST_DOUBLE], TEST_DOUBLE);
  finally
    SL.Free;
  end;
end;
{$ENDREGION}

{$REGION 'Test assignment operator'}
procedure TestGenericTRecord.Test_assignment_operator_for_generic_TRecord_to_TRecord;
var
  R : TRecord;
  F : IDynamicField;
begin
  R := FRecord;
  for F in FRecord do
  begin
    CheckTrue(F.Value.Equals(R[F.Name]));
  end;
end;

procedure TestGenericTRecord.Test_assignment_operator_for_TRecord_to_generic_TRecord;
var
  R : TRecord;
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
    CheckTrue(F.Value.Equals(R[F.Name]), F.Name);
  end;
  R[TEST_INTEGER] := 125;
  CheckFalse(R.Data.TestInteger = FRecord.Data.TestInteger);
end;

procedure TestGenericTRecord.Test_assignment_operator_for_generic_TRecord_to_generic_TRecord;
var
  R : TRecord<TTestClass>;
  F : IDynamicField;
begin
  R := FRecord;
  for F in FRecord do
  begin
    CheckTrue(F.Value.Equals(R[F.Name]), F.Name);
  end;
  R.Data.TestInteger := 0;
  CheckNotEquals(R.Data.TestInteger, FRecord.Data.TestInteger);
end;

procedure TestGenericTRecord.Test_assignment_operator_for_generic_IDynamicRecord_to_generic_IDynamicRecord;
var
  DR : IDynamicRecord<TTestClass>;
  F : IDynamicField;
begin
  DR := TRecord<TTestClass>.CreateDynamicRecord;
  DR := FDynamicRecord; // reference is copied
  for F in FDynamicRecord do
  begin
    CheckTrue(F.Value.Equals(DR[F.Name]), F.Name);
  end;
  DR.Data.TestInteger := 78;
  CheckEquals(DR.Data.TestInteger, FDynamicRecord.Data.TestInteger);
end;

procedure TestGenericTRecord.Test_assignment_operator_for_IDynamicRecord_to_generic_IDynamicRecord;
var
  DR : IDynamicRecord;
  F  : IDynamicField;
begin
  DR := TRecord.CreateDynamicRecord; // is not needed, but added for clarity

  DR := FDynamicRecord; // reference is copied
  for F in FDynamicRecord do
  begin
    CheckTrue(F.Value.Equals(DR[F.Name]), F.Name);
  end;
  //DR.Data.TestInteger := 78;
  CheckEquals(DR.Data.TestInteger, FDynamicRecord.Data.TestInteger);
end;
{$ENDREGION}

{$REGION 'Test Assign method'}
procedure TestGenericTRecord.Test_Assign_method_for_IDynamicRecord_argument;
var
  DR : IDynamicRecord;
begin
  DR := TRecord.CreateDynamicRecord;
  DR.Data.TestInteger := 5;
  DR.Data.TestString  := 'Test';
  FRecord.Assign(DR);
  CheckEquals(DR.ToInteger(TEST_INTEGER), FRecord.Data.TestInteger);
  CheckEquals(DR.ToString(TEST_STRING), FRecord.Data.TestString);
  DR.Data.TestInteger := 48;
  CheckNotEquals(DR.ToInteger(TEST_INTEGER), FRecord.Data.TestInteger);
end;

procedure TestGenericTRecord.Test_Assign_method_for_TRecord_argument;
var
  R : TRecord;
begin
  R.Data.TestInteger := 8;
  R.Data.TestString  := 'Foo';
  FRecord.Assign(R);
  CheckEquals(R.ToInteger(TEST_INTEGER), FRecord.Data.TestInteger);
  CheckEquals(R.ToString(TEST_STRING), FRecord.Data.TestString);
  R.Data.TestString  := 'Value';
  CheckNotEquals(R.ToString(TEST_STRING), FRecord.Data.TestString);
end;

procedure TestGenericTRecord.Test_Assign_method_for_generic_IDynamicRecord_argument;
var
  DR : IDynamicRecord<TTestClass>;
begin
  DR := TRecord<TTestClass>.CreateDynamicRecord;
  DR.Data.TestInteger := 7;
  DR.Data.TestString  := 'Some test';
  FRecord.Assign(DR);
  CheckEquals(DR.Data.TestInteger, FRecord.Data.TestInteger);
  CheckEquals(DR.Data.TestString, FRecord.Data.TestString);
end;

procedure TestGenericTRecord.Test_Assign_method_for_generic_TRecord_argument;
var
  R : TRecord<TTestClass>;
begin
  R.Data.TestInteger := 1;
  R.Data.TestString  := 'Another test';
  FRecord.Assign(R);
  CheckEquals(R.Data.TestInteger, FRecord.Data.TestInteger);
  CheckEquals(R.Data.TestString, FRecord.Data.TestString);
  R.Data.TestInteger := 2;
  CheckNotEquals(R.Data.TestInteger, FRecord.Data.TestInteger);
end;

procedure TestGenericTRecord.Test_Assign_method_for_generic_argument;
var
  F : IDynamicField;
begin
  FRecord.Assign(FObject);
  CheckEquals(FObject.TestInteger, FRecord.Data.TestInteger);
  FObject.TestInteger := 0;
  CheckNotEquals(FObject.TestInteger, FRecord.Data.TestInteger);
end;
{$ENDREGION}

end.

