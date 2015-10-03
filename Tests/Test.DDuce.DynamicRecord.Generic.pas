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

uses
  System.Variants, System.SysUtils, System.Contnrs, System.Classes,
  Data.DB,
  Datasnap.DBClient,
  Winapi.Windows, Winapi.Messages,
  Vcl.Controls, Vcl.Dialogs, Vcl.Forms, Vcl.Buttons, Vcl.ActnList, Vcl.StdCtrls,
  Vcl.Graphics,

  DDuce.DynamicRecord,

  TestFramework,

  Test.DDuce.DynamicRecord.Data;

type
  TestGenericTRecord = class(TTestCase)
  private
    FObject : TTestClass;
    FRecord : TRecord<TTestClass>;

    function RetrieveRecord(
      const AString : string;
       var  ARecord : TRecord
    ): Boolean;

    function RetrieveRecordFunction: TRecord;
    procedure ProcessRecord(ARecord: IDynamicRecord);

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestTRecordAssignments;
    procedure Test_ContainsField_method;
    procedure Test_DeleteField_method;
    procedure Test_IsEmpty_method;

    procedure TestConversions;

    // test methods with automatic conversion to destination type
    procedure Test_ToFloat_Method;
    procedure Test_ToInteger_method;
    procedure Test_ToString_method;
    procedure Test_ToBoolean_method;
    procedure Test_IsBlank_method;

    procedure Test_FromDataSet_method;

    procedure Test_FromStrings_method;
    procedure Test_ToStrings_method;

    procedure Test_AsDelimitedText_method;
    procedure Test_AsVarArray_method;
    procedure Test_AsCommaText_method;

    procedure Test_Count_Property;

    procedure TestAssignProperty;

    procedure TestRetrieveRecord;

    procedure TestRetrieveRecordFunction;

  end;

implementation

uses
  System.Math, System.Types, System.Rtti;

const
  TEST_INTEGER            = 'TestInteger';
  TEST_DOUBLE             = 'TestDouble';
  TEST_BOOLEAN            = 'TestBoolean';
  TEST_STRING             = 'TestString';
  TEST_TVALUE             = 'TestTValue';
  TEST_VARIANT            = 'TestVariant';
  TEST_NON_EXISTENT_VALUE = 'TestNonExistentValue';

{$REGION 'private methods'}
procedure TestGenericTRecord.ProcessRecord(ARecord: IDynamicRecord);
begin
  Status('ProcessRecord:');
  Status(ARecord.ToString);
end;

function TestGenericTRecord.RetrieveRecord(const AString: string;
  var ARecord: TRecord): Boolean;
begin
  Result := True;
end;

function TestGenericTRecord.RetrieveRecordFunction: TRecord;
begin
  Result := TRecord.Create;
end;
{$ENDREGION}

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

  // FRecord: TRecord<TTestClass>;
  FRecord.Data.TestBoolean := True;
  FRecord.Data.TestString  := 'Test';
  FRecord.Data.TestInteger := 5;
  FRecord.Data.TestDouble  := 3.14;
end;

procedure TestGenericTRecord.TearDown;
begin
  FreeAndNil(FObject);
  inherited TearDown;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'Test methods'}
procedure TestGenericTRecord.Test_AsCommaText_method;
var
  R : TRecord<TTestClass>;
  T : TRecord;
begin
//  R.Create(FObject);
//  Status(R.ToString);
//  T := R;
  ProcessRecord(FRecord);
end;

procedure TestGenericTRecord.Test_AsDelimitedText_method;
begin

end;

procedure TestGenericTRecord.TestAssignProperty;
begin

end;

procedure TestGenericTRecord.Test_AsVarArray_method;
begin
  //
end;

procedure TestGenericTRecord.Test_ContainsField_method;
begin
  CheckTrue(FRecord.ContainsField('TestChar'));
end;

procedure TestGenericTRecord.Test_Count_Property;
begin
  CheckEquals(12, FRecord.Count);
end;

procedure TestGenericTRecord.TestConversions;
var
  O  : TTestClass;
  //TR : TTestRecord;
  R  : TRecord<TTestClass>;
begin
  FObject.TestBoolean := True;
  FObject.TestString  := 'test';
  FObject.TestInteger := 5;
  FObject.TestDouble  := 3.14;
    //R.From<TTestClass>(O);
    //R.Create(O);
//    R.AssignProperty(O, TEST_BOOLEAN);
//    R.AssignProperty(O, TEST_STRING);
//    R.AssignProperty(O, TEST_INTEGER);
//    R.AssignProperty(O, TEST_DOUBLE);

    CheckEquals(True, R[TEST_BOOLEAN].AsBoolean, TEST_BOOLEAN);
    CheckEquals(5, R[TEST_INTEGER].AsInteger, TEST_INTEGER);
    CheckEquals('test', R[TEST_STRING].AsString, TEST_STRING);

//    TR.TestBoolean := True;
//    TR.TestString  := 'test';
//    TR.TestInteger := 5;
//    TR.TestDouble  := 3.14;
//    R.AssignProperty(TValue.From(TR), TEST_BOOLEAN);
//    R.AssignProperty(TValue.From(TR), TEST_STRING);
//    R.AssignProperty(TValue.From(TR), TEST_INTEGER);
//    R.AssignProperty(TValue.From(TR), TEST_DOUBLE);

//finally
    O.Free;
  //end;
end;

procedure TestGenericTRecord.Test_DeleteField_method;
begin
        // should not be possible!
end;

procedure TestGenericTRecord.Test_FromDataSet_method;
begin

end;

procedure TestGenericTRecord.Test_FromStrings_method;
begin

end;

procedure TestGenericTRecord.Test_IsBlank_method;
var
  R: TRecord<TTestClass>;
begin
  R[TEST_STRING] := '';
  CheckTrue(R.IsBlank(TEST_STRING), TEST_STRING);
end;

{ TODO: IsEmpty has little meaning in the generic version. }

procedure TestGenericTRecord.Test_IsEmpty_method;
var
  R: TRecord<TTestClass>;
begin
  CheckFalse(R.IsEmpty);  //
end;

procedure TestGenericTRecord.TestRetrieveRecord;
begin
//
end;

procedure TestGenericTRecord.TestRetrieveRecordFunction;
begin
//
end;

procedure TestGenericTRecord.Test_ToBoolean_method;
begin
  CheckTrue(FRecord.ToBoolean(TEST_STRING, True), TEST_STRING);
  //CheckTrue(FRecord.ToBoolean(TEST_STRING_INTEGER, True), TEST_STRING_INTEGER);
  //CheckTrue(FRecord.ToBoolean(TEST_STRING_DOUBLE, True), TEST_STRING_DOUBLE);
  CheckTrue(FRecord.ToBoolean(TEST_DOUBLE, True), TEST_DOUBLE);
  CheckTrue(FRecord.ToBoolean(TEST_BOOLEAN, True), TEST_BOOLEAN);
  CheckTrue(FRecord.ToBoolean(TEST_INTEGER, True), TEST_INTEGER);
end;

procedure TestGenericTRecord.Test_ToFloat_Method;
begin
  CheckEquals(5, FRecord.ToFloat(TEST_INTEGER), TEST_INTEGER);
  try
    CheckTrue(IsZero(FRecord.ToFloat(TEST_STRING)), TEST_STRING);
  except
  end;
  try
    CheckTrue(IsZero(FRecord.ToFloat(TEST_BOOLEAN)), TEST_BOOLEAN);
  except
  end;
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

procedure TestGenericTRecord.TestTRecordAssignments;
var
  DR : IDynamicRecord;
  DCR : IDynamicRecord;
  DCGR : IDynamicRecord<TTestClass>;
begin
  DR := TRecord.CreateDynamicRecord;
  DR[TEST_INTEGER] := 5;
  Status(DR.ToString);

//  //DCR := TRecord<TTestClass>.Create(FObject);
//  Status(DCR.ToString);
//
//  // test TRecord<T> => IDynamicRecord<T>
//  DCGR := TRecord<TTestClass>.Create(FObject);
//  DCGR.Data.TestInteger := 50;
//  Status(DCGR.ToString);
//
//  DCR := DCGR;
//  DCR.Data.TestString := 'String';
//  Status(DCR.ToString);
end;
{$ENDREGION}

end.

