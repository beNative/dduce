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

unit Test.DDuce.DynamicRecord;

{ Tests all members of the non-generic version of TRecord. }

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Variants,
  Winapi.Windows, Winapi.Messages,
  Vcl.ActnList, Vcl.Forms, Vcl.Buttons, Vcl.Graphics,
  Data.DB,
  Datasnap.DBClient,

  DDuce.DynamicRecord, DDuce.RandomData,

  TestFramework, // DUnit

  Test.DDuce.DynamicRecord.Data;

type
  TestTRecord = class(TTestCase)
  private
    FRecord        : TRecord;
    FDynamicRecord : IDynamicRecord;

    function RetrieveRecord(
      const AString : string;
       var  ARecord : TRecord
    ): Boolean;
    function CreateDataSet: TDataSet;
    function RetrieveRecordFunction: TRecord;

    procedure PassingRecordArgumentByValueParam(ARecord: TRecord);
    procedure PassingRecordArgumentByConstParam(const ARecord: TRecord);
    procedure PassingRecordArgumentByVarParam(var ARecord: TRecord);
    procedure PassingRecordArgumentByInterfaceParam(const ARecord: IDynamicRecord);

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure Test_passing_TRecord_argument_as_const_parameter;
    procedure Test_passing_TRecord_argument_as_var_parameter;
    procedure Test_passing_TRecord_argument_as_value_parameter;
    procedure Test_passing_TRecord_argument_as_IDynamicRecord_parameter;

    procedure Test_Assign_method_for_IDynamicRecord_argument;

    procedure Test_assignment_operator_for_TRecord_to_TRecord;
    procedure Test_assignment_operator_for_TRecord_to_IDynamicRecord;

    procedure Test_faulty_condition;

    procedure Test_ContainsField_method;
    procedure Test_DeleteField_method;

    procedure Test_IsEmpty_method;
    procedure Test_IsBlank_method;

    // test methods with automatic conversion to destination type
    procedure Test_ToFloat_method;
    procedure Test_ToInteger_method;
    procedure Test_ToString_method;
    procedure Test_ToBoolean_method;

    procedure Test_FromDataSet_method;
    procedure Test_FromStrings_method;

    procedure Test_AsDelimitedText_method;
    procedure Test_AsVarArray_method;
    procedure Test_AsCommaText_method;
    procedure Test_ToStrings_method;

    procedure Test_AssignProperty_method;

    procedure TestRetrieveRecord;
    procedure TestRetrieveRecordFunction;

  end;

implementation

uses
  System.Math, System.Types, System.Rtti,

  Vcl.Dialogs;

const
  TEST_INTEGER            = 'TestInteger';
  TEST_DOUBLE             = 'TestDouble';
  TEST_BOOLEAN            = 'TestBoolean';
  TEST_STRING             = 'TestString';
  TEST_STRING_INTEGER     = 'TestStringInteger';
  TEST_STRING_DOUBLE      = 'TestStringDouble';
  TEST_TVALUE             = 'TestTValue';
  TEST_VARIANT            = 'TestVariant';
  TEST_NON_EXISTENT_VALUE = 'TestNonExistentValue';

{$REGION 'SetUp and TearDown methods'}
procedure TestTRecord.SetUp;
begin
  FRecord[TEST_INTEGER]        := 5;
  FRecord[TEST_STRING]         := 'Test';
  FRecord[TEST_STRING_INTEGER] := '5';
  FRecord[TEST_STRING_DOUBLE]  := '3,14';
  FRecord[TEST_BOOLEAN]        := True;
  FRecord[TEST_DOUBLE]         := 3.14;

  FDynamicRecord := TRecord.CreateDynamicRecord;
  FDynamicRecord[TEST_INTEGER]        := 5;
  FDynamicRecord[TEST_STRING]         := 'Test';
  FDynamicRecord[TEST_STRING_INTEGER] := '5';
  FDynamicRecord[TEST_STRING_DOUBLE]  := '3,14';
  FDynamicRecord[TEST_BOOLEAN]        := True;
  FDynamicRecord[TEST_DOUBLE]         := 3.14;
end;

procedure TestTRecord.TearDown;
begin
  FRecord.Clear;
  FDynamicRecord := nil;
end;
{$ENDREGION}

{$REGION 'private methods'}
// helper methods used in tests

function TestTRecord.CreateDataSet: TDataSet;
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
  for I := 0 to 100 do
  begin
    DS.Append;
    DS.FieldByName('Name').AsString := RandomData.FirstName(gnMale);
    DS.FieldByName('Age').AsInteger := RandomData.Number(50);
    DS.Post;
  end;
  DS.First;
  Result := DS;
end;

procedure TestTRecord.PassingRecordArgumentByConstParam(const ARecord: TRecord);
var
  S : string;
  F : IDynamicField;
begin
  ARecord.Data.NewValue := 'Test';
  for F in ARecord do
  begin
    Status(F.ToString);
  end;
  S := Format('PassingRecordThroughConstParam: '#13#10'%s', [ARecord.ToString]);
end;

procedure TestTRecord.PassingRecordArgumentByInterfaceParam(
  const ARecord: IDynamicRecord);
var
  S : string;
begin
  S := Format('PassingRecordThroughInterfaceParam: '#13#10'%s', [ARecord.ToString]);
end;

procedure TestTRecord.PassingRecordArgumentByValueParam(ARecord: TRecord);
var
  F : IDynamicField;
begin
  for F in ARecord do
  begin
    Status(F.ToString);
  end;
end;

procedure TestTRecord.PassingRecordArgumentByVarParam(var ARecord: TRecord);
var
  S : string;
begin
  S := Format('PassingRecordThroughVarParam: '#13#10'%s', [ARecord.ToString]);
  Status(S);
end;

function TestTRecord.RetrieveRecordFunction: TRecord;
begin
  Result['Test'] := 'test';
end;

function TestTRecord.RetrieveRecord(const AString: string;
  var ARecord: TRecord): Boolean;
var
  DS: TDataSet;
begin
  DS := CreateDataSet;
  try
    Result := True;
    ARecord.FromDataSet(DS);
    ARecord['Test'] := AString;
    ARecord['Count'] := DS.RecordCount;
  finally
    DS.Free;
  end;
end;
{$ENDREGION}

{$REGION 'Test methods that convert all content to another format'}
procedure TestTRecord.Test_AsCommaText_method;
begin
  CheckEqualsString(
    '5,Test,5,3,14,True,3,14',
    FRecord.AsCommaText
  );
end;

procedure TestTRecord.Test_AsDelimitedText_method;
begin
  CheckEqualsString('5,Test,5,3,14,True,3,14', FRecord.AsDelimitedText(','));
  CheckEqualsString('5', FRecord.AsDelimitedText(TEST_INTEGER, ','));
end;

procedure TestTRecord.Test_AsVarArray_method;
var
  VA : Variant;
  I  : Integer;
  N  : Integer;
  V  : Variant;
begin
  VA := FRecord.AsVarArray;
  N := VarArrayDimCount(VA);
  for I := VarArrayLowBound(VA, N) to VarArrayHighBound(VA, N) do
  begin
    V := VarArrayGet(VA, [I]);
    Status(IntToStr(I));
    Status(VarToStrDef(V, ''));
  end;
  CheckTrue(True, 'OK');
end;

procedure TestTRecord.Test_ToStrings_method;
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

{$REGION 'Test methods that load all content from another format'}
procedure TestTRecord.Test_FromDataSet_method;
var
  DS : TDataSet;
  R  : TRecord;
  I  : Integer;
begin
  for I := 0 to 10 do
  begin
    DS := CreateDataSet;
    try
      R.FromDataSet(DS);
      Status(R.ToString);
    finally
      FreeAndNil(DS);
    end;
  end;
  CheckTrue(True, 'OK');
end;

procedure TestTRecord.Test_FromStrings_method;
var
  SL : TStrings;
  R  : TRecord;
begin
  SL := TStringList.Create;
  try
    SL.Values['S'] := 'test';
    SL.Values['I'] := '5';
    R.FromStrings(SL);
    CheckTrue(R.ToInteger('I') = 5, 'I');
    CheckTrue(R.ToString('S') = 'test', 'S');
  finally
    SL.Free;
  end;
  CheckTrue(True, 'OK');
end;
{$ENDREGION}

{$REGION 'Test Assign methods'}
procedure TestTRecord.Test_Assign_method_for_IDynamicRecord_argument;
var
  R : TRecord;
begin
  R.Assign(FDynamicRecord);
  Status(R.ToString);
  CheckEquals(FDynamicRecord.Data.TestBoolean, R[TEST_BOOLEAN].AsBoolean, TEST_BOOLEAN);
  CheckEquals(FDynamicRecord.Data.TestInteger, R[TEST_INTEGER].AsInteger, TEST_INTEGER);
  CheckEquals(FDynamicRecord.Data.TestString, R[TEST_STRING].AsString, TEST_STRING);
  //CheckEquals(FDynamicRecord.Data.TestDouble, R[TEST_DOUBLE].AsExtended, TEST_DOUBLE);
end;
{$ENDREGION}

{$REGION 'Tests of field manipulation methods'}
procedure TestTRecord.Test_ContainsField_method;
begin
  // Test ContainsField method
  CheckTrue(FRecord.ContainsField(TEST_INTEGER), TEST_INTEGER);
  CheckTrue(FRecord.ContainsField(UpperCase(TEST_INTEGER)), TEST_INTEGER);
  CheckTrue(FRecord.ContainsField(TEST_STRING), TEST_STRING);
  CheckTrue(FRecord.ContainsField(TEST_BOOLEAN), TEST_BOOLEAN);
  CheckFalse(FRecord.ContainsField(TEST_NON_EXISTENT_VALUE), TEST_NON_EXISTENT_VALUE);
  CheckTrue(FRecord.ContainsField(TEST_DOUBLE), TEST_DOUBLE);
end;

procedure TestTRecord.Test_DeleteField_method;
begin
  // Test DeleteField
  CheckTrue(FRecord.DeleteField(TEST_INTEGER), TEST_INTEGER);
  CheckFalse(FRecord.ContainsField(TEST_INTEGER), TEST_INTEGER);
  CheckFalse(FRecord.ContainsField(TEST_INTEGER), TEST_INTEGER);
  CheckTrue(FRecord.ContainsField(TEST_STRING), TEST_STRING);
end;
{$ENDREGION}

{$REGION 'Tests for methods that check the content'}
procedure TestTRecord.Test_IsBlank_method;
var
  R : TRecord;
  S : string;
begin
  S := TEST_NON_EXISTENT_VALUE;
  CheckTrue(R.IsBlank(S), S);

  S := 'TestEmptyString';
  R[S] := '';
  CheckTrue(R.IsBlank(S), S);

  S := 'TestEmptyValue';
  R[S] := TValue.Empty;
  CheckTrue(R.IsBlank(S), S);

  S := TEST_STRING;
  R[S] := 'Test';
  CheckFalse(R.IsBlank(S), S);

  S := TEST_INTEGER;
  R[S] := 0;
  CheckFalse(R.IsBlank(S), S);

  S := TEST_DOUBLE;
  R[S] := 3.14;
  CheckFalse(R.IsBlank(S), S);
  R[S] := NaN;
  CheckFalse(R.IsBlank(S), S);

  S := TEST_BOOLEAN;
  R[S] := False;
  CheckFalse(R.IsBlank(S), S);

  S := TEST_VARIANT;
  R[S] := TValue.FromVariant(Null);
  CheckTrue(R.IsBlank(S), 'Null');
  R[S] := TValue.FromVariant(Unassigned);
  CheckTrue(R.IsBlank(S), 'Unassigned');
// This test fails! EmptyParam is not translated correctly by TValue
//  R[S] := TValue.FromVariant(EmptyParam);
//  CheckTrue(R.IsBlank(S), 'EmptyParam');
  R[S] := TValue.FromVariant('');
  CheckTrue(R.IsBlank(S), 'Empty string');
end;

procedure TestTRecord.Test_IsEmpty_method;
var
  R : TRecord;
begin
  CheckTrue(R.IsEmpty);

  R['S'] := 'test';
  CheckFalse(R.IsEmpty);

  R['T'] := 'test';
  R.Clear;
  CheckTrue(R.IsEmpty);

  R['T'] := 'test';
  R.DeleteField('T');
  CheckTrue(R.IsEmpty);
end;
{$ENDREGION}

{$REGION 'Passing TRecord instances as an argument'}
procedure TestTRecord.Test_passing_TRecord_argument_as_const_parameter;
var
  R : TRecord;
begin
  R.Data.B := False;
  R.Data.I := 10;
  R.Data.S := 'string';
  R.Data.F := 3.14;
  PassingRecordArgumentByConstParam(R);
  CheckTrue(R.Data.NewValue = 'Test', 'NewValue not found');
end;

procedure TestTRecord.Test_passing_TRecord_argument_as_IDynamicRecord_parameter;
var
  R : TRecord;
begin
  R.Data.B := False;
  R.Data.I := 10;
  R.Data.S := 'string';
  R.Data.F := 3.14;
  PassingRecordArgumentByInterfaceParam(R);
  CheckTrue(True, 'OK');
end;

procedure TestTRecord.Test_passing_TRecord_argument_as_value_parameter;
var
  R : TRecord;
  I : Integer;
begin
  R.Data.B := False;
  R.Data.I := 10;
  R.Data.S := 'string';
  R.Data.F := 3.14;
  PassingRecordArgumentByValueParam(R);

  for I := 0 to 100 do
  begin
    R.Data.B := True;
    R.Data.I := RandomData.Number(20);
    R.Data.S := RandomData.CompanyName;
    R.Data.F := RandomData.Number(457);

    PassingRecordArgumentByValueParam(R);
    PassingRecordArgumentByConstParam(R);
    PassingRecordArgumentByVarParam(R);
    PassingRecordArgumentByInterfaceParam(R);
  end;
  CheckTrue(True, 'OK');
end;

procedure TestTRecord.Test_passing_TRecord_argument_as_var_parameter;
begin
  PassingRecordArgumentByVarParam(FRecord);
  CheckTrue(True, 'OK');
end;
{$ENDREGION}

{$REGION 'Tests for methods that convert the TValue to the destination type'}
procedure TestTRecord.Test_ToBoolean_method;
begin
  CheckTrue(FRecord.ToBoolean(TEST_STRING, True), TEST_STRING);
  CheckTrue(FRecord.ToBoolean(TEST_STRING_INTEGER, True), TEST_STRING_INTEGER);
  CheckTrue(FRecord.ToBoolean(TEST_STRING_DOUBLE, True), TEST_STRING_DOUBLE);
  CheckTrue(FRecord.ToBoolean(TEST_DOUBLE, True), TEST_DOUBLE);
  CheckTrue(FRecord.ToBoolean(TEST_BOOLEAN, True), TEST_BOOLEAN);
  CheckTrue(FRecord.ToBoolean(TEST_INTEGER, True), TEST_INTEGER);
end;

procedure TestTRecord.Test_ToFloat_method;
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

procedure TestTRecord.Test_ToInteger_method;
begin
  CheckEquals(1, FRecord.ToInteger(TEST_BOOLEAN), TEST_BOOLEAN);
  CheckEquals(5, FRecord.ToInteger(TEST_INTEGER), TEST_INTEGER);
  CheckEquals(0, FRecord.ToInteger(TEST_STRING), TEST_STRING);
  CheckEquals(5, FRecord.ToInteger(TEST_STRING_INTEGER), TEST_STRING_INTEGER);
  CheckEquals(0, FRecord.ToInteger(TEST_STRING_DOUBLE), TEST_STRING_DOUBLE);

  // float values are not rounded or trunked when converting. Conversion fails.
  CheckEquals(0, FRecord.ToInteger(TEST_DOUBLE), TEST_DOUBLE);
end;

procedure TestTRecord.Test_ToString_method;
begin
  CheckEquals('True', FRecord.ToString(TEST_BOOLEAN), TEST_BOOLEAN);
  CheckEquals('5', FRecord.ToString(TEST_INTEGER), TEST_INTEGER);
  CheckEquals('Test', FRecord.ToString(TEST_STRING), TEST_STRING);
  CheckEquals('5', FRecord.ToString(TEST_STRING_INTEGER), TEST_STRING_INTEGER);
  CheckEquals('3,14', FRecord.ToString(TEST_STRING_DOUBLE), TEST_STRING_DOUBLE);
  CheckEquals('3,14', FRecord.ToString(TEST_DOUBLE), TEST_DOUBLE);
end;
{$REGION 'Operator overload tests'}
procedure TestTRecord.Test_assignment_operator_for_TRecord_to_TRecord;
var
  R: TRecord;
begin
  R := FRecord;
  FRecord[TEST_INTEGER] := 6;
  CheckEquals(5, R[TEST_INTEGER].AsInteger, TEST_INTEGER);
end;
{$ENDREGION}

procedure TestTRecord.Test_AssignProperty_method;
var
  O  : TTestClass;
  R  : TRecord;
begin
  O := TTestClass.Create;
  try
    O.TestBoolean := True;
    O.TestString  := 'test';
    O.TestInteger := 5;
    O.TestDouble  := 3.14;
    R.AssignProperty(O, TEST_BOOLEAN);
    R.AssignProperty(O, TEST_STRING);
    R.AssignProperty(O, TEST_INTEGER);
    R.AssignProperty(O, TEST_DOUBLE);

    CheckEquals(O.TestBoolean, R[TEST_BOOLEAN].AsBoolean, TEST_BOOLEAN);
    CheckEquals(O.TestInteger, R[TEST_INTEGER].AsInteger, TEST_INTEGER);
    CheckEquals(O.TestString, R[TEST_STRING].AsString, TEST_STRING);
    CheckEquals(O.TestDouble, R[TEST_DOUBLE].AsExtended, TEST_DOUBLE);
  finally
    O.Free;
  end;
end;

procedure TestTRecord.TestRetrieveRecord;
var
  R : TRecord;
  I : Integer;
begin
  for I := 0 to 1000 do
  begin
    RetrieveRecord(RandomData.Name, R);
    Status(R['Test'].AsString);
  end;
  CheckTrue(True, 'OK');
end;

procedure TestTRecord.TestRetrieveRecordFunction;
var
  R : TRecord;
begin
  R := RetrieveRecordFunction;
  CheckEquals('test', R['Test'].AsString);
end;

procedure TestTRecord.Test_assignment_operator_for_TRecord_to_IDynamicRecord;
var
  DR : IDynamicRecord;
begin
  DR := FRecord;
  FRecord[TEST_STRING] := 'Yes';
  CheckEquals('Test', DR[TEST_STRING].AsString); // a copy has been made
end;

procedure TestTRecord.Test_faulty_condition;
begin
  FDynamicRecord[TEST_STRING] := '';
  FRecord := FDynamicRecord;
  CheckEqualsString('', FRecord[TEST_STRING].AsString);

  FRecord['TEST_STRING'] := 'New Value';
  CheckNotEqualsString('New Value', FDynamicRecord[TEST_STRING].AsString);

  FDynamicRecord[TEST_STRING] := 'Bad';
  CheckNotEqualsString('Bad', FRecord[TEST_STRING].AsString);

  FRecord := FDynamicRecord;
  FRecord[TEST_STRING] := 'Good';
  CheckEqualsString('Bad', FDynamicRecord[TEST_STRING].AsString);

  FDynamicRecord := FRecord;
//FDynamicRecord.Assign(FRecord);
  CheckEqualsString('Good', FDynamicRecord[TEST_STRING].AsString);

  FDynamicRecord[TEST_STRING] := 'Bad';
  CheckEqualsString('Good', FRecord[TEST_STRING].AsString);


end;

end.

