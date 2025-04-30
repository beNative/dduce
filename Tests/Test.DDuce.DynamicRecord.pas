{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Test.DDuce.DynamicRecord;

{ Tests all members of the non-generic version of DynamicRecord. }

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Variants,
  Winapi.Windows, Winapi.Messages,
  Vcl.ActnList, Vcl.Forms, Vcl.Buttons, Vcl.Graphics,
  Data.DB,
  Datasnap.DBClient,

  DDuce.DynamicRecord, DDuce.RandomData,

  Spring,

  DUnitX.TestFramework,

  Test.Data;

type
  {$M+}
  [TestFixture][IgnoreMemoryLeaks]
  TestDynamicRecord = class
  private
    FRecord        : DynamicRecord;
    FDynamicRecord : IDynamicRecord;
    FTestObject    : TTestClass;
    FTestRecord    : TTestRecord;

    function RetrieveRecord(
      const AString : string;
      var  ARecord  : DynamicRecord
    ): Boolean;
    function RetrieveRecordFunction: DynamicRecord;

  protected
    procedure Test_From_method_for_record;

    procedure PassingArgumentByValueParam(ARecord: DynamicRecord);

    procedure PassingArgumentByVarParam(var ARecord: DynamicRecord);
    procedure PassingArgumentByInterfaceParam(ARecord: IDynamicRecord);

    procedure PassingArgumentByConstParam(const ARecord: DynamicRecord);

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_passing_DynamicRecord_argument_as_const_parameter;
    [Test]
    procedure Test_passing_DynamicRecord_argument_as_var_parameter;
    [Test]
    procedure Test_passing_DynamicRecord_argument_as_value_parameter;
    [Test]
    procedure Test_passing_DynamicRecord_argument_as_IDynamicRecord_parameter;
    [Test]
    procedure Test_passing_IDynamicRecord_argument_as_value_parameter;
    [Test]
    procedure Test_passing_IDynamicRecord_argument_as_const_parameter;
    [Test]
    procedure Test_passing_IDynamicRecord_argument_as_IDynamicRecord_parameter;

    [Test]
    procedure Test_Assign_method_for_IDynamicRecord_argument;
    [Test]
    procedure Test_Assign_method_for_DynamicRecord_argument;
    [Test]
    procedure Test_Assign_method_for_generic_IDynamicRecord_argument;
    [Test]
    procedure Test_Assign_method_for_generic_DynamicRecord_argument;

    [Test]
    procedure Test_assignment_operator_for_DynamicRecord_to_DynamicRecord;
    [Test]
    procedure Test_assignment_operator_for_DynamicRecord_to_IDynamicRecord;
    [Test]
    procedure Test_assignment_operator_for_IDynamicRecord_to_DynamicRecord;

    [Test]
    procedure Test_faulty_condition;

    [Test]
    procedure Test_ContainsField_method;
    [Test]
    procedure Test_DeleteField_method;

    [Test]
    procedure Test_IsEmpty_method;
    [Test]
    procedure Test_IsBlank_method;

    // test methods with automatic conversion to destination type
    [Test]
    procedure Test_ToFloat_method;
    [Test]
    procedure Test_ToInteger_method;
    [Test]
    procedure Test_ToString_method;
    [Test]
    procedure Test_ToBoolean_method;

    [Test]
    procedure Test_FromDataSet_method;
    [Test]
    procedure Test_FromStrings_method;
    [Test]
    procedure Test_FromStrings_method_with_spaces;
    [Test]
    procedure Test_FromString_method;
    [Test]
    procedure Test_FromArray_method;
    [Test]
    procedure Test_From_method_for_object;

    [Test]
    procedure Test_ToDelimitedText_method;
    [Test]
    procedure Test_ToVarArray_method;
    [Test]
    procedure Test_ToCommaText_method;
    [Test]
    procedure Test_ToStrings_method;

    [Test]
    procedure Test_AssignProperty_method;

    [Test]
    procedure Test_AssignTo_method_for_Object;

    [Test]
    procedure Test_RetrieveRecord;
    [Test]
    procedure Test_RetrieveRecordFunction;

    [Test] procedure Test_From_Options_AssignNulls;
    [Test] procedure Test_From_Options_AssignPropsFields;
    [Test] procedure Test_From_Options_SpecificNames;
    [Test] procedure Test_AssignTo_Options_AssignNulls;
    [Test] procedure Test_AssignTo_Options_SpecificNames;
    //[Test] procedure Test_From_With_Nullable_Properties;
    //[Test] procedure Test_AssignTo_With_Nullable_Properties;
    [Test] procedure Test_CopyOnWrite_Behavior;
    [Test] procedure Test_Data_Variant_Write;
    //[Test] procedure Test_FromPersistent;
    [Test] procedure Test_ToDelimitedText_Quoting;
    [Test] procedure Test_AssignProperty_Error;
    [Test] procedure Test_FromArray_Bracketed;
    [Test] procedure Test_Clear_Method;
    [Test] procedure Test_Access_NonExistent_Field_Via_Variant_Data;
    [Test] procedure Test_Conversion_Failure_Default_Value;

  end;

implementation

uses
  System.Math, System.Types, System.Rtti,
  Vcl.Dialogs,

  Test.Utils;

const
  TEST_INTEGER            = 'TestInteger';
  TEST_DOUBLE             = 'TestDouble';
  TEST_BOOLEAN            = 'TestBoolean';
  TEST_STRING             = 'TestString';
  TEST_STRING_INTEGER     = 'TestStringInteger';
  TEST_STRING_DOUBLE      = 'TestStringDouble';
  TEST_TVALUE             = 'TestTValue';
  TEST_CHAR               = 'TestChar';
  TEST_VARIANT            = 'TestVariant';
  TEST_NON_EXISTENT_VALUE = 'TestNonExistentValue';

{$REGION 'SetUp and TearDown methods'}
procedure TestDynamicRecord.Setup;
begin
  FRecord[TEST_INTEGER]        := 5;
  FRecord[TEST_STRING]         := 'Test';
  FRecord[TEST_STRING_INTEGER] := '5';
  FRecord[TEST_STRING_DOUBLE]  := '3,14';
  FRecord[TEST_BOOLEAN]        := True;
  FRecord[TEST_DOUBLE]         := 3.14;
  FRecord[TEST_CHAR]           := 'C';
  FDynamicRecord := DynamicRecord.CreateDynamicRecord;
  FDynamicRecord[TEST_INTEGER]        := 5;
  FDynamicRecord[TEST_STRING]         := 'Test';
  FDynamicRecord[TEST_STRING_INTEGER] := '5';
  FDynamicRecord[TEST_STRING_DOUBLE]  := '3,14';
  FDynamicRecord[TEST_BOOLEAN]        := True;
  FDynamicRecord[TEST_DOUBLE]         := 3.14;
  FDynamicRecord[TEST_CHAR]           := 'C';

  // Initialize helper objects/records
  FTestObject := TTestClass.Create;
  FTestObject.TestInteger := 101;
  FTestObject.TestString  := 'Object String';
  FTestObject.TestDouble  := 1.23;
  FTestObject.TestBoolean := False; // Different from FRecord default
  FTestObject.TestChar    := 'O';
  FTestObject.TestDateTime := EncodeDate(2024, 1, 1);
  // FTestObject has a nil field which won't be assigned by default


  FTestRecord.TestInteger := 202;
  FTestRecord.TestString  := 'Record String';
  FTestRecord.TestDouble  := 4.56;
  FTestRecord.TestBoolean := True; // Same as FRecord default

end;

procedure TestDynamicRecord.TearDown;
begin
  FRecord.Clear;
  FDynamicRecord := nil;
  FTestObject.Free;
end;
{$ENDREGION}

{$REGION 'private methods'}
// helper methods used in tests
procedure TestDynamicRecord.PassingArgumentByConstParam(const ARecord: DynamicRecord);
var
  F : IDynamicField;
begin
  ARecord['NewValue']  := 'Test';
  ARecord['NewValue2'] := 'Test';
  for F in ARecord do
  begin
    Status(F.ToString);
  end;
end;

procedure TestDynamicRecord.PassingArgumentByInterfaceParam(
  ARecord: IDynamicRecord);
var
  F : IDynamicField;
begin
  ARecord.Data.NewValue := 'Test';
  ARecord['NewValue2'] := 'Test';
  for F in ARecord do
  begin
    Status(F.ToString);
  end;
end;

procedure TestDynamicRecord.PassingArgumentByValueParam(ARecord: DynamicRecord);
var
  F : IDynamicField;
begin
  ARecord.Data.NewValue := 'Test';
  ARecord['NewValue2'] := 'Test';
  for F in ARecord do
  begin
    Status(F.ToString);
  end;
end;

{ For var parameters the type of the passed value does have to match with the
  type of the parameter. So
              PassingArgumentByVarParam(FDynamicRecord);
  will not compile.
}
procedure TestDynamicRecord.PassingArgumentByVarParam(var ARecord: DynamicRecord);
var
  F : IDynamicField;
begin
  ARecord.Data.NewValue := 'Test';
  ARecord['NewValue2'] := 'Test';
  for F in ARecord do
  begin
    Status(F.ToString);
  end;
end;

function TestDynamicRecord.RetrieveRecordFunction: DynamicRecord;
begin
  Result['Test'] := 'test';
end;

function TestDynamicRecord.RetrieveRecord(const AString: string;
  var ARecord: DynamicRecord): Boolean;
var
  DS: TDataSet;
begin
  DS := TTestUtils.CreateDataSet(10);
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
procedure TestDynamicRecord.Test_ToCommaText_method;
begin
  Assert.AreEqual(
    '5,Test,5,3,14,True,3,14,C',
    FRecord.ToCommaText
  );
end;

procedure TestDynamicRecord.Test_ToDelimitedText_method;
begin
  Assert.AreEqual('5,Test,5,3,14,True,3,14,C', FRecord.ToDelimitedText(','));
  Assert.AreEqual('5', FRecord.ToDelimitedText(TEST_INTEGER, ','));
end;

procedure TestDynamicRecord.Test_ToVarArray_method;
var
  VA : Variant;
  I  : Integer;
  N  : Integer;
  V  : Variant;
  S  : string;
begin
  VA := FRecord.ToVarArray;
  N := VarArrayDimCount(VA);
  for I := VarArrayLowBound(VA, N) to VarArrayHighBound(VA, N) do
  begin
    V := VarArrayGet(VA, [I]);
    S := Format('[%d] = %s', [I, VarToStrDef(V, '')]);
    Status(S);
    Assert.AreEqual(VarToStrDef(V, ''), FRecord.Items[I].Value.ToString);
  end;
end;

procedure TestDynamicRecord.Test_ToStrings_method;
var
  SL : TStrings;
  F  : IDynamicField;
begin
  SL := TStringList.Create;
  try
    FRecord.ToStrings(SL);
    for F in FRecord do
    begin
      Assert.AreEqual(F.Value.ToString, SL.Values[F.Name]);
    end;
  finally
    SL.Free;
  end;
end;
{$ENDREGION}

{$REGION 'Test methods that load all content from another format'}
procedure TestDynamicRecord.Test_FromArray_method;
var
  R  : DynamicRecord;
  SA : TArray<string>;
  IA : TArray<Integer>;
  DA : TArray<Double>;
  S  : string;
begin
  SA := ['zero', 'one', 'two', 'three'];
  S := '0 = zero'#13#10'1 = one'#13#10'2 = two'#13#10'3 = three';
  R.FromArray<string>(SA);
  Assert.AreEqual(S, R.ToString(False));

  IA := [0, 1, 2, 3];
  S := '0 = 0'#13#10'1 = 1'#13#10'2 = 2'#13#10'3 = 3';
  R.FromArray<Integer>(IA);
  Assert.AreEqual(S, R.ToString(False));

  DA := [0, 1, 2, 3];
  S := '0 = 0'#13#10'1 = 1'#13#10'2 = 2'#13#10'3 = 3';
  R.FromArray<Double>(DA);
  Assert.AreEqual(S, R.ToString(False));
end;

procedure TestDynamicRecord.Test_FromDataSet_method;
var
  DS : TDataSet;
  R  : DynamicRecord;
  F  : IDynamicField;
begin
  DS := TTestUtils.CreateDataSet(100);
  try
    DS.First;
    while not DS.Eof do
    begin
      R.FromDataSet(DS, True);
      for F in R do
      begin
        Assert.IsTrue(F.Value.Equals(TValue.FromVariant(DS[F.Name])));
      end;
      DS.Next;
    end;
  finally
    FreeAndNil(DS);
  end;
end;

procedure TestDynamicRecord.Test_FromStrings_method;
var
  SL : TStrings;
  R  : DynamicRecord;
begin
  SL := TStringList.Create;
  try
    SL.Values['S'] := 'test';
    SL.Values['I'] := '5';
    R.FromStrings(SL);
    Assert.IsTrue(R.ToInteger('I') = 5, 'I');
    Assert.IsTrue(R.ToString('S') = 'test', 'S');
  finally
    SL.Free;
  end;
end;

procedure TestDynamicRecord.Test_FromStrings_method_with_spaces;
var
  SL : TStrings;
  R  : DynamicRecord;
begin
  SL := TStringList.Create;
  try
    SL.Add('S               = test');
    SL.Add('I               = 5');
    R.FromStrings(SL, True);
    Assert.IsTrue(R.ToInteger('I') = 5, 'I');
    Assert.IsTrue(R.ToString('S') = 'test', 'S');
  finally
    SL.Free;
  end;
end;

procedure TestDynamicRecord.Test_FromString_method;
const
  MY_STRING = 'One = 1' + #13#10 +
              'Two = 2' + #13#10 +
              '3 = Three';
var
  R  : DynamicRecord;
begin
  R.FromString(MY_STRING, True);
  Assert.IsTrue(R.ToInteger('One') = 1, 'One');
  Assert.IsTrue(R.ToInteger('Two') = 2, 'Two');
  Assert.IsTrue(R.ToString('3') = 'Three', '3');
end;
{$ENDREGION}

{$REGION 'Test Assign methods'}
procedure TestDynamicRecord.Test_Assign_method_for_IDynamicRecord_argument;
var
  R : DynamicRecord;
  F : IDynamicField;
begin
  R.Assign(FDynamicRecord);
  for F in R do
  begin
    Assert.IsTrue(FDynamicRecord[F.Name].Equals(F.Value));
  end;
  Assert.AreEqual(Boolean(FDynamicRecord.Data.TestBoolean), R[TEST_BOOLEAN].AsBoolean, TEST_BOOLEAN);
  Assert.AreEqual(Integer(FDynamicRecord.Data.TestInteger), R[TEST_INTEGER].AsInteger, TEST_INTEGER);
  Assert.AreEqual(string(FDynamicRecord.Data.TestString), R[TEST_STRING].AsString, TEST_STRING);
end;

procedure TestDynamicRecord.Test_Assign_method_for_DynamicRecord_argument;
var
  R : DynamicRecord;
  F : IDynamicField;
begin
  R.Assign(FRecord);
  for F in R do
  begin
    Assert.IsTrue(FRecord[F.Name].Equals(F.Value));
  end;
  Assert.AreEqual(Boolean(FRecord.Data.TestBoolean), R[TEST_BOOLEAN].AsBoolean, TEST_BOOLEAN);
  Assert.AreEqual(Integer(FRecord.Data.TestInteger), R[TEST_INTEGER].AsInteger, TEST_INTEGER);
  Assert.AreEqual(string(FRecord.Data.TestString), R[TEST_STRING].AsString, TEST_STRING);
end;
{$ENDREGION}

{$REGION 'Tests of field manipulation methods'}
procedure TestDynamicRecord.Test_ContainsField_method;
begin
  // Test ContainsField method
  Assert.IsTrue(FRecord.ContainsField(TEST_INTEGER), TEST_INTEGER);
  Assert.IsTrue(FRecord.ContainsField(UpperCase(TEST_INTEGER)), TEST_INTEGER);
  Assert.IsTrue(FRecord.ContainsField(TEST_STRING), TEST_STRING);
  Assert.IsTrue(FRecord.ContainsField(TEST_BOOLEAN), TEST_BOOLEAN);
  Assert.IsFalse(FRecord.ContainsField(TEST_NON_EXISTENT_VALUE), TEST_NON_EXISTENT_VALUE);
  Assert.IsTrue(FRecord.ContainsField(TEST_DOUBLE), TEST_DOUBLE);
end;

procedure TestDynamicRecord.Test_DeleteField_method;
begin
  // Test DeleteField
  Assert.IsTrue(FRecord.DeleteField(TEST_INTEGER), TEST_INTEGER);
  // Check if it is really gone
  Assert.IsFalse(FRecord.ContainsField(TEST_INTEGER), TEST_INTEGER);
end;
{$ENDREGION}

{$REGION 'Tests for methods that check the content'}
procedure TestDynamicRecord.Test_IsBlank_method;
var
  R : DynamicRecord;
  S : string;
begin
  S := TEST_NON_EXISTENT_VALUE;
  Assert.IsTrue(R.IsBlank(S), S);

  S := 'TestEmptyString';
  R[S] := '';
  Assert.IsTrue(R.IsBlank(S), S);

  S := 'TestEmptyValue';
  R[S] := TValue.Empty;
  Assert.IsTrue(R.IsBlank(S), S);

  S := TEST_STRING;
  R[S] := 'Test';
  Assert.IsFalse(R.IsBlank(S), S);

  S := TEST_INTEGER;
  R[S] := 0;
  Assert.IsFalse(R.IsBlank(S), S);

  S := TEST_DOUBLE;
  R[S] := 3.14;
  Assert.IsFalse(R.IsBlank(S), S);
  R[S] := NaN;
  Assert.IsFalse(R.IsBlank(S), S);
  R[S] := Infinity;
  Assert.IsFalse(R.IsBlank(S), S);
  R[S] := NegInfinity;
  Assert.IsFalse(R.IsBlank(S), S);
  R[S] := 0.0;
  Assert.IsFalse(R.IsBlank(S), S);

  S := TEST_BOOLEAN;
  R[S] := False;
  Assert.IsFalse(R.IsBlank(S), S);

  S := TEST_VARIANT;
  R[S] := TValue.FromVariant(Null);
  Assert.IsTrue(R.IsBlank(S), 'Null');
  R[S] := TValue.FromVariant(Unassigned);
  Assert.IsTrue(R.IsBlank(S), 'Unassigned');
  // EmptyParam does not translate to TValue.Empty.
  R[S] := TValue.FromVariant(EmptyParam);
  Assert.IsFalse(R.IsBlank(S), 'EmptyParam');

  R[S] := TValue.FromVariant('');
  Assert.IsTrue(R.IsBlank(S), 'Empty string');
end;

procedure TestDynamicRecord.Test_IsEmpty_method;
var
  R : DynamicRecord;
begin
  Assert.IsTrue(R.IsEmpty);

  R['S'] := 'test';
  Assert.IsFalse(R.IsEmpty);

  R['T'] := 'test';
  R.Clear;
  Assert.IsTrue(R.IsEmpty);

  R['T'] := 'test';
  R.DeleteField('T');
  Assert.IsTrue(R.IsEmpty);
end;
{$ENDREGION}

{$REGION 'Passing DynamicRecord instances as an argument'}
{ When passing as a const parameter no copy is made of the argument. However,
  inspite of being a const parameter it is possible to change its value in
  the method call. This behaviour is normal as explained here:
  http://stackoverflow.com/questions/7413899/record-methods-and-const-parameters-in-delphi
  Using const in this case is equivalent as passing by reference (or var
  parameter).
}
procedure TestDynamicRecord.Test_passing_DynamicRecord_argument_as_const_parameter;
var
  R : DynamicRecord;
begin
  R.Data.B := False;
  R.Data.I := 10;
  R.Data.S := 'string';
  R.Data.F := 3.14;
  PassingArgumentByConstParam(R);
  Assert.IsTrue(R.Data.NewValue = 'Test');
  Assert.IsTrue(R.Data.NewValue2 = 'Test');
end;

{ When passing a DynamicRecord value through a IDynamicRecord parameter, an implicit
  copy is created by the overloaded assignment operator of DynamicRecord. }
procedure TestDynamicRecord.Test_passing_DynamicRecord_argument_as_IDynamicRecord_parameter;
var
  R : DynamicRecord;
begin
  R.Data.B := False;
  R.Data.I := 10;
  R.Data.S := 'string';
  R.Data.F := 3.14;
  PassingArgumentByInterfaceParam(R);
  Assert.IsFalse(R.Data.NewValue = 'Test');
  Assert.IsFalse(R.Data.NewValue2 = 'Test');
end;

{ When passing a DynamicRecord by value, an implicit copy is created. }
procedure TestDynamicRecord.Test_passing_DynamicRecord_argument_as_value_parameter;
var
  R : DynamicRecord;
begin
  R.Data.B := False;
  R.Data.I := 10;
  R.Data.S := 'string';
  R.Data.F := 3.14;
  PassingArgumentByValueParam(R);
  Assert.IsFalse(R.Data.NewValue = 'Test');
  Assert.IsFalse(R.Data.NewValue2 = 'Test');
end;

{ When passing a DynamicRecord by reference (var parameter), no copy is created. }
procedure TestDynamicRecord.Test_passing_DynamicRecord_argument_as_var_parameter;
begin
  PassingArgumentByVarParam(FRecord);
  Assert.IsTrue(FRecord.Data.NewValue = 'Test');
  Assert.IsTrue(FRecord.Data.NewValue2 = 'Test');
end;

{ As the IDynamicRecord argument is implicitly casted to DynamicRecord, a copy of
  its content is made. }
procedure TestDynamicRecord.Test_passing_IDynamicRecord_argument_as_value_parameter;
begin
  PassingArgumentByValueParam(FDynamicRecord);
  Assert.IsFalse(FDynamicRecord.Data.NewValue = 'Test');
  Assert.IsFalse(FDynamicRecord.Data.NewValue2 = 'Test');
end;

{ As the IDynamicRecord argument is implicitly casted to DynamicRecord, a copy of
  its content is made. }
procedure TestDynamicRecord.Test_passing_IDynamicRecord_argument_as_const_parameter;
begin
  PassingArgumentByConstParam(FDynamicRecord);
  Assert.IsFalse(FDynamicRecord.Data.NewValue = 'Test');
  Assert.IsFalse(FDynamicRecord.Data.NewValue2 = 'Test');
  Status(FDynamicRecord.ToString);
end;

{ As we pass the reference, no copy is made. }
procedure TestDynamicRecord.Test_passing_IDynamicRecord_argument_as_IDynamicRecord_parameter;
begin
  PassingArgumentByInterfaceParam(FDynamicRecord);
  Assert.IsTrue(FDynamicRecord.Data.NewValue = 'Test');
  Assert.IsTrue(FDynamicRecord.Data.NewValue2 = 'Test');
  Status(FDynamicRecord.ToString);
end;
{$ENDREGION}

{$REGION 'Tests for methods that convert the TValue to the destination type'}
procedure TestDynamicRecord.Test_ToBoolean_method;
begin
  Assert.IsTrue(FRecord.ToBoolean(TEST_STRING, True), TEST_STRING);
  Assert.IsTrue(FRecord.ToBoolean(TEST_STRING_INTEGER, True), TEST_STRING_INTEGER);
  Assert.IsTrue(FRecord.ToBoolean(TEST_STRING_DOUBLE, True), TEST_STRING_DOUBLE);
  Assert.IsTrue(FRecord.ToBoolean(TEST_DOUBLE, True), TEST_DOUBLE);
  Assert.IsTrue(FRecord.ToBoolean(TEST_BOOLEAN, True), TEST_BOOLEAN);
  Assert.IsTrue(FRecord.ToBoolean(TEST_INTEGER, True), TEST_INTEGER);
end;

procedure TestDynamicRecord.Test_ToFloat_method;
begin
  Assert.AreEqual(Double(5), FRecord.ToFloat(TEST_INTEGER), TEST_INTEGER);
  Assert.IsTrue(IsZero(FRecord.ToFloat(TEST_STRING)), TEST_STRING);
  Assert.IsTrue(IsZero(FRecord.ToFloat(TEST_BOOLEAN)), TEST_BOOLEAN);
  Assert.AreEqual(3.14, FRecord.ToFloat(TEST_DOUBLE), 0.001);
end;

procedure TestDynamicRecord.Test_ToInteger_method;
begin
  Assert.AreEqual(1, FRecord.ToInteger(TEST_BOOLEAN), TEST_BOOLEAN);
  Assert.AreEqual(5, FRecord.ToInteger(TEST_INTEGER), TEST_INTEGER);
  Assert.AreEqual(0, FRecord.ToInteger(TEST_STRING), TEST_STRING);
  Assert.AreEqual(5, FRecord.ToInteger(TEST_STRING_INTEGER), TEST_STRING_INTEGER);
  Assert.AreEqual(0, FRecord.ToInteger(TEST_STRING_DOUBLE), TEST_STRING_DOUBLE);

  // float values are not rounded or trunked when converting. Conversion fails.
  Assert.AreEqual(0, FRecord.ToInteger(TEST_DOUBLE), TEST_DOUBLE);
end;

procedure TestDynamicRecord.Test_ToString_method;
begin
  Assert.AreEqual('True', FRecord.ToString(TEST_BOOLEAN), TEST_BOOLEAN);
  Assert.AreEqual('5', FRecord.ToString(TEST_INTEGER), TEST_INTEGER);
  Assert.AreEqual('Test', FRecord.ToString(TEST_STRING), TEST_STRING);
  Assert.AreEqual('5', FRecord.ToString(TEST_STRING_INTEGER), TEST_STRING_INTEGER);
  Assert.AreEqual('3,14', FRecord.ToString(TEST_STRING_DOUBLE), TEST_STRING_DOUBLE);
  Assert.AreEqual('3,14', FRecord.ToString(TEST_DOUBLE), TEST_DOUBLE);
end;

{$REGION 'Operator overload tests'}
{ Assigning a DynamicRecord to a DynamicRecord results in creating a copy. }
procedure TestDynamicRecord.Test_assignment_operator_for_DynamicRecord_to_DynamicRecord;
var
  R : DynamicRecord;
begin
  R := FRecord;
  FRecord[TEST_INTEGER] := 6;
  Assert.AreEqual(5, R[TEST_INTEGER].AsInteger, TEST_INTEGER);
end;

{ Assigning a DynamicRecord to a IDynamicRecord results in creating a copy of the
  content. }
procedure TestDynamicRecord.Test_assignment_operator_for_DynamicRecord_to_IDynamicRecord;
var
  DR  : IDynamicRecord;
begin
  // if DR is nil like here, a new instance with a copy of the data is
  // automatically created by the overloaded assignment operator of DynamicRecord.
  DR := FRecord;
  FRecord[TEST_STRING] := 'another value';
  Assert.AreEqual('Test', DR[TEST_STRING].AsString); // a copy has been made
end;

{ Assigning an IDynamicRecord to a DynamicRecord results in creating a copy. }
procedure TestDynamicRecord.Test_assignment_operator_for_IDynamicRecord_to_DynamicRecord;
var
  R : DynamicRecord;
begin
  R := FDynamicRecord;
  R[TEST_INTEGER] := 6;
  Assert.AreEqual(5, FDynamicRecord[TEST_INTEGER].AsInteger, TEST_INTEGER);
end;
{$ENDREGION}

procedure TestDynamicRecord.Test_AssignProperty_method;
var
  O : TTestClass;
  R : DynamicRecord;
begin
  O := TTestClass.Create;
  try
    O.TestBoolean := True;
    O.TestString  := 'test';
    O.TestInteger := 5;
    O.TestDouble  := 3.14;
    // fill the DynamicRecord instance with property name-value pairs.
    R.AssignProperty(O, TEST_BOOLEAN);
    R.AssignProperty(O, TEST_STRING);
    R.AssignProperty(O, TEST_INTEGER);
    R.AssignProperty(O, TEST_DOUBLE);
    Assert.AreEqual(O.TestBoolean, R[TEST_BOOLEAN].AsBoolean, TEST_BOOLEAN);
    Assert.AreEqual(O.TestInteger, R[TEST_INTEGER].AsInteger, TEST_INTEGER);
    Assert.AreEqual(O.TestString, R[TEST_STRING].AsString, TEST_STRING);
    Assert.AreEqual(O.TestDouble, Double(R[TEST_DOUBLE].AsExtended), TEST_DOUBLE);
  finally
    O.Free;
  end;
end;

procedure TestDynamicRecord.Test_RetrieveRecord;
var
  R : DynamicRecord;
  I : Integer;
begin
  for I := 0 to 100 do
  begin
    RetrieveRecord(RandomData.Name, R);
    Status(R['Test'].AsString);
  end;
  Assert.IsTrue(True, 'OK');
end;

procedure TestDynamicRecord.Test_RetrieveRecordFunction;
var
  R : DynamicRecord;
begin
  R := RetrieveRecordFunction;
  Assert.AreEqual('test', R['Test'].AsString);
end;

procedure TestDynamicRecord.Test_faulty_condition;
begin
  FDynamicRecord[TEST_STRING] := '';
  FRecord := FDynamicRecord;
  Assert.AreEqual('', FRecord[TEST_STRING].AsString);

  FRecord[TEST_STRING] := 'New Value';
  Assert.AreNotEqual('New Value', FDynamicRecord[TEST_STRING].AsString);

  FDynamicRecord[TEST_STRING] := 'Bad';
  Assert.AreNotEqual('Bad', FRecord[TEST_STRING].AsString);

  FRecord := FDynamicRecord;
  FRecord[TEST_STRING] := 'Good';
  Assert.AreEqual('Bad', FDynamicRecord[TEST_STRING].AsString);

  FDynamicRecord := FRecord;
  Assert.AreEqual('Good', FDynamicRecord[TEST_STRING].AsString);

  FDynamicRecord[TEST_STRING] := 'Bad';
  Assert.AreEqual('Good', FRecord[TEST_STRING].AsString);
end;

procedure TestDynamicRecord.Test_From_method_for_object;
var
  R  : DynamicRecord;
  O1 : TTestClass;
  O2 : TTestClass;
begin
  O1 := TTestUtils.CreateTestObject;
  try
    R.From(O1);
    O2 := TTestClass.Create;
    try
      R.AssignTo(O2);
      Assert.AreEqual(O1.TestInteger, O2.TestInteger);
    finally
      O2.Free;
    end;
  finally
    O1.Free;
  end;
end;

procedure TestDynamicRecord.Test_From_method_for_record;
var
  R  : DynamicRecord;
  R1 : TTestRecord;
  R2 : TTestRecord;
begin
  R1 := TTestUtils.CreateTestRecord;
  // REMARK: only fieldvalues can be copied from records. There is no RTTI for
  // record properties
  R.From(R1, False, True);
  R.AssignTo(R2);
  Assert.AreEqual(R1.TestInteger, R2.TestInteger);
end;

procedure TestDynamicRecord.Test_AssignTo_method_for_object;
var
  R  : DynamicRecord;
  O1 : TTestClass;
  O2 : TTestClass;
begin
  O1 := TTestUtils.CreateTestObject;
  try
    R.From(O1);
    O2 := TTestClass.Create;
    try
      R.AssignTo(O2);
      Assert.IsTrue(O1.Equals(O2));
    finally
      O2.Free;
    end;
  finally
    O1.Free;
  end;
end;

procedure TestDynamicRecord.Test_Assign_method_for_generic_IDynamicRecord_argument;
var
  DR : IDynamicRecord<TTestClass>;
begin
  DR := DynamicRecord<TTestClass>.CreateDynamicRecord;
  DR.Data.TestString := 'Some teststring';
  DR.Data.TestChar   := 'A';
  FRecord.Assign(DR);
  Assert.AreEqual(FRecord[TEST_STRING].AsString, DR.Data.TestString);
  Assert.AreEqual(FRecord[TEST_CHAR].AsString, DR.Data.TestChar);
end;

procedure TestDynamicRecord.Test_Assign_method_for_generic_DynamicRecord_argument;
var
  R : DynamicRecord<TTestClass>;
begin
  R.Data.TestString := 'Some teststring';
  R.Data.TestChar   := 'A';
  FRecord.Assign(R);
  Assert.AreEqual(FRecord[TEST_STRING].AsString, R.Data.TestString);
  Assert.AreEqual(FRecord[TEST_CHAR].AsString, R.Data.TestChar);
end;
{$ENDREGION}


{$REGION 'Test From/AssignTo Options'}
procedure TestDynamicRecord.Test_From_Options_AssignNulls;
var
  R : DynamicRecord;
  O : TTestClass;
begin
  O := TTestClass.Create;
  try
    O.TestInteger := 5;
    O.TestString := ''; // Empty string, not null
    //O.FTestSubObject := nil; // This is an object field, will be assigned as nil TValue

    // Test Case 1: AAssignNulls = False (Default)
    R.From<TTestClass>(O, True, True, False); // Props=T, Fields=T, Nulls=F
    Assert.IsTrue(R.ContainsField('TestInteger'));
    Assert.IsTrue(R.ContainsField('TestString'), 'Empty string should be included');
    Assert.AreEqual('', R['TestString'].AsString);
    Assert.IsFalse(R.ContainsField('FTestSubObject'), 'Nil object field should be excluded');

    // Test Case 2: AAssignNulls = True
    R.From<TTestClass>(O, True, True, True); // Props=T, Fields=T, Nulls=T
    Assert.IsTrue(R.ContainsField('TestInteger'));
    Assert.IsTrue(R.ContainsField('TestString'));
    Assert.IsTrue(R.ContainsField('FTestSubObject'), 'Nil object field should be included');
    Assert.IsTrue(R['FTestSubObject'].IsEmpty, 'Value should be TValue.Empty');
    Assert.IsTrue(R['FTestSubObject'].IsObject, 'Value should be kind tkClass'); // It stores nil pointer
    Assert.IsNull(R['FTestSubObject'].AsObject, 'Value should be nil object');

  finally
    O.Free;
  end;
end;

procedure TestDynamicRecord.Test_From_Options_AssignPropsFields;
var
  R: DynamicRecord;
begin
  // Use FTestObject initialized in Setup
  // FTestObject has both properties (TestInteger) and public fields (FPublicField - add this to TTestClass if not present)
  // Let's assume TTestClass has: public FPublicField: string;
  //FTestObject.FPublicField := 'Public Field Data';

  // Test Case 1: Properties=True, Fields=False (Default for From<T>)
  R.From<TTestClass>(FTestObject, True, False);
  Assert.IsTrue(R.ContainsField('TestInteger'), 'Property TestInteger');
  Assert.IsFalse(R.ContainsField('FPublicField'), 'Field FPublicField (Props=T, Fields=F)');
  Assert.IsFalse(R.ContainsField('FPrivateField'), 'Private Field FPrivateField (Props=T, Fields=F)');

  // Test Case 2: Properties=False, Fields=True
  R.From<TTestClass>(FTestObject, False, True);
  Assert.IsFalse(R.ContainsField('TestInteger'), 'Property TestInteger (Props=F, Fields=T)');
  Assert.IsTrue(R.ContainsField('FPublicField'), 'Field FPublicField (Props=F, Fields=T)');
   // Private fields are not included by GetFields unless specific RTTI context allows it
  Assert.IsFalse(R.ContainsField('FPrivateField'), 'Private Field FPrivateField (Props=F, Fields=T)');

  // Test Case 3: Properties=True, Fields=True
  R.From<TTestClass>(FTestObject, True, True);
  Assert.IsTrue(R.ContainsField('TestInteger'), 'Property TestInteger (Props=T, Fields=T)');
  Assert.IsTrue(R.ContainsField('FPublicField'), 'Field FPublicField (Props=T, Fields=T)');
  Assert.IsFalse(R.ContainsField('FPrivateField'), 'Private Field FPrivateField (Props=T, Fields=T)');
end;

procedure TestDynamicRecord.Test_From_Options_SpecificNames;
var
  R: DynamicRecord;
begin
  // Use FTestObject initialized in Setup
  R.From<TTestClass>(FTestObject, True, True, False, ['TestInteger', 'TestString', 'NonExistent']);

  Assert.AreEqual(2, R.Count, 'Count should be 2');
  Assert.IsTrue(R.ContainsField('TestInteger'));
  Assert.IsTrue(R.ContainsField('TestString'));
  Assert.IsFalse(R.ContainsField('TestDouble'), 'TestDouble should not be included');
  Assert.IsFalse(R.ContainsField('NonExistent'), 'NonExistent should not be included');
end;

procedure TestDynamicRecord.Test_AssignTo_Options_AssignNulls;
var
  R: DynamicRecord;
  O: TTestClass;
begin
  O := TTestClass.Create;
  try
    O.TestInteger := 5;
    O.TestString := 'Initial';
    // Create a record with a null/empty value representation
    R['TestInteger'] := 10;
    R['TestString'] := TValue.Empty; // Represents null/unassigned

    // Test Case 1: AssignNulls = False (Default)
    //R.AssignTo<TTestClass>(O, True, False, False); // Props=T, Fields=F, Nulls=F
    Assert.AreEqual(10, O.TestInteger);
    Assert.AreEqual('Initial', O.TestString, 'Empty TValue should not overwrite existing string');

    // Test Case 2: AssignNulls = True
    // Reset O
    O.TestInteger := 5;
    O.TestString := 'Initial';
    //R.AssignTo<TTestClass>(O, True, False, True); // Props=T, Fields=F, Nulls=T
    Assert.AreEqual(10, O.TestInteger);
    // Assigning TValue.Empty to a string property usually results in an empty string
    Assert.AreEqual('', O.TestString, 'Empty TValue should assign empty string');

  finally
    O.Free;
  end;
end;

procedure TestDynamicRecord.Test_AssignTo_Options_SpecificNames;
var
  R : DynamicRecord;
  O : TTestClass;
begin
  O := TTestClass.Create;
  try
    O.TestInteger := 0;
    O.TestString := 'Initial';
    O.TestDouble := 0.0;

    R['TestInteger'] := 10;
    R['TestString'] := 'New String';
    R['TestDouble'] := 3.14;

    R.AssignTo(TValue.From<TTestClass>(O), True, False, False, ['TestInteger', 'TestDouble', 'NonExistent']);

    Assert.AreEqual(10, O.TestInteger, 'TestInteger should be assigned');
    Assert.AreEqual(3.14, O.TestDouble, 0.001, 'TestDouble should be assigned');
    Assert.AreEqual('Initial', O.TestString, 'TestString should NOT be assigned');

  finally
    O.Free;
  end;
end;
{$ENDREGION}

{$REGION 'Test Nullable Types'}
//procedure TestDynamicRecord.Test_From_With_Nullable_Properties;
//var
//  R: DynamicRecord;
//  NR: TNullableRecord;
//begin
//  NR.NullableIntWithValue := 123;
//  NR.NullableIntWithoutValue := nil; // Or Nullable<Integer>.Null
//  NR.NullableStringWithValue := 'Has Value';
//  NR.RegularInt := 456;
//
//  R.From<TNullableRecord>(NR, True, False); // Properties only
//
//  Assert.IsTrue(R.ContainsField('PropNullableIntWithValue'), 'NullableIntWithValue Prop');
//  Assert.AreEqual(123, R['PropNullableIntWithValue'].AsInteger);
//
//  Assert.IsTrue(R.ContainsField('PropNullableIntWithoutValue'), 'NullableIntWithoutValue Prop');
//  Assert.IsTrue(R['PropNullableIntWithoutValue'].IsEmpty, 'Value should be TValue.Empty');
//
//  Assert.IsTrue(R.ContainsField('PropNullableStringWithValue'), 'NullableStringWithValue Prop');
//  Assert.AreEqual('Has Value', R['PropNullableStringWithValue'].AsString);
//
//  Assert.IsTrue(R.ContainsField('PropRegularInt'), 'RegularInt Prop');
//  Assert.AreEqual(456, R['PropRegularInt'].AsInteger);
//end;
//
//procedure TestDynamicRecord.Test_AssignTo_With_Nullable_Properties;
//var
//  R: DynamicRecord;
//  NR: TNullableRecord;
//begin
//  R['PropNullableIntWithValue'] := 123;
//  R['PropNullableIntWithoutValue'] := TValue.Empty; // Simulate null
//  R['PropNullableStringWithValue'] := 'Assigned Value';
//  R['PropRegularInt'] := 456;
//
//  // Assign R to NR
//  R.AssignTo<TNullableRecord>(NR);
//
//  Assert.IsTrue(NR.NullableIntWithValue.HasValue);
//  Assert.AreEqual(123, NR.NullableIntWithValue.Value);
//
//  Assert.IsFalse(NR.NullableIntWithoutValue.HasValue, 'Assigning TValue.Empty should result in no value');
//
//  Assert.IsTrue(NR.NullableStringWithValue.HasValue);
//  Assert.AreEqual('Assigned Value', NR.NullableStringWithValue.Value);
//
//  Assert.AreEqual(456, NR.RegularInt);
//end;
{$ENDREGION}

{$REGION 'Test COW Behavior'}
procedure TestDynamicRecord.Test_CopyOnWrite_Behavior;
var
  R1, R2: DynamicRecord;
  InitialRefCount: Integer;
begin
  R1['A'] := 1;
  R1['B'] := 'Original';
  Assert.AreEqual(0, R1.InternalRefCount, 'Initial RefCount should be 0 (internal interface only)');

  R2 := R1; // Assignment creates a copy because R1 holds the interface internally

  // R1 and R2 should now have independent internal interfaces
  //Assert.AreNotSame(Pointer(R1.InternalDynamicRecord), Pointer(R2.InternalDynamicRecord),
    //'Internal interfaces should be different after record assignment');
  Assert.AreEqual(0, R1.InternalRefCount, 'R1 RefCount after assignment');
  Assert.AreEqual(0, R2.InternalRefCount, 'R2 RefCount after assignment');

  // Modify R2
  R2['A'] := 2;
  R2['B'] := 'Modified';

  // Verify R1 is unchanged
  Assert.AreEqual(1, R1['A'].AsInteger, 'R1[A] should be unchanged');
  Assert.AreEqual('Original', R1['B'].AsString, 'R1[B] should be unchanged');

  // Verify R2 is changed
  Assert.AreEqual(2, R2['A'].AsInteger, 'R2[A] should be changed');
  Assert.AreEqual('Modified', R2['B'].AsString, 'R2[B] should be changed');

  // Test COW with interface assignment
  var DR1: IDynamicRecord := R1; // Implicit cast - should copy data to new interface
  var DR2: IDynamicRecord;

  // Data was copied, R1's internal interface should still have refcount 0
  Assert.AreEqual(0, R1.InternalRefCount, 'R1 RefCount after DR1 := R1');
  Assert.IsNotNull(DR1);
  // The *new* interface DR1 holds should have refcount 0 (held only by DR1)
  Assert.AreEqual(0, DR1.RefCount, 'DR1 RefCount after DR1 := R1');


  DR2 := DR1; // Interface assignment, reference is copied
  Assert.AreEqual(1, DR1.RefCount, 'DR1 RefCount should be 1 after DR2 := DR1');
  Assert.AreEqual(1, DR2.RefCount, 'DR2 RefCount should be 1 after DR2 := DR1');
  Assert.AreSame(Pointer(DR1), Pointer(DR2), 'DR1 and DR2 should point to the same interface instance');

  // Now, assign the shared interface back to a record. This should trigger a copy.
  R2 := DR2;
  Assert.AreEqual(1, DR1.RefCount, 'DR1 RefCount should still be 1 after R2 := DR2');
  Assert.AreEqual(1, DR2.RefCount, 'DR2 RefCount should still be 1 after R2 := DR2');
  //Assert.AreNotSame(Pointer(R2.InternalDynamicRecord), Pointer(DR2),
   //'R2 internal interface should be different from DR2 after R2 := DR2');

  // Modify R2 again, should not affect DR1/DR2
  R2['A'] := 3;
  Assert.AreEqual(1, DR1['A'].AsInteger, 'DR1[A] should be unchanged after modifying R2');
  Assert.AreEqual(1, DR2['A'].AsInteger, 'DR2[A] should be unchanged after modifying R2');
  Assert.AreEqual(3, R2['A'].AsInteger, 'R2[A] should be 3');
end;
{$ENDREGION}

{$REGION 'Test Data Variant Property'}
procedure TestDynamicRecord.Test_Data_Variant_Write;
var
  R: DynamicRecord;
begin
  R['IntVal'] := 10;
  R['StrVal'] := 'Hello';

  //Assert.AreEqual(10, R.Data.IntVal);
  //Assert.AreEqual('Hello', R.Data.StrVal);

  // Write through the variant property
  R.Data.IntVal := 25;
  R.Data.StrVal := 'World';
  R.Data.NewVal := True; // Add a new field

  // Verify changes are reflected in the DynamicRecord
  Assert.AreEqual(25, R['IntVal'].AsInteger, 'IntVal after variant write');
  Assert.AreEqual('World', R['StrVal'].AsString, 'StrVal after variant write');
  Assert.IsTrue(R.ContainsField('NewVal'), 'NewVal should exist');
  Assert.AreEqual(True, R['NewVal'].AsBoolean, 'NewVal value check');
  //Assert.AreEqual(True, R.Data.NewVal, 'NewVal access via variant');
end;

procedure TestDynamicRecord.Test_Access_NonExistent_Field_Via_Variant_Data;
var
  R: DynamicRecord;
  V: Variant;
begin
  R['Existing'] := 1;
  // Reading non-existent property via variant raises exception
  Assert.WillRaise(
    procedure
    begin
      V := R.Data.NonExistent;
    end,
    Exception, 'EVariantError or specific exception expected' // Exact type might vary
  );
end;


{$ENDREGION}

{$REGION 'Other Tests'}

//procedure TestDynamicRecord.Test_FromPersistent;
//var
//  P: TTestPersistent;
//  R: DynamicRecord;
//begin
//  P := TTestPersistent.Create;
//  try
//    P.IntValue := 99;
//    P.StrValue := 'Persistent Test';
//    R.FromPersistent(P);
//
//    Assert.AreEqual(2, R.Count, 'Count should be 2');
//    Assert.IsTrue(R.ContainsField('IntValue'));
//    Assert.AreEqual(99, R['IntValue'].AsInteger);
//    Assert.IsTrue(R.ContainsField('StrValue'));
//    Assert.AreEqual('Persistent Test', R['StrValue'].AsString);
//  finally
//    P.Free;
//  end;
//end;

procedure TestDynamicRecord.Test_ToDelimitedText_Quoting;
var
  R: DynamicRecord;
begin
  R['Field1'] := 'Value1';
  R['Field2'] := 'Value with, comma';
  R['Field3'] := 'Value with " quote';
  R['Field4'] := '';

  // No quoting
  Assert.AreEqual('Value1|Value with, comma|Value with " quote|',
    R.ToDelimitedText('|', False), 'No Quoting');

  // With quoting (double quote default)
  Assert.AreEqual('"Value1"|"Value with, comma"|"Value with "" quote"|""',
    R.ToDelimitedText('|', True), 'Default Quoting');

  // With quoting (single quote)
  Assert.AreEqual('''Value1''|''Value with, comma''|''Value with " quote''|''''',
    R.ToDelimitedText('|', True, ''''), 'Single Quote Quoting');

  // Specific fields, quoting
  Assert.AreEqual('"Value with, comma"|"Value1"',
    R.ToDelimitedText('Field2,Field1', '|', True), 'Specific Fields Quoting');

end;

procedure TestDynamicRecord.Test_AssignProperty_Error;
var
  O: TTestClass;
  R: DynamicRecord;
begin
  O := TTestClass.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        R.AssignProperty(O, 'NonExistentProperty');
      end,
      Exception, 'Should raise for non-existent property'
    );
  finally
    O.Free;
  end;
end;

procedure TestDynamicRecord.Test_FromArray_Bracketed;
var
  R: DynamicRecord;
  IA: TArray<Integer>;
begin
  IA := [10, 20, 30];
  R.FromArray<Integer>(IA, True); // Bracketed names = True

  Assert.AreEqual(3, R.Count);
  Assert.IsTrue(R.ContainsField('[0]'));
  Assert.AreEqual(10, R['[0]'].AsInteger);
  Assert.IsTrue(R.ContainsField('[1]'));
  Assert.AreEqual(20, R['[1]'].AsInteger);
  Assert.IsTrue(R.ContainsField('[2]'));
  Assert.AreEqual(30, R['[2]'].AsInteger);
  Assert.IsFalse(R.ContainsField('0'), 'Should use bracketed name');

  // Check string representation
  var ExpectedStr := '[0] = 10' + #13#10 +
                     '[1] = 20' + #13#10 +
                     '[2] = 30';
   Assert.AreEqual(ExpectedStr, R.ToString(False)); // No align for easier comparison
end;

procedure TestDynamicRecord.Test_Clear_Method;
begin
  Assert.AreEqual(7, FRecord.Count, 'Initial count');
  Assert.IsFalse(FRecord.IsEmpty);

  FRecord.Clear;

  Assert.AreEqual(0, FRecord.Count, 'Count after Clear');
  Assert.IsTrue(FRecord.IsEmpty, 'IsEmpty after Clear');
  Assert.IsFalse(FRecord.ContainsField(TEST_INTEGER), 'Field should be gone');
end;

procedure TestDynamicRecord.Test_Conversion_Failure_Default_Value;
begin
  FRecord['NotANumber'] := 'ABC';
  FRecord['NotABoolean'] := 'Maybe';

  Assert.AreEqual(99, FRecord.ToInteger('NotANumber', 99), 'Integer default');
  Assert.AreEqual(-1.0, FRecord.ToFloat('NotANumber', -1.0), 0.001, 'Float default');
  Assert.AreEqual(True, FRecord.ToBoolean('NotABoolean', True), 'Boolean default True');
  Assert.AreEqual(False, FRecord.ToBoolean('NotABoolean', False), 'Boolean default False');
  Assert.AreEqual('Default', FRecord.ToString('NonExistentField', 'Default'), 'String default');

  // Test default for valid field but wrong type
  Assert.AreEqual(0, FRecord.ToInteger(TEST_STRING, 0), 'Integer default for string field');
end;

// Reimplement Test_From_method_for_record to test properties if desired
// Note: Standard RTTI for records doesn't expose properties well before XE7/XE8.
// The current implementation likely relies on GetFields.
//procedure TestDynamicRecord.Test_From_method_for_record_fields;
//var
//  R  : DynamicRecord;
//  R1 : TTestRecord;
//begin
//  R1 := TTestUtils.CreateTestRecord;
//  // Use AssignFields = True for records
//  R.From(R1, False, True); // Props=F, Fields=T
//  Assert.IsTrue(R.ContainsField('TestInteger'));
//  Assert.AreEqual(R1.TestInteger, R.ToInteger('TestInteger'));
//  Assert.IsTrue(R.ContainsField('TestString'));
//  Assert.AreEqual(R1.TestString, R.ToString('TestString'));
//  // Add assertions for other fields (TestDouble, TestBoolean)
//end;
//
//// Add a corresponding AssignTo test
//procedure TestDynamicRecord.Test_AssignTo_method_for_Record_fields;
//var
//  R  : DynamicRecord;
//  R1 : TTestRecord;
//  R2 : TTestRecord; // Target record
//begin
//  R1 := TTestUtils.CreateTestRecord;
//  // Populate DynamicRecord from R1 fields
//  R.From(R1, False, True);
//
//  // Modify a value in DynamicRecord
//  R['TestInteger'] := R1.TestInteger + 10;
//
//  // Assign DynamicRecord to R2 fields
//  R.AssignTo(R2, False, True); // Assign fields only
//
//  // Verify R2 fields match the (modified) DynamicRecord
//  Assert.AreEqual(R1.TestInteger + 10, R2.TestInteger, 'Assigned TestInteger');
//  Assert.AreEqual(R1.TestString, R2.TestString, 'Assigned TestString');
//  // Add assertions for other fields
//end;


{$ENDREGION}


end.

