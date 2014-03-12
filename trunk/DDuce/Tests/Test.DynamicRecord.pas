unit Test.DynamicRecord;

interface

uses
  DB, Contnrs, SysUtils, DBClient, ActnList, Windows, StdCtrls, Messages,
  Variants, Controls, Generics.Collections, Classes, Dialogs, Forms, Buttons,
  Graphics,

  DDuce.DynamicRecord, DDuce.RandomData,

  TestFramework, // DUnit

  Test.DynamicRecord.Data;

type
  // Test methods for class TfrmMain
  TestTRecord = class(TTestCase)
  private
    FRecord: TRecord;

    function RetrieveRecord(
      const AString : string;
       var  ARecord : TRecord
    ): Boolean;
    function CreateDataSet: TDataSet;
    function RetrieveRecordFunction: TRecord;

    procedure PassingRecordThroughParam(ARecord: TRecord);
    procedure PassingRecordThroughConstParam(const ARecord: TRecord);
    procedure PassingRecordThroughVarParam(var ARecord: TRecord);
    procedure PassingRecordThroughInterfaceParam(const ARecord: IDynamicRecord);

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestPassingRecordAsConstTRecordParameter;
    procedure TestPassingRecordAsVarTRecordParameter;
    procedure TestPassingRecordAsTRecordParameter;
    procedure TestPassingRecordAsIDynamicRecordParameter;

    procedure TestContainsField;
    procedure TestDeleteField;
    procedure TestIsEmpty;

    procedure TestConversions;

    // test methods with automatic conversion to destination type
    procedure TestToFloat;
    procedure TestToInteger;
    procedure TestToString;
    procedure TestToBoolean;
    procedure TestIsBlank;

    procedure TestFromDataSet;

    procedure TestFromStrings;
    procedure TestToStrings;

    procedure TestAsDelimitedText;
    procedure TestAsVarArray;
    procedure TestAsCommaText;

    procedure TestAssignProperty;

    procedure TestRetrieveRecord;

    procedure TestRetrieveRecordFunction;

  end;

implementation

uses
  Math, Types, Rtti;

const
  TEST_INTEGER            = 'TestInteger';
  TEST_DOUBLE             = 'TestDouble';
  TEST_BOOLEAN            = 'TestBoolean';
  TEST_STRING             = 'TestString';
  TEST_TVALUE             = 'TestTValue';
  TEST_VARIANT            = 'TestVariant';
  TEST_NON_EXISTENT_VALUE = 'TestNonExistentValue';

procedure TestTRecord.SetUp;
begin
  FRecord[TEST_INTEGER] := 5;
  FRecord[TEST_STRING]  := 'Test';
  FRecord[TEST_BOOLEAN] := True;
  FRecord[TEST_DOUBLE]  := 3.14;
end;

procedure TestTRecord.TearDown;
begin
  FRecord.Clear;
end;

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

procedure TestTRecord.PassingRecordThroughConstParam(const ARecord: TRecord);
var
  S : string;
  F : IDynamicField;
begin
  //ARecord.Data.NewValue := 'Test';
  for F in ARecord do
  begin
    Status(F.ToString);
  end;
  S := Format('PassingRecordThroughConstParam: '#13#10'%s', [ARecord.ToString]);
  //Status(S);
end;

procedure TestTRecord.PassingRecordThroughInterfaceParam(
  const ARecord: IDynamicRecord);
var
  S : string;
  F : IDynamicField;
begin
  S := Format('PassingRecordThroughInterfaceParam: '#13#10'%s', [ARecord.ToString]);
//    for F in ARecord do
//  begin
//    Status(F.ToString);
//  end;
  //Status(S);
end;

procedure TestTRecord.PassingRecordThroughParam(ARecord: TRecord);
var
  S : string;
  F : IDynamicField;

begin
  //FStatusStrings.Add('PassingRecordThroughParam');
  for F in ARecord do
  begin
    Status(F.ToString);
  end;


//  S := Format('PassingRecordThroughParam: '#13#10'%s', [ARecord.ToString]);
//  Status(S);
end;

procedure TestTRecord.PassingRecordThroughVarParam(var ARecord: TRecord);
var
  S : string;
begin
  S := Format('PassingRecordThroughVarParam: '#13#10'%s', [ARecord.ToString]);
  Status(S);
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

function TestTRecord.RetrieveRecordFunction: TRecord;
begin
  Result['Test'] := 'test';
end;


{$REGION 'Test methods'}
procedure TestTRecord.TestAsCommaText;
begin
  CheckEqualsString(
    'TestInteger = 5,TestString = Test,TestBoolean = True,TestDouble = 3,14',
    FRecord.AsCommaText
  );
end;

procedure TestTRecord.TestAsDelimitedText;
begin
  CheckEqualsString('5,Test,True,3,14', FRecord.AsDelimitedText(','));
  CheckEqualsString('5', FRecord.AsDelimitedText(TEST_INTEGER, ','));
end;

procedure TestTRecord.TestAssignProperty;
var
  O  : TTestClass;
  //TR : TTestRecord;
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

  finally
    O.Free;
  end;
end;

procedure TestTRecord.TestAsVarArray;
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
end;

procedure TestTRecord.TestContainsField;
begin
  // Test contains
  CheckTrue(FRecord.ContainsField(TEST_INTEGER), TEST_INTEGER);
  CheckTrue(FRecord.ContainsField(UpperCase(TEST_INTEGER)), TEST_INTEGER);
  CheckTrue(FRecord.ContainsField(TEST_STRING), TEST_STRING);
  CheckTrue(FRecord.ContainsField(TEST_BOOLEAN), TEST_BOOLEAN);
  CheckFalse(FRecord.ContainsField(TEST_NON_EXISTENT_VALUE), TEST_NON_EXISTENT_VALUE);
  CheckTrue(FRecord.ContainsField(TEST_DOUBLE), TEST_DOUBLE);
end;

procedure TestTRecord.TestConversions;
begin
  CheckEquals('3,14', FRecord.ToString(TEST_DOUBLE), TEST_DOUBLE);
  CheckEquals('5', FRecord.ToString(TEST_INTEGER), TEST_INTEGER);
  CheckEquals('Test', FRecord.ToString(TEST_STRING), TEST_STRING);
  CheckEquals('True', FRecord.ToString(TEST_BOOLEAN), TEST_BOOLEAN);
end;

procedure TestTRecord.TestDeleteField;
begin
  // Test DeleteField
  CheckTrue(FRecord.DeleteField(TEST_INTEGER), TEST_INTEGER);
  CheckFalse(FRecord.ContainsField(TEST_INTEGER), TEST_INTEGER);
  CheckFalse(FRecord.ContainsField(TEST_INTEGER), TEST_INTEGER);
  CheckTrue(FRecord.ContainsField(TEST_STRING), TEST_STRING);
end;

procedure TestTRecord.TestFromDataSet;
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
end;

procedure TestTRecord.TestFromStrings;
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
end;

procedure TestTRecord.TestIsBlank;
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
  R[S] := TValue.FromVariant(EmptyParam);
  CheckTrue(R.IsBlank(S), 'EmptyParam');
  R[S] := TValue.FromVariant('');
  CheckTrue(R.IsBlank(S), 'Empty string');
end;

procedure TestTRecord.TestIsEmpty;
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

procedure TestTRecord.TestPassingRecordAsConstTRecordParameter;
var
  R : TRecord;
begin
  R.Data.B := False;
  R.Data.I := 10;
  R.Data.S := 'string';
  R.Data.F := 3.14;
  PassingRecordThroughConstParam(R);
end;

procedure TestTRecord.TestPassingRecordAsIDynamicRecordParameter;
var
  R : TRecord;
begin
  R.Data.B := False;
  R.Data.I := 10;
  R.Data.S := 'string';
  R.Data.F := 3.14;
  PassingRecordThroughInterfaceParam(R);
end;

procedure TestTRecord.TestPassingRecordAsTRecordParameter;
var
  R : TRecord;
  I : Integer;
begin
  R.Data.B := False;
  R.Data.I := 10;
  R.Data.S := 'string';
  R.Data.F := 3.14;
  PassingRecordThroughParam(R);

  for I := 0 to 100 do
  begin
    R.Data.B := True;
    R.Data.I := RandomData.Number(20);
    R.Data.S := RandomData.CompanyName;
    R.Data.F := RandomData.Number(457);

    PassingRecordThroughParam(R);
    PassingRecordThroughConstParam(R);
    PassingRecordThroughVarParam(R);
    PassingRecordThroughInterfaceParam(R);
  end;
end;

procedure TestTRecord.TestPassingRecordAsVarTRecordParameter;
begin
  PassingRecordThroughVarParam(FRecord);
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
end;

procedure TestTRecord.TestRetrieveRecordFunction;
var
  R : TRecord;
begin
  R := RetrieveRecordFunction;
  CheckEquals('test', R['Test'].AsString);
end;

procedure TestTRecord.TestToBoolean;
begin
  CheckTrue(FRecord.ToBoolean(TEST_STRING, True), TEST_STRING);
  CheckTrue(FRecord.ToBoolean(TEST_DOUBLE, True), TEST_DOUBLE);
  CheckTrue(FRecord.ToBoolean(TEST_BOOLEAN), TEST_BOOLEAN);
  CheckTrue(FRecord.ToBoolean(TEST_INTEGER, True), TEST_INTEGER);
end;

procedure TestTRecord.TestToFloat;
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

procedure TestTRecord.TestToInteger;
begin
  CheckEquals(0, FRecord.ToInteger(TEST_BOOLEAN), TEST_BOOLEAN);
  CheckEquals(5, FRecord.ToInteger(TEST_INTEGER), 5, TEST_INTEGER);
  CheckEquals(0, FRecord.ToInteger(TEST_STRING), TEST_STRING);
  CheckEquals(3, FRecord.ToInteger(TEST_DOUBLE), TEST_DOUBLE);
end;

procedure TestTRecord.TestToString;
begin
  CheckEquals('True', FRecord.ToString(TEST_BOOLEAN), TEST_BOOLEAN);
  CheckEquals('5', FRecord.ToString(TEST_INTEGER), TEST_INTEGER);
  CheckEquals('Test', FRecord.ToString(TEST_STRING), TEST_STRING);
  CheckEquals('3,14', FRecord.ToString(TEST_DOUBLE), TEST_DOUBLE);
end;

procedure TestTRecord.TestToStrings;
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

initialization
  // Register any test cases with the test runner
  RegisterTest('TRecord', TestTRecord.Suite);
end.

