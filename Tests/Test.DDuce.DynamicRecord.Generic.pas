{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Test.DDuce.DynamicRecord.Generic;

interface

uses
  System.Variants, System.SysUtils, System.Contnrs, System.Classes,
  Data.DB,
  Datasnap.DBClient,
  Winapi.Windows, Winapi.Messages,
  Vcl.Controls, Vcl.Dialogs, Vcl.Forms, Vcl.Buttons, Vcl.ActnList, Vcl.StdCtrls,
  Vcl.Graphics,

  DDuce.DynamicRecord,

  TestFramework, // DUnit

  Test.DDuce.DynamicRecord.Data;

type
  // Test methods for class TfrmMain
  TestGenericTRecord = class(TTestCase)
  private
    FObject: TTestClass;
    FRecord : TRecord<TTestClass>;

    function RetrieveRecord(
      const AString : string;
       var  ARecord : TRecord
    ): Boolean;

    function RetrieveRecordFunction: TRecord;
    function CreateDataSet: TDataSet;
    procedure ProcessRecord(ARecord: IDynamicRecord);

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestTRecordAssignments;
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
function TestGenericTRecord.CreateDataSet: TDataSet;
begin

end;

procedure TestGenericTRecord.ProcessRecord(ARecord: IDynamicRecord);
begin
  Status('ProcessRecord:');
  Status(ARecord.ToString);
end;

function TestGenericTRecord.RetrieveRecord(const AString: string;
  var ARecord: TRecord): Boolean;
begin

end;

function TestGenericTRecord.RetrieveRecordFunction: TRecord;
begin

end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TestGenericTRecord.SetUp;
begin
  inherited;
  FObject := TTestClass.Create;
  FObject.TestBoolean := True;
  FObject.TestString  := 'Test';
  FObject.TestInteger := 5;
  FObject.TestDouble  := 3.14;
  FRecord.Data.TestBoolean := True;
  FRecord.Data.TestString  := 'Test';
  FRecord.Data.TestInteger := 5;
  FRecord.Data.TestDouble  := 3.14;
end;

procedure TestGenericTRecord.TearDown;
begin
  FreeAndNil(FObject);
  inherited;
end;
{$ENDREGION}

{$REGION 'Test methods'}
procedure TestGenericTRecord.TestAsCommaText;
var
  R : TRecord<TTestClass>;
  T : TRecord;
begin
//  R.Create(FObject);
//  Status(R.ToString);
//  T := R;
  ProcessRecord(FRecord);
end;

procedure TestGenericTRecord.TestAsDelimitedText;
begin

end;

procedure TestGenericTRecord.TestAssignProperty;
begin

end;

procedure TestGenericTRecord.TestAsVarArray;
begin

end;

procedure TestGenericTRecord.TestContainsField;
var
  R : TRecord<TTestClass>;
begin
  R.Create;
  CheckTrue(R.ContainsField('TestChar'));
end;

procedure TestGenericTRecord.TestConversions;
var
  O  : TTestClass;
  //TR : TTestRecord;
  R  : TRecord<TTestClass>;
begin
  O := TTestClass.Create;
  try
    O.TestBoolean := True;
    O.TestString  := 'test';
    O.TestInteger := 5;
    O.TestDouble  := 3.14;
    R.Create(O);
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

  finally
    O.Free;
  end;
end;

procedure TestGenericTRecord.TestDeleteField;
begin
        // should not be possible
end;

procedure TestGenericTRecord.TestFromDataSet;
begin

end;

procedure TestGenericTRecord.TestFromStrings;
begin

end;

procedure TestGenericTRecord.TestIsBlank;
var
  R: TRecord<TTestClass>;
begin
  R[TEST_STRING] := '';
  CheckTrue(R.IsBlank(TEST_STRING), TEST_STRING);
end;

procedure TestGenericTRecord.TestIsEmpty;
begin
//
end;

procedure TestGenericTRecord.TestRetrieveRecord;
begin
//
end;

procedure TestGenericTRecord.TestRetrieveRecordFunction;
begin
//
end;

procedure TestGenericTRecord.TestToBoolean;
begin
  CheckTrue(FRecord.ToBoolean(TEST_STRING, True), TEST_STRING);
  //CheckTrue(FRecord.ToBoolean(TEST_STRING_INTEGER, True), TEST_STRING_INTEGER);
  //CheckTrue(FRecord.ToBoolean(TEST_STRING_DOUBLE, True), TEST_STRING_DOUBLE);
  CheckTrue(FRecord.ToBoolean(TEST_DOUBLE, True), TEST_DOUBLE);
  CheckTrue(FRecord.ToBoolean(TEST_BOOLEAN, True), TEST_BOOLEAN);
  CheckTrue(FRecord.ToBoolean(TEST_INTEGER, True), TEST_INTEGER);
end;

procedure TestGenericTRecord.TestToFloat;
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

procedure TestGenericTRecord.TestToInteger;
begin
  CheckEquals(1, FRecord.ToInteger(TEST_BOOLEAN), TEST_BOOLEAN);
  CheckEquals(5, FRecord.ToInteger(TEST_INTEGER), TEST_INTEGER);
  CheckEquals(0, FRecord.ToInteger(TEST_STRING), TEST_STRING);
  //CheckEquals(5, FRecord.ToInteger(TEST_STRING_INTEGER), TEST_STRING_INTEGER);
//  CheckEquals(0, FRecord.ToInteger(TEST_STRING_DOUBLE), TEST_STRING_DOUBLE);

  // float values are not rounded or trunked when converting. Conversion fails.
  CheckEquals(0, FRecord.ToInteger(TEST_DOUBLE), TEST_DOUBLE);
end;

procedure TestGenericTRecord.TestToString;
begin
  CheckEquals('True', FRecord.ToString(TEST_BOOLEAN), TEST_BOOLEAN);
  CheckEquals('5', FRecord.ToString(TEST_INTEGER), TEST_INTEGER);
  CheckEquals('Test', FRecord.ToString(TEST_STRING), TEST_STRING);
//CheckEquals('5', FRecord.ToString(TEST_STRING_INTEGER), TEST_STRING_INTEGER);
  //CheckEquals('3,14', FRecord.ToString(TEST_STRING_DOUBLE), TEST_STRING_DOUBLE);
  CheckEquals('3,14', FRecord.ToString(TEST_DOUBLE), TEST_DOUBLE);
end;

procedure TestGenericTRecord.TestToStrings;
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
  DR := TRecord.Create;
  DR[TEST_INTEGER] := 5;
  Status(DR.ToString);

  //DCR := TRecord<TTestClass>.Create(FObject);
  Status(DCR.ToString);

  // test TRecord<T> => IDynamicRecord<T>
  DCGR := TRecord<TTestClass>.Create(FObject);
  DCGR.Data.TestInteger := 50;
  Status(DCGR.ToString);

  DCR := DCGR;
  DCR.Data.TestString := 'String';
  Status(DCR.ToString);
end;
{$ENDREGION}

end.

