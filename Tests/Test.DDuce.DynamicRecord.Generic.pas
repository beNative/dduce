{
  Copyright (C) 2013-2014 Tim Sinaeve tim.sinaeve@gmail.com

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
  FObject.TestString  := 'test';
  FObject.TestInteger := 5;
  FObject.TestDouble  := 3.14;
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
  R.Create(FObject);
  //Status(R.ToString);
  T := R;
  ProcessRecord(R);
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

end;

procedure TestGenericTRecord.TestFromDataSet;
begin

end;

procedure TestGenericTRecord.TestFromStrings;
begin

end;

procedure TestGenericTRecord.TestIsBlank;
begin

end;

procedure TestGenericTRecord.TestIsEmpty;
begin

end;

procedure TestGenericTRecord.TestRetrieveRecord;
begin

end;

procedure TestGenericTRecord.TestRetrieveRecordFunction;
begin

end;

procedure TestGenericTRecord.TestToBoolean;
begin

end;

procedure TestGenericTRecord.TestToFloat;
begin

end;

procedure TestGenericTRecord.TestToInteger;
begin

end;

procedure TestGenericTRecord.TestToString;
begin

end;

procedure TestGenericTRecord.TestToStrings;
begin

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

  DCR := TRecord<TTestClass>.Create(FObject);
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

