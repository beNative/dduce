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

unit Test.DDuce.Logger;

interface

uses
  System.SysUtils, System.Classes,

  DDuce.Logger,

  DUnitX.TestFramework;

{ REMARK: these unit tests should be executed in combination with a LogViewer
  instance to test the display of the logged messages. }

type
  TestLogger = class
  protected
    // fails because no type information is generated for the legacy type Real48
    procedure Test_Send_method_for_Real48_argument;
    // not implemented yet
    procedure Test_SendException_method;
    procedure Test_SendMemory_method;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    [Test]
    procedure Test_Send_method_for_AnsiString_argument;
    [Test]
    procedure Test_Send_method_for_Boolean_argument;
    [Test]
    procedure Test_Send_method_for_Byte_argument;
    [Test]
    procedure Test_Send_method_for_Cardinal_argument;
    [Test]
    procedure Test_Send_method_for_Currency_argument;
    [Test]
    procedure Test_Send_method_for_Double_argument;
    [Test]
    procedure Test_Send_method_for_Enumeration_argument;
    [Test]
    procedure Test_Send_method_for_Extended_argument;
    [Test]
    procedure Test_Send_method_for_FixedInt_argument;
    [Test]
    procedure Test_Send_method_for_FixedUInt_argument;
    [Test]
    procedure Test_Send_method_for_Int16_argument;
    [Test]
    procedure Test_Send_method_for_Int32_argument;
    [Test]
    procedure Test_Send_method_for_Int64_argument;
    [Test]
    procedure Test_Send_method_for_Int8_argument;
    [Test]
    procedure Test_Send_method_for_Integer_argument;
    [Test]
    procedure Test_Send_method_for_LongInt_argument;
    [Test]
    procedure Test_Send_method_for_LongWord_argument;
    [Test]
    procedure Test_Send_method_for_NativeInt_argument;
    [Test]
    procedure Test_Send_method_for_NativeUInt_argument;
    [Test]
    procedure Test_Send_method_for_Set_argument;
    [Test]
    procedure Test_Send_method_for_ShortInt_argument;
    [Test]
    procedure Test_Send_method_for_ShortString_argument;
    [Test]
    procedure Test_Send_method_for_Single_argument;
    [Test]
    procedure Test_Send_method_for_SmallInt_argument;
    [Test]
    procedure Test_Send_method_for_string_argument;
    [Test]
    procedure Test_Send_method_for_TDateTime_argument;
    [Test]
    procedure Test_Send_method_for_TDate_argument;
    [Test]
    procedure Test_Send_method_for_TTime_argument;
    [Test]
    procedure Test_Send_method_for_UInt16_argument;
    [Test]
    procedure Test_Send_method_for_UInt32_argument;
    [Test]
    procedure Test_Send_method_for_UInt64_argument;
    [Test]
    procedure Test_Send_method_for_UInt8_argument;
    [Test]
    procedure Test_Send_method_for_WideString_argument;
    [Test]
    procedure Test_Send_method_for_Word_argument;

    [Test]
    procedure Test_Send_method_for_static_array_of_string_argument;
    [Test]
    procedure Test_Send_method_for_static_array_of_Integer_argument;
    [Test]
    procedure Test_Send_method_for_dynamic_array_of_Integer_argument;
    [Test]
    procedure Test_Send_method_for_array_of_const_argument;

    [Test]
    procedure Test_Send_method_for_Record_argument;
    [Test]
    procedure Test_Send_method_for_Object_argument;
    [Test]
    procedure Test_Send_method_for_Interface_argument;

    [Test]
    procedure Test_SendDateTime_method;
    [Test]
    procedure Test_SendDate_method;
    [Test]
    procedure Test_SendTime_method;
    [Test]
    procedure Test_SendRect_method;
    [Test]
    procedure Test_SendPoint_method;
    [Test]
    procedure Test_SendPointer_method;
    [Test]
    procedure Test_SendComponent_method;
    [Test]
    procedure Test_SendObject_method;
    [Test]
    procedure Test_SendStrings_method;
    [Test]
    procedure Test_SendColor_method;
    [Test]
    procedure Test_SendAlphaColor_method;
    [Test]
    procedure Test_SendInterface_method;
    [Test]
    procedure Test_SendShortCut_method;
    [Test]
    procedure Test_SendBitmap_method;
    [Test]
    procedure Test_SendPersistent_method;
    [Test]
    procedure Test_SendDataSet_method;

    [Test]
    procedure Test_SendSQL_method;
    [Test]
    procedure Test_SendXML_method;
    [Test]
    procedure Test_SendINI_method;
    [Test]
    procedure Test_SendJSON_method;

    [Test]
    procedure Test_SendIf_method;
    [Test]
    procedure Test_SendText_method;

    [Test]
    procedure Test_SendVariant_method_for_TDateTime_argument;
    [Test]
    procedure Test_SendVariant_method_for_string_argument;
    [Test]
    procedure Test_SendVariant_method_for_Integer_argument;
    [Test]
    procedure Test_SendVariant_method_for_Double_argument;
    [Test]
    procedure Test_SendVariant_method_for_Null_argument;
    [Test]
    procedure Test_SendVariant_method_for_Unassigned_argument;
    [Test]
    procedure Test_SendVariant_method_for_EmptyParam_argument;

    [Test]
    procedure Test_Send_method;

    [Test]
    procedure Test_Watch_method_for_AnsiString_argument;
    [Test]
    procedure Test_Watch_method_for_Boolean_argument;
    [Test]
    procedure Test_Watch_method_for_Cardinal_argument;
    [Test]
    procedure Test_Watch_method_for_Double_argument;
    [Test]
    procedure Test_Watch_method_for_Enumeration_argument;
    [Test]
    procedure Test_Watch_method_for_Extended_argument;
    [Test]
    procedure Test_Watch_method_for_Int64_argument;
    [Test]
    procedure Test_Watch_method_for_Integer_argument;
    [Test]
    procedure Test_Watch_method_for_Set_argument;
    [Test]
    procedure Test_Watch_method_for_ShortString_argument;
    [Test]
    procedure Test_Watch_method_for_Single_argument;
    [Test]
    procedure Test_Watch_method_for_string_argument;
    [Test]
    procedure Test_Watch_method_for_TDateTime_argument;
    [Test]
    procedure Test_Watch_method_for_TDate_argument;
    [Test]
    procedure Test_Watch_method_for_TTime_argument;
    [Test]
    procedure Test_Watch_method_for_Variant_argument;
    [Test]
    procedure Test_Watch_method_for_WideString_argument;

    [Test]
    procedure Test_Watch_method_with_empty_name_argument; // should fail

    [Test]
    { For a watch the name/type combination is used to identify the watch. }
    procedure Test_Watch_methods_with_same_name_and_different_type;

    [Test]
    procedure Test_Enter_and_Leave_methods_for_routine;
    [Test]
    procedure Test_Enter_and_Leave_methods_for_method;
    [Test]
    procedure Test_Track_method_for_method;
    [Test]
    procedure Test_Track_method_for_routine;

    [Test]
    procedure Test_Info_method;
    [Test]
    procedure Test_Warn_method;
    [Test]
    procedure Test_Error_method;

    [Test]
    procedure Test_Counter_methods;

    [Test]
    procedure Test_Checkpoint_methods;

    [Test]
    procedure Test_Method_Chaining;

    [Test]
    procedure Test_OutputDebugString_method;
    [Test]
    procedure Test_OutputDebugString_method_for_long_string;
    [Test]
    procedure Test_OutputDebugString_method_for_valuelist;

  end;

implementation

uses
  System.TypInfo, System.Rtti, System.Types, System.UITypes, System.UIConsts,
  System.Variants,
  Vcl.Forms, Vcl.Graphics, Vcl.Menus, Vcl.Dialogs,
  Data.DB,

  Spring,

  DDuce.Reflect, DDuce.Utils,
  DDuce.Logger.Channels.WinIPC, DDuce.Logger.Channels.ZeroMQ,

  Test.Utils, Test.Resources;

{$REGION 'construction and destruction'}
procedure TestLogger.AfterConstruction;
begin
  inherited AfterConstruction;
  Logger.Channels.Add(TWinIPCChannel.Create);
  //Logger.Channels.Add(TZeroMQChannel.Create('tcp://*:5555'));
  Sleep(1000);
  Logger.Clear;
  Logger.Clear;
  Logger.Clear;
end;
{$ENDREGION}

{$REGION 'Test Send methods'}
procedure TestLogger.Test_Send_method;
var
  T : TPoint;
begin
  T.X := 4;
  T.Y := 5;
  Logger.Send('TestTPoint', TValue.From(T));
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_AnsiString_argument;
var
  T : AnsiString;
begin
  T := 'Test';
  Logger.Send('TestAnsiString', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_array_of_const_argument;
var
  T : array[0..3] of TVarRec;
  S : string;
begin
  S := 'MyString';
  T[0].VType := vtUnicodeString;
  T[0].VUnicodeString := Pointer(S);
  T[1].VType := vtBoolean;
  T[1].VBoolean := False;
  T[2].VType := vtInteger;
  T[2].VInteger := 4;
  Logger.Send('TestArrayOfConst', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_Boolean_argument;
var
  T : Boolean;
begin
  T := True;
  Logger.Send('TestBoolean', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_Byte_argument;
var
  T : Byte;
begin
  T := 128;
  Logger.Send('TestByte', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_Cardinal_argument;
var
  T : Cardinal;
begin
  T := 8888888;
  Logger.Send('TestCardinal', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_Currency_argument;
var
  T : Currency;
begin
  T := Pi;
  Logger.Send('TestCurrency', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_Double_argument;
var
  T : Double;
begin
  T := Pi;
  Logger.Send('TestDouble', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_dynamic_array_of_Integer_argument;
var
  T : TArray<Integer>;
begin
  T := [1, 2 , 3, 4, 5];
  Logger.Send('TestTArrayOfInteger', TValue.From(T));
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_Enumeration_argument;
var
  T : TAlignment;
begin
  T := taCenter;
  Logger.Send('TestEnum', TValue.From(T));
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_Extended_argument;
var
  T : Extended;
begin
  T := Pi;
  Logger.Send('TestExtended', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_FixedInt_argument;
var
  T : FixedInt;
begin
  T := 12345;
  Logger.Send('TestFixedInt', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_FixedUInt_argument;
var
  T : FixedUInt;
begin
  T := 12345;
  Logger.Send('TestFixedUInt', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_Int16_argument;
var
  T : Int16;
begin
  T := 12345;
  Logger.Send('TestInt16', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_Int32_argument;
var
  T : Int32;
begin
  T := 12345;
  Logger.Send('TestInt32', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_Int64_argument;
var
  T : Int64;
begin
  T := 1234567899843211234;
  Logger.Send('TestInt64', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_Int8_argument;
var
  T : Int8;
begin
  T := 123;
  Logger.Send('TestInt8', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_Integer_argument;
var
  T : Integer;
begin
  T := MaxInt;
  Logger.Send('TestInteger', T);
  Assert.Pass;
end;

{ REMARK: LongInt is platform-dependent!
  LongInt = Integer    for 32-bit platforms and 64-bit Windows platforms
  LongInt = Int64      for 64-bit iOS platforms }

procedure TestLogger.Test_Send_method_for_LongInt_argument;
var
  T : LongInt;
begin
  T := -489;
  Logger.Send('TestLongInt', T);
  Assert.Pass;
end;

{ REMARK: LongWord is platform-dependent!
  LongWord = Cardinal    for 32-bit platforms and 64-bit Windows platforms
  LongWord = UInt64      for 64-bit iOS platforms }

procedure TestLogger.Test_Send_method_for_LongWord_argument;
var
  T : LongWord;
begin
  T := 564564123;
  Logger.Send('TestLongWord', T);
  Assert.Pass;
end;

{ REMARK: NativeInt is platform-dependent!
  NativeInt = Integer    for 32-bit platforms
  NativeInt = Int64      for 64-bit platforms }

procedure TestLogger.Test_Send_method_for_NativeInt_argument;
var
  T : NativeInt;
begin
  T := 312;
  Logger.Send('TestNativeInt', T);
  Assert.Pass;
end;

{ REMARK: NativeUInt is platform-dependent!
  NativeUInt = Cardinal    for 32-bit platforms
  NativeUInt = UInt64      for 64-bit platforms }

procedure TestLogger.Test_Send_method_for_NativeUInt_argument;
var
  T : NativeUInt;
begin
  T := 428;
  Logger.Send('TestNativeUInt', T);
  Assert.Pass;
end;

{ This will send the object reference information, not the content. }

procedure TestLogger.Test_Send_method_for_Object_argument;
var
  T : TObject;
begin
  T := TTestUtils.CreateTestObject;
  try
    Logger.Send('TestObject', T);
  finally
    T.Free;
  end;
  Assert.Pass;
end;

{ This will send the reference information of the interface variable, not the
  content. }

procedure TestLogger.Test_Send_method_for_Interface_argument;
var
  T : IInterface;
begin
  T := TInterfacedObject.Create;
  Logger.Send('TestInterface', TValue.From(T));
  Assert.Pass;
end;

{ This test fails because no RTTI is generated for the legacy Real48 type. }

procedure TestLogger.Test_Send_method_for_Real48_argument;
var
  T : Real48;
  V : TValue;
begin
  T := Pi;
  V := TValue.From(T);
  Logger.Send('TestReal48', V);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_Record_argument;
begin
  Logger.Send('TestRecord', TValue.From(TTestUtils.CreateTestRecord));
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_Set_argument;
var
  T : TBorderIcons;
begin
  T := [biSystemMenu, biMinimize, biMaximize, biHelp];
  Logger.Send('TestSet', TValue.From(T));
  Assert.Pass;
end;

{ ShortStrings use single byte chars. }

procedure TestLogger.Test_Send_method_for_ShortInt_argument;
var
  T : ShortInt;
begin
  T := -123;
  Logger.Send('TestShortInt', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_ShortString_argument;
var
  T : ShortString;
begin
  T := 'Test ShortString';
  Logger.Send('TestShortString', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_Single_argument;
var
  T : Single;
begin
  T := Pi;
  Logger.Send('TestSingle', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_SmallInt_argument;
var
  T : SmallInt;
begin
  T := -10256;
  Logger.Send('TestSmallInt', T);
  Assert.Pass;
end;

{ REMARK: no RTTI is generated by the compiler if no type declaration is
  present for static arrays }

procedure TestLogger.Test_Send_method_for_static_array_of_Integer_argument;
type
  TMyStaticArrayOfInteger = array[0..2] of Integer;
var
  T : TMyStaticArrayOfInteger;
begin
  T[0] := 1;
  T[1] := 2;
  T[2] := 3;
  Logger.Send('TestStaticArrayOfInteger', TValue.From(T));
  Assert.Pass;
end;

{ REMARK: no RTTI is generated by the compiler if no type declaration is
  present for static arrays }

procedure TestLogger.Test_Send_method_for_static_array_of_string_argument;
type
  TMyStaticArrayOfString = array[0..2] of string;
var
  T : TMyStaticArrayOfString;
begin
  T[0] := 'one';
  T[1] := 'two';
  T[2] := 'three';
  Logger.Send('TestStaticArrayOfString', TValue.From(T));
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_string_argument;
var
  T : string;
begin
  T := 'Test';
  Logger.Send('TestString', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_TDateTime_argument;
var
  T : TDateTime;
begin
  T := Now;
  Logger.Send('TestTDateTime', TValue.From(T)); // no implicit cast for TDateTime
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_TDate_argument;
var
  T : TDate;
begin
  T := Now;
  Logger.Send('TestTDate', TValue.From(T)); // no implicit cast for TDate
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_TTime_argument;
var
  T : TTime;
begin
  T := Now;
  Logger.Send('TestTTime', TValue.From(T)); // no implicit cast for TTime
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_UInt16_argument;
var
  T : UInt16;
begin
  T := 12345;
  Logger.Send('TestUInt16', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_UInt32_argument;
var
  T : UInt32;
begin
  T := 12345;
  Logger.Send('TestUInt32', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_UInt64_argument;
var
  T : UInt64;
begin
  T := 1234567890;
  Logger.Send('TestUInt64', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_UInt8_argument;
var
  T : UInt8;
begin
  T := 123;
  Logger.Send('TestUInt8', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_WideString_argument;
var
  T : WideString;
begin
  T := 'Test';
  Logger.Send('TestWideString', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Send_method_for_Word_argument;
var
  T : Word;
begin
  T := 32000;
  Logger.Send('TestWord', T);
  Assert.Pass;
end;
{$ENDREGION}

{$REGION 'Test custom Send methods'}
procedure TestLogger.Test_SendAlphaColor_method;
var
  T : TAlphaColor;
begin
  T := claAqua;
  Logger.SendAlphaColor('TestTAlphaColor', T);
  T := claAntiquewhite;
  Logger.SendAlphaColor('TestTAlphaColor', T);
  Assert.Pass;
end;

{ Makes a bitmap with a red cross (100 x 100 pixels) }

procedure TestLogger.Test_SendBitmap_method;
var
  T : TBitmap;
begin
  T := TBitmap.Create;
  try
    T.SetSize(100,100);
    T.Canvas.Pen.Color := clRed;
    T.Canvas.Pen.Width := 2;
    T.Canvas.MoveTo(0, 0);
    T.Canvas.LineTo(100, 100);
    T.Canvas.MoveTo(100, 0);
    T.Canvas.LineTo(0, 100);
    Logger.SendBitmap('TestTBitmap', T);
  finally
    T.Free;
  end;
  Assert.Pass;
end;

procedure TestLogger.Test_SendColor_method;
var
  T : TColor;
begin
  T := clYellow;
  Logger.SendColor('TestTColor', T);
  T := $00C08080;
  Logger.SendColor('TestTColor', T);
  Assert.Pass;
end;

procedure TestLogger.Test_SendComponent_method;
var
  T : TComponent;
begin
  T := Application;
  Logger.SendComponent('TestTComponent', T);
  Assert.Pass;
end;

procedure TestLogger.Test_SendDataSet_method;
var
  DS : TDataSet;
begin
  DS := TTestUtils.CreateDataSet(100);
  try
    Logger.SendDataSet('TestDataSet', DS);
  finally
    DS.Free;
  end;
  Assert.Pass;
end;

procedure TestLogger.Test_SendDateTime_method;
var
  T : TDateTime;
begin
  T := Now;
  Logger.SendDateTime('TestTDateTime', T);
  Assert.Pass;
end;

procedure TestLogger.Test_SendDate_method;
var
  T : TDate;
begin
  T := Now;
  Logger.SendDate('TestTDate', T);
  Assert.Pass;
end;

procedure TestLogger.Test_SendException_method;
var
  T : Exception;
begin
  T := Exception.Create('Test exception');
  try
    Logger.SendException('TestException', T);
  finally
    T.Free;
  end;
  Assert.Pass;
end;

procedure TestLogger.Test_SendIf_method;
begin
  Logger.SendIf('Following condition was True (1 = 1)', 1 = 1);
  Logger.SendIf('Following condition was False (1 = 0)', 1 = 0, False);
  Logger.SendIf('Following condition was True (1 = 0)', 1 = 0);
  Assert.Pass;
end;

procedure TestLogger.Test_SendINI_method;
begin
  Logger.SendINI('INI example', TEXT_INI);
  Assert.Pass;
end;

procedure TestLogger.Test_SendInterface_method;
var
  T : IInterface;
begin
  T := TInterfacedObject.Create;
  Logger.SendInterface('TestIInterface', T);
  Assert.Pass;
end;

procedure TestLogger.Test_SendJSON_method;
begin
  Logger.SendJSON('JSON example', TEXT_JSON);
  Assert.Pass;
end;

procedure TestLogger.Test_SendMemory_method;
var
  M : Pointer;
  S : Integer;
begin
// TODO: not working yet
  M := Self;
  S := Self.InstanceSize;
  Logger.SendMemory('Self', M, S);
  Assert.Pass;
end;

procedure TestLogger.Test_SendObject_method;
var
  O : TObject;
begin
  O := TTestUtils.CreateTestObject;
  try
    Logger.SendObject('TestTObject', O);
  finally
    O.Free;
  end;
  Assert.Pass;
end;

procedure TestLogger.Test_SendPersistent_method;
var
  F : TFont;
begin
  F := TFont.Create;
  try
    Logger.SendPersistent('Font', F);
  finally
    F.Free;
  end;
  Assert.Pass;
end;

procedure TestLogger.Test_SendPointer_method;
var
  T : Pointer;
begin
  T := Application;
  Logger.SendPointer('TestPointer', T);
  Assert.Pass;
end;

procedure TestLogger.Test_SendPoint_method;
var
  T : TPoint;
begin
  T.X := 3;
  T.Y := 8;
  Logger.SendPoint('TestTPoint', T);
  Assert.Pass;
end;

procedure TestLogger.Test_SendRect_method;
var
  T : TRect;
begin
  T.Left   := 4;
  T.Right  := 5;
  T.Top    := 8;
  T.Bottom := 10;
  Logger.SendRect('TestTRect', T);
  Assert.Pass;
end;

procedure TestLogger.Test_SendShortCut_method;
var
  T : TShortCut;
begin
  T := TextToShortCut('CTRL+S');
  Logger.SendShortCut('TestTShortCut', T);
  Assert.Pass;
end;

procedure TestLogger.Test_SendStrings_method;
var
  SL : TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('One');
    SL.Add('Two');
    SL.Add('Three');
    SL.Add('Four');
    Logger.SendStrings('TestTStringList', SL);
  finally
    SL.Free;
  end;
  Assert.Pass;
end;

procedure TestLogger.Test_SendText_method;
begin
  Logger.SendText(TEXT_LOREM_IPSUM);
  Assert.Pass;
end;

procedure TestLogger.Test_SendTime_method;
var
  T : TTime;
begin
  T := Now;
  Logger.SendTime('TestTTime', T);
  Assert.Pass;
end;

procedure TestLogger.Test_SendVariant_method_for_Double_argument;
var
  T : Variant;
begin
  T := Pi;
  Logger.SendVariant('TestVariantDouble', T);
  Assert.Pass;
end;

procedure TestLogger.Test_SendVariant_method_for_EmptyParam_argument;
var
  T : Variant;
begin
  T := EmptyParam;
  Logger.SendVariant('TestVariantEmptyParam', T);
  Assert.Pass;
end;

procedure TestLogger.Test_SendVariant_method_for_Integer_argument;
var
  T : Variant;
begin
  T := 25;
  Logger.SendVariant('TestVariantInteger', T);
  Assert.Pass;
end;

procedure TestLogger.Test_SendVariant_method_for_Null_argument;
var
  T : Variant;
begin
  T := Null;
  Logger.SendVariant('TestVariantNull', T);
  Assert.Pass;
end;

procedure TestLogger.Test_SendVariant_method_for_string_argument;
var
  T : Variant;
begin
  T := 'Test';
  Logger.SendVariant('TestVariantString', T);
  Assert.Pass;
end;

procedure TestLogger.Test_SendVariant_method_for_TDateTime_argument;
var
  T : Variant;
begin
  T := Now;
  Logger.SendVariant('TestVariantTDateTime', T);
  Assert.Pass;
end;

procedure TestLogger.Test_SendVariant_method_for_Unassigned_argument;
var
  T : Variant;
begin
  T := Unassigned;
  Logger.SendVariant('TestVariantUnassigned', T);
  Assert.Pass;
end;

procedure TestLogger.Test_SendSQL_method;
begin
  Logger.SendSQL('SQL example query', TEXT_SQL);
  Assert.Pass;
end;

procedure TestLogger.Test_SendXML_method;
begin
  Logger.SendXML('XML example document', TEXT_XML);
  Assert.Pass;
end;
{$ENDREGION}

{$REGION 'Watch'}
procedure TestLogger.Test_Watch_method_for_AnsiString_argument;
var
  T : AnsiString;
begin
  T := 'Test';
  Logger.Watch('TestAnsiString', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_method_for_Boolean_argument;
var
  T : Boolean;
begin
  T := True;
  Logger.Watch('TestBoolean', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_method_for_Cardinal_argument;
var
  T : Cardinal;
begin
  T := 525;
  Logger.Watch('TestCardinal', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_method_for_Double_argument;
var
  T : Double;
begin
  T := Pi;
  Logger.Watch('TestDouble', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_method_for_Enumeration_argument;
var
  T : TFormStyle;
begin
  T := fsStayOnTop;
  Logger.Watch('TestEnum', TValue.From(T));
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_method_for_Extended_argument;
var
  T : Extended;
begin
  T := Pi;
  Logger.Watch('TestExtended', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_method_for_Int64_argument;
var
  T : Int64;
begin
  T := 1234567899843211234;
  Logger.Watch('TestInt64', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_method_for_Integer_argument;
var
  T : Integer;
begin
  T := MaxInt;
  Logger.Watch('TestInteger', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_method_for_Set_argument;
var
  T : TBorderIcons;
begin
  T := [biSystemMenu, biMinimize, biMaximize, biHelp];
  Logger.Watch('TestSet', TValue.From(T));
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_method_for_ShortString_argument;
var
  T : ShortString;
begin
  T := 'Succes!';
  Logger.Watch('TestShortString', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_method_for_Single_argument;
var
  T : Single;
begin
  T := Pi;
  Logger.Watch('TestSingle', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_method_for_string_argument;
var
  T : string;
begin
  T := 'Hello!';
  Logger.Watch('TestString', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_method_for_TDateTime_argument;
var
  T : TDateTime;
begin
  T := Now;
  Logger.Watch('TestTDateTime', TValue.From(T)); // no implicit cast for TDateTime
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_method_for_TDate_argument;
var
  T : TDate;
begin
  T := Now;
  Logger.Watch('TestTDate', TValue.From(T)); // no implicit cast for TDate
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_method_for_TTime_argument;
var
  T : TTime;
begin
  T := Now;
  Logger.Watch('TestTTime', TValue.From(T)); // no implicit cast for TTime
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_method_for_Variant_argument;
var
  T : Variant;
begin
// TODO: make overload for variant
  T := 'Test';
  Logger.Watch(Reflect.TypeName(T), T);
  T := Pi;
  Logger.Watch(Reflect.TypeName(T), T);
  T := False;
  Logger.Watch(Reflect.TypeName(T), T);
  T := 520;
  Logger.Watch(Reflect.TypeName(T), T);
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_method_for_WideString_argument;
var
  T : WideString;
begin
  T := 'Hi there!';
  Logger.Watch('TestWideString', T);
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_method_with_empty_name_argument;
begin
  Logger.Watch('', Self);
  Logger.Watch('', 5);
  Assert.Pass;
end;

procedure TestLogger.Test_Watch_methods_with_same_name_and_different_type;
begin
  Logger.Watch('ChangingType', 'String');
  Logger.Watch('ChangingType', 5);
  Logger.Watch('ChangingType', Self);
  Assert.Pass;
end;
{$ENDREGION}

{$REGION 'Test methods for Enter and Leave'}
procedure TestLogger.Test_Enter_and_Leave_methods_for_method;
begin
  Logger.Enter(Self, 'Test_Enter_and_Leave_methods_for_method');
  Sleep(100);
  Logger.Leave(Self, 'Test_Enter_and_Leave_methods_for_method');
  Assert.Pass;
end;

procedure TestLogger.Test_Enter_and_Leave_methods_for_routine;
begin
  Logger.Enter('Test_Enter_and_Leave_methods_for_routine');
  Sleep(100);
  Logger.Leave('Test_Enter_and_Leave_methods_for_routine');
  Assert.Pass;
end;

procedure TestLogger.Test_Track_method_for_method;
begin
  Logger.Track(Self, 'Test_Track_methods_for_method');
  Logger.Track(Self, 'Level 1');
  Logger.Track(Self, 'Level 2');
  Logger.Track(Self, 'Level 3');
  Sleep(100);
  Assert.Pass;
end;

procedure TestLogger.Test_Track_method_for_routine;
begin
  Logger.Track('Test_Track_methods_for_routine');
  Logger.Track('Level 1');
  Logger.Track('Level 2');
  Logger.Track('Level 3');
  Sleep(53);
  Assert.Pass;
end;
{$ENDREGION}

{$REGION 'Send notification test methods'}
procedure TestLogger.Test_Error_method;
begin
  Logger.Error('Error');
  Assert.Pass;
end;

procedure TestLogger.Test_Info_method;
begin
  Logger.Info('Info');
  Assert.Pass;
end;

procedure TestLogger.Test_Method_Chaining;
begin
  Logger.Track(Self, 'Test_Method_Chaining');
  Logger.SendDateTime('Now', Now)
        .Send('TestInteger', 2)
        .Info('Info message')
        .Warn('Warning message');
  Assert.Pass;
end;

procedure TestLogger.Test_Warn_method;
begin
  Logger.Warn('Warning');
  Assert.Pass;
end;
{$ENDREGION}

{$REGION 'Counter test methods'}
procedure TestLogger.Test_Counter_methods;
var
  S : string;
  I : Integer;
begin
  S := 'TestCounter';
  Logger.IncCounter(S);
  Assert.IsTrue(Logger.GetCounter(S) = 1, Logger.GetCounter('TestCounter').ToString);
  Logger.Enter('Increasing counter');
  for I := 1 to 100 do
  begin
    Logger.IncCounter(S);
  end;
  Logger.Leave('Increasing counter');
  Assert.IsTrue(Logger.GetCounter(S) = 101, Logger.GetCounter('TestCounter').ToString);
  Logger.Enter('Decreasing counter');
  for I := 1 to 50 do
  begin
    Logger.DecCounter(S);
  end;
  Logger.Leave('Decreasing counter');
  Logger.ResetCounter(S);
  Logger.Info('Last known value for %s is %d', [S, Logger.GetCounter(S)]);
  Assert.Pass;
end;
{$ENDREGION}

{$REGION 'Checkpoint test methods'}
destructor TestLogger.Destroy;
begin
  Logger.Channels.First.Disconnect;
  inherited Destroy;
end;

procedure TestLogger.Test_Checkpoint_methods;
begin
  Logger.AddCheckPoint('TestCheckPoint 1');
  Sleep(42);
  Logger.AddCheckPoint('TestCheckPoint 2');
  Sleep(80);
  Logger.AddCheckPoint('TestCheckPoint 1');
  Sleep(45);
  Logger.AddCheckPoint('TestCheckPoint 1');
  Sleep(36);
  Logger.ResetCheckPoint('TestCheckPoint 1');
  Sleep(22);
  Logger.ResetCheckPoint('TestCheckPoint 2');
  Assert.Pass;
end;
{$ENDREGION}

{$REGION 'OutputDebugString'}
procedure TestLogger.Test_OutputDebugString_method;
begin
  OutputDebugString('Short OutputDebugString');
  Assert.Pass;
end;

procedure TestLogger.Test_OutputDebugString_method_for_long_string;
begin
  OutputDebugString(TEXT_LOREM_IPSUM);
  Assert.Pass;
end;

procedure TestLogger.Test_OutputDebugString_method_for_valuelist;
begin
  Assert.Pass;
end;
{$ENDREGION}

end.

