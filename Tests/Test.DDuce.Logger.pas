{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Test.DDuce.Logger;

{$I Test.DDuce.inc}

interface

uses
  System.SysUtils, System.Classes,

  DDuce.Logger,

  TestFramework;

type
  TestLogger = class(TTestCase)
  public
    class constructor Create;

    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure Test_Send_method_for_Boolean_argument;
    procedure Test_Send_method_for_Integer_argument;
    procedure Test_Send_method_for_Cardinal_argument;
    procedure Test_Send_method_for_Int64_argument;
    procedure Test_Send_method_for_Single_argument;
    procedure Test_Send_method_for_Double_argument;
    procedure Test_Send_method_for_Extended_argument;
    procedure Test_Send_method_for_Variant_argument;
    procedure Test_Send_method_for_TDate_argument;
    procedure Test_Send_method_for_TTime_argument;
    procedure Test_Send_method_for_TDateTime_argument;
    procedure Test_Send_method_for_string_argument;

    procedure Test_SendDateTime_method;
    procedure Test_SendDate_method;
    procedure Test_SendTime_method;
    procedure Test_SendRect_method;
    procedure Test_SendPoint_method;
    procedure Test_SendPointer_method;
    procedure Test_SendComponent_method;
    procedure Test_SendObject_method;
    procedure Test_SendStrings_method;
    procedure Test_SendMemory_method;

    procedure Test_Send_method;

    procedure Test_Watch_method_for_Boolean_argument;
    procedure Test_Watch_method_for_Integer_argument;
    procedure Test_Watch_method_for_Cardinal_argument;
    procedure Test_Watch_method_for_Int64_argument;
    procedure Test_Watch_method_for_Single_argument;
    procedure Test_Watch_method_for_Double_argument;
    procedure Test_Watch_method_for_Extended_argument;
    procedure Test_Watch_method_for_Variant_argument;
    procedure Test_Watch_method_for_TDateTime_argument;
    procedure Test_Watch_method_for_TDate_argument;
    procedure Test_Watch_method_for_TTime_argument;
    procedure Test_Watch_method_for_string_argument;

    procedure Test_Enter_and_Leave_methods_for_routine;
    procedure Test_Enter_and_Leave_methods_for_method;
    procedure Test_Track_method_for_method;
    procedure Test_Track_method_for_routine;

    procedure Test_SendInfo_method;
    procedure Test_SendWarning_method;
    procedure Test_SendError_method;

    procedure Test_Counter_methods;

    procedure Test_Checkpoint_methods;
  end;

implementation

uses
  System.TypInfo, System.Rtti, System.Types,
  Vcl.Forms,

  DDuce.Reflect, DDuce.Logger.Channels.WinIPC,

  Test.Utils;

{ TestLogger }

class constructor TestLogger.Create;
begin
  Logger.Channels.Add(TWinIPCChannel.Create);
end;

procedure TestLogger.SetUp;
begin
  inherited SetUp;

end;

procedure TestLogger.TearDown;
begin
  inherited TearDown;

end;

{$REGION 'Test Send methods'}
procedure TestLogger.Test_Send_method;
var
  T : TPoint;
begin
  T.X := 4;
  T.Y := 5;
  Logger.Send(Reflect.TypeName(T), TValue.From(T));
end;

procedure TestLogger.Test_Send_method_for_Boolean_argument;
var
  T : Boolean;
begin
  T := True;
  Logger.Send(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_Send_method_for_Cardinal_argument;
var
  T : Cardinal;
begin
  T := 525;
  Logger.Send(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_Send_method_for_Double_argument;
var
  T : Double;
begin
  T := Pi;
  Logger.Send(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_Send_method_for_Extended_argument;
var
  T : Extended;
begin
  T := Pi;
  Logger.Send(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_Send_method_for_Int64_argument;
var
  T : Int64;
begin
  T := 1234567899843211234;
  Logger.Send(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_Send_method_for_Integer_argument;
var
  T : Integer;
begin
  T := MaxInt;
  Logger.Send(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_Send_method_for_Single_argument;
var
  T : Single;
begin
  T := Pi;
  Logger.Send(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_Send_method_for_string_argument;
var
  T : string;
begin
  T := 'Test';
  Logger.Send(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_Send_method_for_TDateTime_argument;
var
  T : TDateTime;
begin
  T := Now;
  Logger.Send(Reflect.TypeName(T), TValue.From(T)); // no implicit cast for TDateTime
end;

procedure TestLogger.Test_Send_method_for_TDate_argument;
var
  T : TDate;
begin
  T := Now;
  Logger.Send(Reflect.TypeName(T), TValue.From(T)); // no implicit cast for TDate
end;

procedure TestLogger.Test_Send_method_for_TTime_argument;
var
  T : TTime;
begin
  T := Now;
  Logger.Send(Reflect.TypeName(T), TValue.From(T)); // no implicit cast for TTime
end;

procedure TestLogger.Test_Send_method_for_Variant_argument;
var
  T : Variant;
begin
  T := 'Test';
  Logger.Send(Reflect.TypeName(T), T);
  T := Pi;
  Logger.Send(Reflect.TypeName(T), T);
  T := False;
  Logger.Send(Reflect.TypeName(T), T);
  T := 520;
  Logger.Send(Reflect.TypeName(T), T);
end;
{$ENDREGION}

{$REGION 'Test custom Send methods'}
procedure TestLogger.Test_SendComponent_method;
begin
  Logger.SendComponent(Reflect.TypeName(Application), Application);
end;

procedure TestLogger.Test_SendDateTime_method;
var
  T : TDateTime;
begin
  T := Now;
  Logger.SendDateTime(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_SendDate_method;
var
  T : TDate;
begin
  T := Now;
  Logger.SendDate(Reflect.TypeName(T), T);
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
end;

procedure TestLogger.Test_SendObject_method;
begin
  Logger.SendObject(Reflect.TypeName(Self), Self);
end;

procedure TestLogger.Test_SendPointer_method;
var
  T : Pointer;
begin
  T := Application;
  Logger.SendPointer(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_SendPoint_method;
var
  P : TPoint;
begin
  P.X := 3;
  P.Y := 8;
  Logger.SendPoint(Reflect.TypeName(P), P);
end;

procedure TestLogger.Test_SendRect_method;
var
  T : TRect;
begin
  T.Left := 4;
  T.Right := 5;
  Logger.SendRect(Reflect.TypeName(T), T);
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
    Logger.SendStrings(Reflect.TypeName(SL), SL);
  finally
    SL.Free;
  end;
end;

procedure TestLogger.Test_SendTime_method;
var
  T : TTime;
begin
  T := Now;
  Logger.SendTime(Reflect.TypeName(T), T);
end;
{$ENDREGION}

{$REGION 'Watch'}
procedure TestLogger.Test_Watch_method_for_Boolean_argument;
var
  T : Boolean;
begin
  T := True;
  Logger.Watch(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_Watch_method_for_Cardinal_argument;
var
  T : Cardinal;
begin
  T := 525;
  Logger.Watch(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_Watch_method_for_Double_argument;
var
  T : Double;
begin
  T := Pi;
  Logger.Watch(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_Watch_method_for_Extended_argument;
var
  T : Extended;
begin
  T := Pi;
  Logger.Watch(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_Watch_method_for_Int64_argument;
var
  T : Int64;
begin
  T := 1234567899843211234;
  Logger.Watch(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_Watch_method_for_Integer_argument;
var
  T : Integer;
begin
  T := MaxInt;
  Logger.Watch(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_Watch_method_for_Single_argument;
var
  T : Single;
begin
  T := Pi;
  Logger.Watch(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_Watch_method_for_string_argument;
var
  T : string;
begin
  T := 'Test';
  Logger.Watch(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_Watch_method_for_TDateTime_argument;
var
  T : TDateTime;
begin
  T := Now;
  Logger.Watch(Reflect.TypeName(T), TValue.From(T)); // no implicit cast for TDateTime
end;

procedure TestLogger.Test_Watch_method_for_TDate_argument;
var
  T : TDate;
begin
  T := Now;
  Logger.Watch(Reflect.TypeName(T), TValue.From(T)); // no implicit cast for TDate
end;

procedure TestLogger.Test_Watch_method_for_TTime_argument;
var
  T : TTime;
begin
  T := Now;
  Logger.Watch(Reflect.TypeName(T), TValue.From(T)); // no implicit cast for TTime
end;

procedure TestLogger.Test_Watch_method_for_Variant_argument;
var
  T : Variant;
begin
// TODO: make overload for variant

  T := 'Test';
  Logger.Watch(Reflect.TypeName(T), TValue.FromVariant(T));
  T := Pi;
  Logger.Watch(Reflect.TypeName(T), TValue.FromVariant(T));
  T := False;
  Logger.Watch(Reflect.TypeName(T), TValue.FromVariant(T));
  T := 520;
  Logger.Watch(Reflect.TypeName(T), TValue.FromVariant(T));
end;
{$ENDREGION}

{$REGION 'Test methods for Enter and Leave'}
procedure TestLogger.Test_Enter_and_Leave_methods_for_method;
begin
  Logger.Enter(Self, 'Test_Enter_and_Leave_methods_for_method');
  Sleep(1100);
  Logger.Leave(Self, 'Test_Enter_and_Leave_methods_for_method');
end;

procedure TestLogger.Test_Enter_and_Leave_methods_for_routine;
begin
  Logger.Enter('Test_Enter_and_Leave_methods_for_routine');
  Sleep(1100);
  Logger.Leave('Test_Enter_and_Leave_methods_for_routine');
end;

procedure TestLogger.Test_Track_method_for_method;
begin
  Logger.Track(Self, 'Test_Track_methods_for_method');
  Logger.Track(Self, 'Level 1');
  Logger.Track(Self, 'Level 2');
  Logger.Track(Self, 'Level 3');
  Sleep(1100);
end;

procedure TestLogger.Test_Track_method_for_routine;
begin
  Logger.Track('Test_Track_methods_for_routine');
  Logger.Track('Level 1');
  Logger.Track('Level 2');
  Logger.Track('Level 3');
  Sleep(1100);
end;
{$ENDREGION}

{$REGION 'Send notification test methods'}
procedure TestLogger.Test_SendError_method;
begin
  Logger.SendError('Error');
end;

procedure TestLogger.Test_SendInfo_method;
begin
  Logger.SendInfo('Info');
end;

procedure TestLogger.Test_SendWarning_method;
begin
  Logger.SendWarning('Warning');
end;
{$ENDREGION}

{$REGION 'Counter test methods'}
procedure TestLogger.Test_Counter_methods;
var
  S : string;
  I: Integer;
begin
  S := 'TestCounter';
  Logger.IncCounter(S);
  Logger.Enter('Increasing counter');
  for I := 1 to 100 do
  begin
    Logger.IncCounter(S);
  end;
  Logger.Leave('Increasing counter');

  Logger.Enter('Decreasing counter');
  for I := 1 to 50 do
  begin
    Logger.DecCounter(S);
  end;
  Logger.Leave('Decreasing counter');
  Logger.ResetCounter(S);
  Logger.SendInfo('Last known value for %s is %d', [S, Logger.GetCounter(S)]);
end;
{$ENDREGION}

{$REGION 'Checkpoint test methods'}
procedure TestLogger.Test_Checkpoint_methods;
begin
  Logger.AddCheckPoint('TestCheckPoint 1');
  Sleep(200);
  Logger.AddCheckPoint('TestCheckPoint 2');
  Sleep(100);
  Logger.AddCheckPoint('TestCheckPoint 1');
  Sleep(800);
  Logger.AddCheckPoint('TestCheckPoint 1');
  Sleep(500);
  Logger.ResetCheckPoint('TestCheckPoint 1');
  Sleep(200);
  Logger.ResetCheckPoint('TestCheckPoint 2');
end;
{$ENDREGION}

end.
