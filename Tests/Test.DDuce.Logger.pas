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
  private


  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure Test_Send_method_for_Boolean_argument;
    procedure Test_Send_method_for_Integer_argument;
    procedure Test_Send_method_for_Cardinal_argument;
    procedure Test_Send_method_for_Int64_argument;
    procedure Test_Send_method_for_Single_argument;
    procedure Test_Send_method_for_Double_argument;
    procedure Test_Send_method_for_Variant_argument;

    procedure Test_Send_method_for_TDateTime_argument;
    procedure Test_Send_method_for_TDate_argument;
    procedure Test_Send_method_for_TTime_argument;

    procedure Test_Send_method_for_Extended_argument;
    procedure Test_Send_method_for_string_argument;

    procedure Test_SendDateTime_method;
    procedure Test_SendDate_method;
    procedure Test_SendTime_method;
    procedure Test_SendRect_method;

    procedure Test_Send_method;


  end;

implementation

uses
  System.TypInfo, System.Rtti, System.Types,

  DDuce.Reflect, Test.Utils;

{ TestLogger }

procedure TestLogger.SetUp;
begin
  inherited SetUp;

end;

procedure TestLogger.TearDown;
begin
  inherited TearDown;

end;

{$REGION 'Test Send method'}
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

procedure TestLogger.Test_SendRect_method;
var
  T : TRect;
begin
  T.Left := 4;
  T.Right := 5;
  Logger.SendRect(Reflect.TypeName(T), T);
end;

procedure TestLogger.Test_SendTime_method;
var
  T : TTime;
begin
  T := Now;
  Logger.SendTime(Reflect.TypeName(T), T);
end;
{$ENDREGION}

end.
