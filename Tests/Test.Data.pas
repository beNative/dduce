{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Test.Data;

{ Data types used for unit tests. }

{$I Test.DDuce.inc}

interface

uses
  System.SysUtils,
  Spring;

type
  TTestClass = class(TObject)
  private
    FTestBoolean          : Boolean;
    FTestChar             : Char;
    FTestDateTime         : TDateTime;
    FTestDouble           : Double;
    FTestInteger          : Integer;
    FTestString           : string;
{$IFDEF NULLABLE}
    FTestNullableBoolean  : Nullable<Boolean>;
    FTestNullableDateTime : Nullable<TDateTime>;
    FTestNullableDouble   : Nullable<Double>;
    FTestNullableInteger  : Nullable<Integer>;
    FTestNullableString   : Nullable<string>;
    FTestNullableChar     : Nullable<Char>;
{$ENDIF}

  public
    function Equals(Obj: TObject): Boolean; override;

    property TestBoolean: Boolean
      read FTestBoolean write FTestBoolean;

    property TestChar: Char
      read FTestChar write FTestChar;

    property TestDateTime: TDateTime
      read FTestDateTime write FTestDateTime;

    property TestDouble: Double
      read FTestDouble write FTestDouble;

    property TestInteger: Integer
      read FTestInteger write FTestInteger;

    property TestString: string
      read FTestString write FTestString;

{$IFDEF NULLABLE}
    property TestNullableBoolean: Nullable<Boolean>
      read FTestNullableBoolean write FTestNullableBoolean;

    property TestNullableDateTime: Nullable<TDateTime>
      read FTestNullableDateTime write FTestNullableDateTime;

    property TestNullableDouble: Nullable<Double>
      read FTestNullableDouble write FTestNullableDouble;

    property TestNullableInteger: Nullable<Integer>
      read FTestNullableInteger write FTestNullableInteger;

    property TestNullableString: Nullable<string>
      read FTestNullableString write FTestNullableString;

    property TestNullableChar: Nullable<Char>
      read FTestNullableChar write FTestNullableChar;
{$ENDIF}
  end;

type
  TTestRecord = record
  public
    FTestBoolean          : Boolean;
    FTestChar             : Char;
    FTestDateTime         : TDateTime;
    FTestDouble           : Double;
    FTestInteger          : Integer;
    FTestString           : string;
{$IFDEF NULLABLE}
    FTestNullableBoolean  : Nullable<Boolean>;
    FTestNullableDateTime : Nullable<TDateTime>;
    FTestNullableDouble   : Nullable<Double>;
    FTestNullableInteger  : Nullable<Integer>;
    FTestNullableString   : Nullable<string>;
    FTestNullableChar     : Nullable<Char>;
{$ENDIF}

  public
    function Equals(var ARecord: TTestRecord): Boolean;

    property TestBoolean: Boolean
      read FTestBoolean write FTestBoolean;

    property TestChar: Char
      read FTestChar write FTestChar;

    property TestDateTime: TDateTime
      read FTestDateTime write FTestDateTime;

    property TestDouble: Double
      read FTestDouble write FTestDouble;

    property TestInteger: Integer
      read FTestInteger write FTestInteger;

    property TestString: string
      read FTestString write FTestString;

{$IFDEF NULLABLE}
    property TestNullableBoolean: Nullable<Boolean>
      read FTestNullableBoolean write FTestNullableBoolean;

    property TestNullableDateTime: Nullable<TDateTime>
      read FTestNullableDateTime write FTestNullableDateTime;

    property TestNullableDouble: Nullable<Double>
      read FTestNullableDouble write FTestNullableDouble;

    property TestNullableInteger: Nullable<Integer>
      read FTestNullableInteger write FTestNullableInteger;

    property TestNullableString: Nullable<string>
      read FTestNullableString write FTestNullableString;

    property TestNullableChar: Nullable<Char>
      read FTestNullableChar write FTestNullableChar;
{$ENDIF}
  end;

implementation

uses
  System.Math;

{$REGION 'TTestClass'}
function TTestClass.Equals(Obj: TObject): Boolean;
var
  O : TTestClass;
begin
  if Obj is TTestClass then
  begin
    O := Obj as TTestClass;
    Result := O.TestString.Equals(TestString)
      and (O.TestBoolean = TestBoolean)
      and (SameValue(O.TestDouble, TestDouble))
      and (O.TestInteger = TestInteger)
      and (O.TestChar = TestChar)
      and (SameValue(O.TestDateTime, TestDateTime))
  end
  else
  begin
    Result := False;
  end;
end;
{$ENDREGION}

{$REGION 'TTestRecord'}
function TTestRecord.Equals(var ARecord: TTestRecord): Boolean;
begin
  Result := ARecord.TestString.Equals(TestString)
    and (ARecord.TestBoolean = TestBoolean)
    and (SameValue(ARecord.TestDouble, TestDouble))
    and (ARecord.TestInteger = TestInteger)
    and (ARecord.TestChar = TestChar)
    and (SameValue(ARecord.TestDateTime, TestDateTime));
end;
{$ENDREGION}

end.
