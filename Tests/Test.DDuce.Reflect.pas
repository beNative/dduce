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

unit Test.DDuce.Reflect;

{$I Test.DDuce.inc}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Variants,

  DDuce.Reflect,

  TestFramework, // DUnit

  Test.Data;

type
  TestReflect = class(TTestCase)
  private
    FTestObject : TTestClass;
    FTestRecord : TTestRecord;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published

    procedure TestSetPropertyOfObject;


  end;


implementation

uses
  System.Rtti,

  Test.Utils,

  DDuce.Logger;

{$REGION 'SetUp and TearDown methods'}
procedure TestReflect.SetUp;
begin
  inherited SetUp;
  FTestObject := TTestUtils.CreateTestObject;
  FTestRecord := TTestUtils.CreateTestRecord;
//
end;

procedure TestReflect.TearDown;
begin
  inherited TearDown;
  FTestObject.Free;
//
end;
{$ENDREGION}

procedure TestReflect.TestSetPropertyOfObject;

begin
  Reflect.Fields(FTestObject).Data.TestInteger := 0;
  CheckEquals(0, FTestObject.TestInteger);
end;

end.
