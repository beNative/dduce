{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Test.DDuce.Mosquitto;

interface

uses
  Spring,

  DUnitX.TestFramework,

  Mosquitto;

type
  [TestFixture]
  TestMosquitto = class
  private
    FMosquitto : TMosquitto;

  protected
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_Connect_method;
  end;

implementation

procedure TestMosquitto.SetUp;
begin
  FMosquitto := TMosquitto.Create;
end;

procedure TestMosquitto.TearDown;
begin
  FMosquitto.Free;
end;

procedure TestMosquitto.Test_Connect_method;
begin
  //CheckTrue(FMosquitto.Connect('192.168.0.219'));
//  CheckTrue(FMosquitto.Connect('localhost'));
end;

end.
