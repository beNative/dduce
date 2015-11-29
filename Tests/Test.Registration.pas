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

unit Test.Registration;

{$I Test.DDuce.inc}

interface

procedure RegisterTests;

implementation

uses
  TestFramework,

  Test.DDuce.DynamicRecord, Test.DDuce.DynamicRecord.Generic,
  Test.DDuce.Reflect, Test.DDuce.Logger;

procedure RegisterTests;
begin
  RegisterTest('TRecord', TestTRecord.Suite);
  RegisterTest('TRecord<T>', TestGenericTRecord.Suite);
  RegisterTest('Reflect', TestReflect.Suite);
  RegisterTest('Logger', TestLogger.Suite);
end;

end.
