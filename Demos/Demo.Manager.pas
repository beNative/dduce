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

unit Demo.Manager;

interface

uses
  System.Classes,
  Vcl.Forms,

  Spring.Collections, Spring.Collections.Lists, Spring.Collections.Extensions;

type
  TDemo = class(TPersistent)
  strict private
    FName      : string;
    FFormClass : TComponentClass;

    function GetSourceFilename: string;

  published
    property Name: string
      read FName write FName;

    property FormClass: TComponentClass
      read FFormClass write FFormClass;

    property SourceFilename: string
      read GetSourceFilename;
  end;

  TDemoManager = record
    class function GetItemList: IList<TDemo>; static;
    class var
      FList: IList<TDemo>;

    class function Register(
            AFormClass : TComponentClass;
      const AName      : string = ''
    ): Boolean; static;

    class procedure Execute(AConcept: TObject); static;

    class constructor Create;

    class property ItemList: IList<TDemo>
      read GetItemList;
  end;

  DemoManager = TDemoManager;

implementation

uses
  System.SysUtils, System.StrUtils;

{$REGION 'construction and destruction'}
class constructor TDemoManager.Create;
begin
  FList := TCollections.CreateObjectList<TDemo>(True);
end;
{$ENDREGION}

{$REGION 'public methods'}
class procedure TDemoManager.Execute(AConcept: TObject);
var
  F : TComponent;
  C : TDemo;
begin
  C := AConcept as TDemo;
  F := C.FormClass.Create(Application);
  if F is TForm then
  begin
    with TForm(F) do
    begin
      Caption := C.Name;
      Position := TPosition.poScreenCenter;
      ShowModal;
    end
  end
  else
    raise Exception.CreateFmt('Cannot create %s', [C.FormClass.ClassName]);
end;

class function TDemoManager.GetItemList: IList<TDemo>;
begin
  Result := FList;
end;

class function TDemoManager.Register(AFormClass: TComponentClass;
  const AName: string): Boolean;
var
  S : string;
  C : TDemo;
begin
  S := IfThen(AName = '', AFormClass.ClassName, AName);
  C := TDemo.Create;
  C.Name := S;
  C.FormClass := AFormClass;
  FList.Add(C);
  Result := True;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TDemo.GetSourceFilename: string;
begin
  Result := FFormClass.UnitName;
end;
{$ENDREGION}

end.
