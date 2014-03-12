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

unit Demo.Manager;

interface

uses
  System.Classes,
  Vcl.Forms,

  DSharp.Collections;

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

  TDemoList = TObjectList<TDemo>;

  TDemoManager = record
    class var
      FList: IList;

    class function Register(
            AFormClass : TComponentClass;
      const AName      : string = ''
    ): Boolean; static;

    class procedure Execute(AConcept: TObject); static;

    class constructor Create;
    class destructor Destroy;

    class property ItemList: IList
      read FList;

  end;

  DemoManager = TDemoManager;

implementation

uses
  System.SysUtils, System.StrUtils;

{$REGION 'construction and destruction'}
class constructor TDemoManager.Create;
begin
  FList := TDemoList.Create(True);
end;

class destructor TDemoManager.Destroy;
begin
  FList := nil;
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
