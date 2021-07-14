{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Demo.DDuce.Reflect;

{ Form demonstrating the Reflect type. }

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,

  DDuce.Components.ValueList, DDuce.DynamicRecord;

type
  TfrmReflect = class(TForm)
    pnlReflected : TPanel;

  private
    FValueList : TValueList;
    FData      : DynamicRecord;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  Vcl.Dialogs,
  DDuce.Reflect;

{$REGION 'construction and destruction'}
procedure TfrmReflect.AfterConstruction;
begin
  inherited AfterConstruction;
  FValueList             := TValueList.Create(Self);
  FValueList.Parent      := Self;
  FValueList.Align       := alClient;
  FValueList.BorderStyle := bsNone;
  FData.FromString(Reflect.Properties(Self).ToString);
  FValueList.Data   := FData;
end;
{$ENDREGION}

end.
