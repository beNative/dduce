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

unit Demo.DDuce.ValueList;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
              DDuce.DynamicRecord,
  DDuce.ValueList;

type
  TfrmValueListDemo = class(TForm)
  private
    FValueList : TfrmValueList;
    dr: dynamicrecord;
  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  Spring,

   DDuce.Logger;

{$REGION 'construction and destruction'}
procedure TfrmValueListDemo.AfterConstruction;
var
  SL : Shared<TStringList>;
  s:string;
begin
  inherited AfterConstruction;
  SL := TStringList.Create;
  FValueList := TfrmValueList.Create(Self);
  FValueList.Parent := Self;
  FValueList.Align := alClient;
  FValueList.BorderStyle := bsNone;
  FValueList.Visible := True;
  //dr.from(Self);
  //DynamicRecord.Create(Self).ToString(SL.Value.Text);

  //FValueList.Data := DynamicRecord.Create(Self);
  //FValueList.Data := DynamicRecord.Create;
  dr['one'] := 1;
  dr['two'] := 2;
  dr['three'] := 'drie';

  //s := SL.Value.Text;
  //dr.FromStrings(SL.Value, False);
  //FValueList.Data := dr;
  FValueList.Data := dr;
  //ShowMessage(FValueList.Data.tostring);
//  Logger.SendText(FValueList.Data.ToString);
end;
{$ENDREGION}

end.
