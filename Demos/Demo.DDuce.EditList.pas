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

unit Demo.DDuce.EditList;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Rtti,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  DDuce.EditList;

type
  TfrmEditList = class(TForm)
    pnlLeft        : TPanel;
    pnlRight       : TPanel;
    splVertical    : TSplitter;
    chkMultiSelect : TCheckBox;
    edtName        : TLabeledEdit;
    edtValue       : TLabeledEdit;
    mmoData        : TMemo;

    procedure chkMultiSelectClick(Sender: TObject);

  private
    FEditList : TEditList;

    procedure FEditListAdd(
      ASender    : TObject;
      var AName  : string;
      var AValue : TValue
    );

    procedure FEditListExecute(
      ASender    : TObject;
      var AName  : string;
      var AValue : TValue
    );

    procedure FEditListExecuteItem(
      ASender    : TObject;
      var AName  : string;
      var AValue : TValue
    );

    procedure FEditListDelete(
      ASender    : TObject;
      var AName  : string;
      var AValue : TValue
    );

    procedure FEditListDeleteItem(
      ASender    : TObject;
      var AName  : string;
      var AValue : TValue
    );
  protected
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmEditList.AfterConstruction;
begin
  inherited AfterConstruction;
  FEditList := TEditList.Create(Self, pnlLeft);
  FEditList.OnAdd.Add(FEditListAdd);
  FEditList.OnDelete.Add(FEditListDelete);
  FEditList.OnDeleteItem.Add(FEditListDeleteItem);
  FEditList.OnExecute.Add(FEditListExecute);
  FEditList.OnExecuteItem.Add(FEditListExecuteItem);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmEditList.chkMultiSelectClick(Sender: TObject);
begin
  FEditList.ValueList.MultiSelect := (Sender as TCheckBox).Checked;
end;

procedure TfrmEditList.FEditListAdd(ASender: TObject; var AName: string;
  var AValue: TValue);
begin
//
end;

procedure TfrmEditList.FEditListDelete(ASender: TObject; var AName: string;
  var AValue: TValue);
begin
//
end;

procedure TfrmEditList.FEditListDeleteItem(ASender: TObject;
  var AName: string; var AValue: TValue);
begin
//
end;

procedure TfrmEditList.FEditListExecute(ASender: TObject; var AName: string;
  var AValue: TValue);
begin
  ShowMessageFmt('Executed %s = %s', [AName, AValue.ToString]);
end;

procedure TfrmEditList.FEditListExecuteItem(ASender: TObject;
  var AName: string; var AValue: TValue);
begin
  ShowMessageFmt('Executed %s = %s', [AName, AValue.ToString]);
end;

{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmEditList.UpdateActions;
begin
  inherited UpdateActions;
  mmoData.Lines.Text := FEditList.Data.ToString;
end;
{$ENDREGION}

end.
