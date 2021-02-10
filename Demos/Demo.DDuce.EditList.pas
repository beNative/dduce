{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

{ Demonstrates the TEditList module. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Rtti,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  DDuce.EditList;

type
  TfrmEditList = class(TForm)
    chkMultiSelect : TCheckBox;
    lblSelected    : TLabel;
    mmoData        : TMemo;
    pnlLeft        : TPanel;
    pnlRight       : TPanel;
    splVertical    : TSplitter;
    btn1: TButton;

    procedure chkMultiSelectClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);

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
    procedure FEditListItemExecute(
      ASender    : TObject;
      var AName  : string;
      var AValue : TValue
    );
    procedure FEditListDelete(
      ASender    : TObject;
      var AName  : string;
      var AValue : TValue
    );
    procedure FEditListItemDelete(
      ASender    : TObject;
      var AName  : string;
      var AValue : TValue
    );
    procedure FEditListDuplicate(
      ASender    : TObject;
      var AName  : string;
      var AValue : TValue
    );
    procedure FEditListItemDuplicate(
      ASender    : TObject;
      var AName  : string;
      var AValue : TValue
    );
    procedure FEditListItemMoveUp(
      ASender    : TObject;
      var AName  : string;
      var AValue : TValue
    );
    procedure FEditListItemMoveDown(
      ASender    : TObject;
      var AName  : string;
      var AValue : TValue
    );

  protected
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

  end;

implementation

uses
  DDuce.Logger, DDuce.DynamicRecord;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmEditList.AfterConstruction;
begin
  inherited AfterConstruction;
  FEditList := TEditList.Create(Self, pnlLeft);
  FEditList.AlignWithMargins := True;
  FEditList.OnAdd.Add(FEditListAdd);
  FEditList.OnDelete.Add(FEditListDelete);
  FEditList.OnItemDelete.Add(FEditListItemDelete);
  FEditList.OnExecute.Add(FEditListExecute);
  FEditList.OnItemExecute.Add(FEditListItemExecute);
  FEditList.OnDuplicate.Add(FEditListDuplicate);
  FEditList.OnItemDuplicate.Add(FEditListItemDuplicate);
  FEditList.OnItemMoveUp.Add(FEditListItemMoveUp);
  FEditList.OnItemMoveDown.Add(FEditListItemMoveDown);
end;

destructor TfrmEditList.Destroy;
begin
  FEditList.OnAdd.RemoveAll(Self);
  FEditList.OnDelete.RemoveAll(Self);
  FEditList.OnItemDelete.RemoveAll(Self);
  FEditList.OnExecute.RemoveAll(Self);
  FEditList.OnItemExecute.RemoveAll(Self);
  FEditList.OnDuplicate.RemoveAll(Self);
  FEditList.OnItemDuplicate.RemoveAll(Self);
  FEditList.OnItemMoveUp.RemoveAll(Self);
  FEditList.OnItemMoveDown.RemoveAll(Self);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmEditList.btn1Click(Sender: TObject);
begin
  FEditList.Refresh;
  //FEditList.ValueList.SelectNode(FEditList.ValueList.Data.Count - 1);
end;

procedure TfrmEditList.chkMultiSelectClick(Sender: TObject);
begin
  //FEditList.ValueList.MultiSelect := (Sender as TCheckBox).Checked;
end;

procedure TfrmEditList.FEditListAdd(ASender: TObject; var AName: string;
  var AValue: TValue);
begin
  Logger.Track(Self, 'FEditListAdd');
  Logger.Send(AName, AValue);
end;

procedure TfrmEditList.FEditListDelete(ASender: TObject; var AName: string;
  var AValue: TValue);
begin
  Logger.Track(Self, 'FEditListDelete');
  Logger.Send(AName, AValue);
end;

procedure TfrmEditList.FEditListItemDelete(ASender: TObject;
  var AName: string; var AValue: TValue);
begin
  Logger.Track(Self, 'FEditListItemDelete');
  Logger.Send(AName, AValue);
end;

procedure TfrmEditList.FEditListDuplicate(ASender: TObject; var AName: string;
  var AValue: TValue);
begin
  Logger.Track(Self, 'FEditListDuplicate');
  Logger.Send(AName, AValue);
end;

procedure TfrmEditList.FEditListItemDuplicate(ASender: TObject;
  var AName: string; var AValue: TValue);
begin
  Logger.Track(Self, 'FEditListItemDuplicate');
  Logger.Send(AName, AValue);
end;

procedure TfrmEditList.FEditListExecute(ASender: TObject; var AName: string;
  var AValue: TValue);
begin
  Logger.Track(Self, 'FEditListExecute');
  Logger.Send(AName, AValue);
  ShowMessageFmt('Executed %s = %s', [AName, AValue.ToString]);
end;

procedure TfrmEditList.FEditListItemExecute(ASender: TObject;
  var AName: string; var AValue: TValue);
begin
  Logger.Track(Self, 'FEditListItemExecute');
  Logger.Send(AName, AValue);
  ShowMessageFmt('Executed %s = %s', [AName, AValue.ToString]);
end;

procedure TfrmEditList.FEditListItemMoveDown(ASender: TObject;
  var AName: string; var AValue: TValue);
begin
  Logger.Track(Self, 'FEditListItemMoveDown');
  Logger.Send(AName, AValue);
end;

procedure TfrmEditList.FEditListItemMoveUp(ASender: TObject; var AName: string;
  var AValue: TValue);
begin
  Logger.Track(Self, 'FEditListItemMoveUp');
  Logger.Send(AName, AValue);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmEditList.UpdateActions;
var
  F : IDynamicField;
begin
  inherited UpdateActions;
//  mmoData.Lines.Text := FEditList.Data.ToString;
//  if Assigned(FEditList.ValueList.FocusedField) then
//  begin
//    F := FEditList.ValueList.FocusedField;
//    lblSelected.Caption := Format('Selected: %s: %s', [F.Name, F.Value.ToString]);
//  end;
end;
{$ENDREGION}

end.
