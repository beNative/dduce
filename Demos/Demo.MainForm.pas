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

unit Demo.MainForm;

{$I ..\Source\DDuce.inc}

interface

uses
  System.Actions, System.UITypes, System.Classes,
  Vcl.ActnList, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.Forms,

  VirtualTrees,

  DDuce.Logger,

  DSharp.Windows.ColumnDefinitions, DSharp.Windows.TreeViewPresenter,

  Spring.Collections;

type
  TfrmMainMenu = class(TForm)
    aclMain        : TActionList;
    actClose       : TAction;
    actExecute     : TAction;
    actFocusFilter : TAction;
    btnExecute     : TBitBtn;
    edtFilter      : TEdit;
    pnlTop         : TPanel;
    pnlVST         : TPanel;
    sbrMain        : TStatusBar;
    vstDemos       : TVirtualStringTree;

    procedure actExecuteExecute(Sender: TObject);
    procedure actFocusFilterExecute(Sender: TObject);

    procedure vstDemosPaintBackground(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; R: TRect; var Handled: Boolean);
    procedure edtFilterChange(Sender: TObject);
    procedure FTVPFilter(Item: TObject; var Accepted: Boolean);
    procedure FTVPDoubleClick(Sender: TObject);
    function FTVPColumnDefinitionsCustomDrawColumn(
      Sender           : TObject;
      ColumnDefinition : TColumnDefinition;
      Item             : TObject;
      TargetCanvas     : TCanvas;
      CellRect         : TRect;
      ImageList        : TCustomImageList;
      DrawMode         : TDrawMode;
      Selected         : Boolean
    ): Boolean;

    procedure edtFilterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtFilterKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vstDemosKeyPress(Sender: TObject; var Key: Char);

  private
    FVKPressed : Boolean;
    FVST       : TVirtualStringTree;
    FTVP       : TTreeViewPresenter;

  public
    procedure AfterConstruction; override;

    procedure ApplyFilter;
  end;

var
  frmMainMenu: TfrmMainMenu;

implementation

{$R *.dfm}

uses
  System.StrUtils, System.SysUtils,
  Winapi.Windows, WinApi.Messages,
  Vcl.Graphics,

  Demo.Factories, Demo.Manager;

type
  TVKSet = set of Byte;

var
  VK_EDIT_KEYS : TVKSet = [
    VK_DELETE,
    VK_BACK,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END,
    VK_SHIFT,
    VK_CONTROL,
    VK_SPACE,
    Byte('0')..Byte('Z'),
    VK_OEM_1..VK_OEM_102,
    VK_MULTIPLY..VK_DIVIDE
  ];

  VK_CTRL_EDIT_KEYS : TVKSet = [
    VK_INSERT,
    VK_DELETE,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END,
    Byte('C'),
    Byte('X'),
    Byte('V'),
    Byte('Z')
  ];

  VK_SHIFT_EDIT_KEYS : TVKSet = [
    VK_INSERT,
    VK_DELETE,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END
  ];

resourcestring
  SDemosLoaded = '%d demos loaded.';

{$REGION 'construction and destruction'}
procedure TfrmMainMenu.AfterConstruction;
begin
  inherited;
  {$IFDEF DSHARP}
  Logger.Channels.Add(TIPCChannel.Create);
  FVST := vstDemos;
  FTVP := TDemoFactories.CreateTVP(Self);
  with FTVP.ColumnDefinitions.Add('Name') do
  begin
    ValuePropertyName := 'Name';
    AutoSize          := True;
    Alignment         := taCenter;
    OnCustomDraw      := FTVPColumnDefinitionsCustomDrawColumn;
  end;
  with FTVP.ColumnDefinitions.Add('UnitName') do
  begin
    ValuePropertyName := 'UnitName';
    AutoSize          := True;
  end;
  FTVP.TreeView := FVST;
  FTVP.View.ItemsSource := DemoManager.ItemList as IObjectList;
  FTVP.View.Filter.Add(FTVPFilter);
  FTVP.OnDoubleClick := FTVPDoubleClick;
  FVST.Header.AutoFitColumns;
  sbrMain.SimpleText := Format(SDemosLoaded, [DemoManager.ItemList.Count]);
  {$ENDIF}
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmMainMenu.actExecuteExecute(Sender: TObject);
begin
  DemoManager.Execute(FTVP.SelectedItem);
end;

procedure TfrmMainMenu.actFocusFilterExecute(Sender: TObject);
begin
  edtFilter.SetFocus;
  edtFilter.SelectAll;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMainMenu.FTVPDoubleClick(Sender: TObject);
begin
  actExecute.Execute;
end;

procedure TfrmMainMenu.FTVPFilter(Item: TObject; var Accepted: Boolean);
var
  C: TDemo;
begin
  if edtFilter.Text <> '' then
  begin
    C := TDemo(Item);
    Accepted :=
      ContainsText(C.Name, edtFilter.Text)
      or ContainsText(C.SourceFilename, edtFilter.Text);
  end
  else
    Accepted := True;
end;

procedure TfrmMainMenu.vstDemosKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
  begin
    Close;
  end
  else if Ord(Key) = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Close;
  end
  else if not edtFilter.Focused then
  begin
    edtFilter.SetFocus;
    PostMessage(edtFilter.Handle, WM_CHAR, Ord(Key), 0);
    edtFilter.SelStart := Length(edtFilter.Text);
    // required to prevent the invocation of accelerator keys!
    Key := #0;
  end;
end;

procedure TfrmMainMenu.vstDemosPaintBackground(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; R: TRect; var Handled: Boolean);
begin
  vstDemos.BackgroundOffsetX := vstDemos.ClientWidth - 128;
  vstDemos.BackgroundOffsetY := (vstDemos.ClientHeight) - 128;
  Handled := False;
end;

procedure TfrmMainMenu.edtFilterChange(Sender: TObject);
begin
  ApplyFilter;
  FVST.FocusedNode := FVST.GetFirstVisible;
  FVST.Selected[FVST.FocusedNode] := True;
end;

procedure TfrmMainMenu.edtFilterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  A : Boolean;
  B : Boolean;
  C : Boolean;
  D : Boolean;
begin
  A := (ssAlt in Shift) or (ssShift in Shift);
  B := (Key in VK_EDIT_KEYS) and (Shift = []);
  C := (Key in VK_CTRL_EDIT_KEYS) and (Shift = [ssCtrl]);
  D := (Key in VK_SHIFT_EDIT_KEYS) and (Shift = [ssShift]);
  if not (A or B or C or D) then
  begin
    FVKPressed := True;
    Key := 0;
  end;
end;

procedure TfrmMainMenu.edtFilterKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FVKPressed and FVST.Enabled then
  begin
    PostMessage(FVST.Handle, WM_KEYDOWN, Key, 0);
    if Visible and FVST.CanFocus then
      FVST.SetFocus;
    end;
    FVKPressed := False;
end;

function TfrmMainMenu.FTVPColumnDefinitionsCustomDrawColumn(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
begin
  TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
  Result := True;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMainMenu.ApplyFilter;
begin
  FTVP.ApplyFilter;
end;
{$ENDREGION}

end.

