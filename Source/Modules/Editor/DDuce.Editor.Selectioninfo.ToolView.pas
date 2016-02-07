{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit DDuce.Editor.SelectionInfo.ToolView;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Forms, Vcl.StdCtrls, Vcl.Controls,

  DDuce.Editor.Interfaces;

type
  TfrmSelectionInfo = class(TForm, IEditorToolView)
    {$REGION 'designer controls'}
    btnStore                         : TButton;
    btnRestore                       : TButton;
    chkLockUpdates                   : TCheckBox;
    chkExcludeEmptyLines             : TCheckBox;
    lblCaretXY                       : TLabel;
    lblLineCount                     : TLabel;
    lblLineCountValue                : TLabel;
    lblLogicalCaretXY                : TLabel;
    lblCaretXYValue                  : TLabel;
    lblLogicalCaretXYValue           : TLabel;
    lblStoredBlockBegin              : TLabel;
    lblBlockBegin                    : TLabel;
    lblStoredBlockBeginValue         : TLabel;
    lblBlockBeginValue               : TLabel;
    lblBlockEnd                      : TLabel;
    lblBlockEndValue                 : TLabel;
    lblStoredCaretXY                 : TLabel;
    lblStoredCaretXYValue            : TLabel;
    lblStoredBlockSelectionMode      : TLabel;
    lblStoredBlockEndValue           : TLabel;
    lblStoredBlockEnd                : TLabel;
    lblStoredBlockLines              : TLabel;
    lblStoredBlockSelectionModeValue : TLabel;
    mmoBlock                         : TMemo;
    {$ENDREGION}

    procedure btnRestoreClick(Sender: TObject);
    procedure btnStoreClick(Sender: TObject);
    procedure mmoBlockChange(Sender: TObject);

  private
    function GetView: IEditorView;

  protected
    function GetForm: TForm;
    function GetName: string;
    function GetVisible: Boolean;

    { Lets the view respond to changes. }
    procedure UpdateView;

    property View: IEditorView
      read GetView;

    procedure UpdateDisplay;
    //procedure Refresh; TODO: refresh all items
    procedure UpdateActions; override;

  public
    procedure SetVisible(AValue: Boolean);
  end;

implementation

{$R *.dfm}

uses
  System.TypInfo,

  DDuce.Editor.Selection;
{
  StoredBlockBegin X Y
  StoredBlockEnd   X Y
  StoredBlockSelectionMode
  StoredBlockLines
}

{$REGION 'property access mehods'}
function TfrmSelectionInfo.GetView: IEditorView;
begin
  Result := Owner as IEditorView;
end;

function TfrmSelectionInfo.GetForm: TForm;
begin
  Result := Self;
end;

function TfrmSelectionInfo.GetName: string;
begin
  Result := Name;
end;

function TfrmSelectionInfo.GetVisible: Boolean;
begin
  Result := Visible;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmSelectionInfo.btnStoreClick(Sender: TObject);
begin
  View.Selection.Store(chkLockUpdates.Checked, chkExcludeEmptyLines.Checked);
end;

procedure TfrmSelectionInfo.mmoBlockChange(Sender: TObject);
begin
  View.Selection.Text := mmoBlock.Text;
end;

procedure TfrmSelectionInfo.SetVisible(AValue: Boolean);
begin
  inherited Visible := AValue;
end;

procedure TfrmSelectionInfo.btnRestoreClick(Sender: TObject);
begin
  View.Selection.Restore;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmSelectionInfo.UpdateView;
begin
  UpdateDisplay;
end;

procedure TfrmSelectionInfo.UpdateDisplay;
var
  ES : IEditorSelection;
begin
  ES := View.Selection;
  lblStoredBlockBeginValue.Caption := Format(
    '(%d, %d)', [ES.BlockBegin.X, ES.BlockBegin.Y]
  );
  lblStoredBlockEndValue.Caption := Format(
    '(%d, %d)', [ES.BlockEnd.X, ES.BlockEnd.Y]
  );
  lblStoredCaretXYValue.Caption := Format(
    '(%d, %d)', [ES.CaretXY.X, ES.CaretXY.Y]
  );

//  lblStoredBlockSelectionModeValue.Caption :=
//    GetEnumName(TypeInfo(TSynSelectionMode), Ord(ES.SelectionMode));

    lblBlockBeginValue.Caption := Format(
      '(%d, %d)', [View.BlockBegin.X, View.BlockBegin.Y]
    );
    lblBlockEndValue.Caption := Format(
      '(%d, %d)', [View.BlockEnd.X, View.BlockEnd.Y]
    );

    lblCaretXYValue.Caption := Format(
      '(%d, %d)', [View.CaretX, View.CaretY]
    );
    lblLogicalCaretXYValue.Caption := Format(
      '(%d, %d)', [View.LogicalCaretXY.X, View.LogicalCaretXY.Y]
    );

    lblLineCountValue.Caption := IntToStr(ES.Lines.Count);
    lblStoredBlockLines.Caption := ES.Text;
    mmoBlock.Lines.Text         := ES.Text;
  end;

  procedure TfrmSelectionInfo.UpdateActions;
  begin
    inherited UpdateActions;
    UpdateDisplay;
  end;
  {$ENDREGION}

end.

