{
  Copyright (C) 2013-2014 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit DDuce.Components.PropertyInspector.StringsEditor;

{$I ..\DDuce.inc}

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Winapi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.StdActns, Vcl.ActnList, Vcl.Menus,
  Vcl.ImgList;

resourcestring
  SCaption            = 'String List Editor';
  SOkBtnCaption       = '&Ok';
  SCancelBtnCaption   = 'Cancel';
  SLinesCountTemplate = '%d lines';

type
  TStringsEditorDialog = class(TForm)
    btnOk            : TButton;
    btnCancel        : TButton;
    lbLineCount      : TLabel;
    memMain          : TRichEdit;
    ppmMain          : TPopupMenu;
    aclMain          : TActionList;
    actEditCut       : TEditCut;
    actEditCopy      : TEditCopy;
    actEditPaste     : TEditPaste;
    actEditSelectAll : TEditSelectAll;
    actEditUndo      : TEditUndo;
    actEditDelete    : TEditDelete;
    mni1             : TMenuItem;
    mni2             : TMenuItem;
    mni3             : TMenuItem;
    mniN1            : TMenuItem;
    mni4             : TMenuItem;
    mni5             : TMenuItem;
    mniN2            : TMenuItem;
    mni6             : TMenuItem;
    imlMain          : TImageList;

    procedure memMainChange(Sender: TObject);

  private
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);

  public
    function Execute: Boolean;
    property Lines: TStrings
      read GetLines write SetLines;
  end;

implementation

{$R *.dfm}

{ TStringsEditorDialog }

function TStringsEditorDialog.Execute: Boolean;
begin
  Caption := SCaption;
  btnOk.Caption := SOkBtnCaption;
  btnCancel.Caption := SCancelBtnCaption;
  lbLineCount.Caption := Format(SLinesCountTemplate,
    [memMain.Lines.Count]);

  Result := (ShowModal = mrOk);
end;

function TStringsEditorDialog.GetLines: TStrings;
begin
  Result := memMain.Lines;
end;

procedure TStringsEditorDialog.SetLines(const Value: TStrings);
begin
  memMain.Lines := Value;
end;

procedure TStringsEditorDialog.memMainChange(Sender: TObject);
begin
  lbLineCount.Caption := Format(SLinesCountTemplate,
    [memMain.Lines.Count]);
end;

end.
