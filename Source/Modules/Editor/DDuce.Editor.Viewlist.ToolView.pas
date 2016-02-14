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

unit DDuce.Editor.ViewList.ToolView;

interface

uses
  System.Classes, System.SysUtils, System.Contnrs, System.Actions,
  Vcl.Forms, Vcl.ActnList, Vcl.Menus, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Controls,

  Spring.Collections,

  VirtualTrees,

  DSharp.Windows.TreeViewPresenter,

  DDuce.Editor.Interfaces, DDuce.Editor.ToolView.Base;

type
  TfrmViewList = class(TCustomEditorToolView, IEditorToolView)
    aclMain               : TActionList;
    actClose              : TAction;
    actCloseSelectedViews : TAction;
    btnClose              : TButton;
    mniClose              : TMenuItem;
    pnlBottom             : TPanel;
    pnlVST                : TPanel;
    ppmMain               : TPopupMenu;

    procedure actCloseExecute(Sender: TObject);
    procedure actCloseSelectedViewsExecute(Sender: TObject);

  private
    FVST      : TVirtualStringTree;
    FItemList : IObjectList;
    FTVP      : TTreeViewPresenter;

    procedure FTVPSelectionChanged(Sender: TObject);

  protected
    procedure UpdateView; override;
    procedure UpdateActions; override;
    procedure Refresh;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{$R *.dfm}

uses
  DSharp.Windows.ColumnDefinitions,

  DDuce.Factories, DDuce.Editor.ViewList.Data;

resourcestring
  SFileName    = 'Filename';
  SHighlighter = 'Highlighter';
  SModified    = 'Modified';
  SPath        = 'Path';

{$REGION 'construction and destruction'}
procedure TfrmViewList.AfterConstruction;
begin
  inherited AfterConstruction;
  FVST := TFactories.CreateVirtualStringTree(Self, pnlVST);
  FTVP := TTreeViewPresenter.Create(Self);
  //FTVP.MultiSelect := True;
  with FTVP.ColumnDefinitions.Add(SFileName, 200) do
  begin

  end;
  FTVP.ColumnDefinitions.Add(SHighlighter, 80);
  with FTVP.ColumnDefinitions.Add(SModified, 80) do
  begin
    ColumnType := TColumnType.ctCheckBox;
  end;
  with FTVP.ColumnDefinitions.Add(SPath, 100) do
  begin
  end;
  FItemList := TCollections.CreateObjectList<TEditorViewInfo> as IObjectList;
  Refresh;
  FTVP.View.ItemsSource   := FItemList;
  FTVP.PopupMenu          := ppmMain;
  FTVP.TreeView           := FVST;
  FTVP.OnSelectionChanged := FTVPSelectionChanged;
end;

procedure TfrmViewList.BeforeDestruction;
begin
  FreeAndNil(FItemList);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmViewList.actCloseExecute(Sender: TObject);
begin
  ModalResult := mrClose;
end;

procedure TfrmViewList.actCloseSelectedViewsExecute(Sender: TObject);
var
  V : IEditorView;
  I : Integer;
begin
  for I := 0 to FTVP.SelectedItems.Count - 1 do
  begin
    V := TEditorViewInfo(FTVP.SelectedItems[I]).View;
    Views.Delete(V);
  end;
  Refresh;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmViewList.FTVPSelectionChanged(Sender: TObject);
var
  V: IEditorView;
begin
  if Assigned(FTVP.SelectedItem) then
  begin
    V := (FTVP.SelectedItem as TEditorViewInfo).View;
    Manager.ActiveView := V;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmViewList.UpdateView;
begin
  FVST.Invalidate;
end;

procedure TfrmViewList.UpdateActions;
begin
  if FItemList.Count <> Views.Count then
  begin
    Refresh;
  end;
  inherited UpdateActions;
end;

procedure TfrmViewList.Refresh;
var
  I: Integer;
begin
  FItemList.Clear;
  for I := 0 to Views.Count - 1 do
  begin
    FItemList.Add(TEditorViewInfo.Create(Views[I]));
  end;
  FTVP.Refresh;
end;
{$ENDREGION}

end.

