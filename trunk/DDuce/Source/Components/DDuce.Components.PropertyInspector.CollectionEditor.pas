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

unit DDuce.Components.PropertyInspector.CollectionEditor;

{$I ..\DDuce.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ActnList, ImgList, ToolWin, Menus,
  Grids, TypInfo,

{$IFDEF HAS_UNIT_SYSTEM_ACTIONS}
  System.Actions,
{$ENDIF}

  DDuce.Components.PropertyInspector;

type
  TfrmCollectionEditor = class(TForm)
    aclMain           : TActionList;
    actAdd            : TAction;
    actDelete         : TAction;
    actDeleteAll      : TAction;
    actDown           : TAction;
    actSelectAll      : TAction;
    actUp             : TAction;
    btnAdd            : TToolButton;
    btnDelete         : TToolButton;
    btnDown           : TToolButton;
    btnSeperator      : TToolButton;
    btnUp             : TToolButton;
    imlMain           : TImageList;
    lvCollectionItems : TListView;
    mniAdd            : TMenuItem;
    mniDelete         : TMenuItem;
    mniDeleteAll      : TMenuItem;
    mniSelectAll      : TMenuItem;
    N1                : TMenuItem;
    pnlLeft           : TPanel;
    pnlRight          : TPanel;
    ppmMain           : TPopupMenu;
    splVertical       : TSplitter;
    tlbMain           : TToolBar;

    procedure actAddExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actUpExecute(Sender: TObject);
    procedure actDownExecute(Sender: TObject);
    procedure lvCollectionItemsDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure lvCollectionItemsDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure actSelectAllExecute(Sender: TObject);
    procedure lvCollectionItemsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);

  private
    FCollection : TCollection;
    FInspector  : TPropertyInspector;

    procedure FInspectorModified(Sender: TObject);
    function GetActiveItem: TCollectionItem;

  protected
    procedure UpdateItems;
    procedure UpdateInspector;
    procedure UpdateActions; override;

  public
    constructor Create(AOwner      : TComponent;
                       ACollection : TCollection); reintroduce;

    property ActiveItem : TCollectionItem
      read GetActiveItem;
  end;

procedure ExecuteCollectionEditor(ACollection : TCollection);

implementation

uses
  Rtti,

  DDuce.Reflect;

{$R *.dfm}

procedure ExecuteCollectionEditor(ACollection : TCollection);
var
  Form : TfrmCollectionEditor;
begin
  Form := TfrmCollectionEditor.Create(Application, ACollection);
  try
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

constructor TfrmCollectionEditor.Create(AOwner: TComponent;
  ACollection: TCollection);
begin
  inherited Create(AOwner);
  FCollection := ACollection;
  FInspector  := TPropertyInspector.Create(Self);
  FInspector.Parent := pnlRight;
  FInspector.Align  := alClient;
  FInspector.OnModified := FInspectorModified;
  UpdateItems;
end;

function TfrmCollectionEditor.GetActiveItem: TCollectionItem;
var
  I : Integer;
begin
  if lvCollectionItems.ItemIndex = -1 then
    I := 0
  else
    I := lvCollectionItems.ItemIndex;

  Result := TCollectionItem(lvCollectionItems.Items[I].Data);
end;

{$REGION 'action handlers'}
procedure TfrmCollectionEditor.actAddExecute(Sender: TObject);
begin
  FCollection.Add;
  UpdateItems;
end;

procedure TfrmCollectionEditor.actDeleteExecute(Sender: TObject);
begin
  if FCollection.Count > 0 then
  begin
    FCollection.Delete(ActiveItem.Index);
    UpdateItems;
  end;
end;

procedure TfrmCollectionEditor.actSelectAllExecute(Sender: TObject);
begin
  lvCollectionItems.SelectAll;
end;

procedure TfrmCollectionEditor.actUpExecute(Sender: TObject);
var
  I : Integer;
begin
  if ActiveItem.Index > 0 then
  begin
    ActiveItem.Index := ActiveItem.Index - 1;
    I := ActiveItem.Index;
    UpdateItems;
    lvCollectionItems.ItemIndex := I;
  end;
end;

procedure TfrmCollectionEditor.actDownExecute(Sender: TObject);
var
  I : Integer;
begin
  if ActiveItem.Index < FCollection.Count - 1 then
  begin
    ActiveItem.Index := ActiveItem.Index + 1;
    I := ActiveItem.Index;
    UpdateItems;
    lvCollectionItems.ItemIndex := I;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmCollectionEditor.lvCollectionItemsDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  if not Assigned(Sender) or not Assigned(Source) or
     not Assigned(lvCollectionItems.DropTarget) or
     not Assigned(lvCollectionItems.Selected) or
     (lvCollectionItems.Selected.Index = lvCollectionItems.DropTarget.Index)
  then
    Exit;

  FCollection.Items[lvCollectionItems.Selected.Index].Index :=
    lvCollectionItems.DropTarget.Index;
  UpdateItems;
end;

procedure TfrmCollectionEditor.lvCollectionItemsDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TfrmCollectionEditor.lvCollectionItemsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
    UpdateInspector;
end;

procedure TfrmCollectionEditor.FInspectorModified(Sender: TObject);
var
  I  : Integer;
  J  : Integer;
  S  : string;
  V  : TValue;
  AI : TPropsPageItem;
  PI : TPropsPageItem;
  SL : TStringList;
  O  : TObject;
begin
  SL := TStringList.Create;
  try
    AI := FInspector.ActiveItem;
    O  := FInspector.Objects[0];
    S  := AI.Caption;
    V  := Reflect.Properties(O).Values[S];
    PI := AI;
    while Assigned(PI) do
    begin
      SL.Add(PI.Caption);
      PI := PI.Parent;
    end;
    for I := 0 to lvCollectionItems.Items.Count - 1 do
    begin
      if lvCollectionItems.Items[I].Selected then
      begin
        O := FCollection.Items[I];
        for J := SL.Count -1 downto 1 do
          O := GetObjectProp(O, SL[J]);
        Reflect.Properties(O).Values[S] := V;
      end;
    end;
  finally
    SL.Free;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmCollectionEditor.UpdateActions;
var
  B: Boolean;
begin
  inherited;
  B := FCollection.Count > 1;
  actDelete.Enabled := FCollection.Count > 0;
  actUp.Enabled := B and (lvCollectionItems.ItemIndex > 0);
  actDown.Enabled := B and (lvCollectionItems.ItemIndex < FCollection.Count - 1);
end;

procedure TfrmCollectionEditor.UpdateInspector;
var
  I : Integer;
begin
  FInspector.BeginUpdate;
  try
    FInspector.Clear;
    if Assigned(lvCollectionItems.Items[lvCollectionItems.ItemIndex].Data) then
    begin
      FInspector.Add(
        TPersistent(lvCollectionItems.Items[lvCollectionItems.ItemIndex].Data)
      );

      for I := 0 to FInspector.Items.Count - 1 do
      begin
        if FInspector.Items[I].Expandable = mieYes then
          FInspector.Items[I].Expand;
      end;
    end;
  finally
    FInspector.EndUpdate;
    UpdateActions;
  end;
end;

procedure TfrmCollectionEditor.UpdateItems;
var
  S  : string;
  I  : Integer;
  LI : TListItem;
begin
  lvCollectionItems.Clear;
  for I := 0 to FCollection.Count - 1 do
  begin
    S := Format('%s', [FCollection.Items[I].DisplayName]);
    LI := lvCollectionItems.Items.Add;
    LI.Caption := IntToStr(I);
    LI.SubItems.Add(S);
    LI.Data := FCollection.Items[I];
  end;
end;
{$ENDREGION}

end.
