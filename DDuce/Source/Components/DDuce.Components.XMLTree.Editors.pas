{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Components.XMLTree.Editors;

{$I ..\DDuce.inc}

interface

uses
  Messages, SysUtils, Classes, Controls, Forms, StdCtrls, Types,

{$IFDEF FPC}
  EditBtn, LCLIntf, LCLType,
{$ENDIF}

  VirtualTrees;

type
  // Node data record for the the document properties treeview.
  PPropertyData = ^TPropertyData;
  TPropertyData = record
    Value: String;      // This value can actually be a date or a number too.
    Changed: Boolean;
  end;

  TXMLEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FEdit: TEdit;        // One of the property editor classes.
    FTree: TCustomVirtualStringTree;
    FNode: PVirtualNode;       // The node being edited.
    FColumn: Integer;          // The column of the node being edited.
  protected
    procedure EditExit(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    destructor Destroy; override;

    { IVTEditLink }
    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

type
  TPropertyTextKind = (
    ptkText,
    ptkHint
  );

type
  PGridData = ^TGridData;
  TGridData = record
    ValueType: array[0..3] of TValueType; // one for each column
    Value: array[0..3] of Variant;
    Changed: Boolean;
  end;

implementation

uses
  Windows,

  DDuce.Components.XMLTree;

// This implementation is used in VST3 to make a connection beween the tree
// and the actual edit window which might be a simple edit, a combobox
// or a memo etc.

destructor TXMLEditLink.Destroy;
begin
  FreeAndNil(FEdit);
  inherited;
end;

procedure TXMLEditLink.EditExit(Sender: TObject);
begin
  FTree.EndEditNode;
end;

procedure TXMLEditLink.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  CanAdvance: Boolean;
begin
  CanAdvance := True;
  case Key of
    VK_ESCAPE:
      if CanAdvance then
      begin
        FTree.CancelEditNode;
        Key := 0;
      end;
    VK_RETURN:
      if CanAdvance then
      begin
        FTree.EndEditNode;
        Key := 0;
      end;
    VK_UP,
    VK_DOWN:
      begin
        // Consider special cases before finishing edit mode.
        CanAdvance := Shift = [];
        if CanAdvance then
        begin
          // Forward the keypress to the tree. It will asynchronously change the focused node.
          PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
          Key := 0;
        end;
      end;
  end;
end;

function TXMLEditLink.BeginEdit: Boolean; stdcall;
begin
  Result := True;
  FEdit.Show;
  FEdit.SetFocus;
end;

function TXMLEditLink.CancelEdit: Boolean; stdcall;
begin
  Result := True;
  FEdit.Hide;
end;

function TXMLEditLink.EndEdit: Boolean; stdcall;
var
  S: string;
begin
  Result := True;
  S := FEdit.Text;
  if S <> FTree.Text[FNode, FColumn] then
  begin
    FTree.Text[FNode, FColumn] := S;
  end;
  FTree.InvalidateNode(FNode);
  FEdit.Hide;
end;

function TXMLEditLink.GetBounds: TRect; stdcall;
begin
  Result := FEdit.BoundsRect;
end;

function TXMLEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex): Boolean; stdcall;
begin
  Result := True;
  FTree := Tree as TCustomVirtualStringTree;
  FNode := Node;
  FColumn := Column;
  FreeAndNil(FEdit);
  FEdit := TEdit.Create(nil);
  FEdit.BorderStyle := bsNone;
  FEdit.AutoSize := False;
  FEdit.Visible := False;
  FEdit.Font.Name := Tree.Font.Name;
  FEdit.Font.Size :=  8;
  FEdit.Parent := Tree;
  FEdit.Text := FTree.Text[Node, FColumn];
  FEdit.SelectAll;
  FEdit.OnKeyDown := EditKeyDown;
end;

procedure TXMLEditLink.ProcessMessage(var Message: TMessage); stdcall;
begin
  FEdit.WindowProc(Message);
end;

procedure TXMLEditLink.SetBounds(R: TRect); stdcall;
var
  Dummy: Integer;
begin
  // Since we don't want to activate grid extensions in the tree (this would influence how the selection is drawn)
  // we have to set the edit's width explicitly to the width of the column.
  TXMLTree(FTree).Header.Columns.GetColumnBounds(FColumn, Dummy, R.Right);
  FEdit.BoundsRect := R;
end;

end.
