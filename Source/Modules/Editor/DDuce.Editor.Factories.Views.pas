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

unit DDuce.Editor.Factories.Views;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Controls,

  DDuce.Editor.Interfaces;

type
{ TEditorViewFactory }

  TEditorViewFactory = class(TInterfacedObject, IEditorViewFactory)
    function CreateInstance(
             AParent      : TWinControl;
             AManager     : IEditorManager;
       const AName        : string = '';
       const AFileName    : string = '';
       const AHighlighter : string = 'TXT'
    ): IEditorView;
  end;

implementation

uses
  Vcl.Forms;

{ TEditorViewFactory }

function TEditorViewFactory.CreateInstance(AParent: TWinControl;
  AManager: IEditorManager; const AName: string; const AFileName: string;
  const AHighlighter: string): IEditorView;
var
  V: IEditorView;
begin
  V := AManager.Views.Add(AName, AFileName, AHighlighter);
  //V.Form.DisableAutoSizing;
  V.Form.BorderStyle := bsNone;
  V.Form.Align := alClient;
  V.Form.Parent := AParent;
  V.PopupMenu := AManager.Menus.EditorPopupMenu;
  V.Form.Visible := True;
  //V.Form.EnableAutoSizing;
  Result := V;
end;

end.

