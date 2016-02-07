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

unit DDuce.Editor.Factories.Toolbars;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Menus, Vcl.Controls, Vcl.ActnList, Vcl.ComCtrls, Vcl.Toolwin,

  DDuce.Editor.Interfaces, DDuce.Editor.Resources;

const
  DEFAULT_EDGE_BORDERS = [ebLeft, ebTop, ebRight, ebBottom];
  DEFAULT_EDGE_INNER   = esNone;
  DEFAULT_EDGE_OUTER   = esNone;
  DEFAULT_TRANSPARANT  = True;

type
  TEditorToolbarsFactory = class(TInterfacedObject, IEditorToolbarsFactory)
  strict private
    FActions : IEditorActions;
    FMenus   : IEditorMenus;

    FEdgeBorders : TEdgeBorders;
    FEdgeInner   : TEdgeStyle;
    FEdgeOuter   : TEdgeStyle;
    FTransparant : Boolean;

    procedure ApplyDefaultProperties(
      AToolbar : TToolbar
    );

    function CreateToolButton(
       AParent    : TToolBar;
       AAction    : TBasicAction;
       APopupMenu : TPopupMenu = nil
    ): TToolButton; overload;

    function CreateToolButton(
            AParent     : TToolBar;
      const AActionName : string = '';
            APopupMenu  : TPopupMenu = nil
    ): TToolButton; overload;

  public
    procedure AfterConstruction; override;

    constructor Create(
      AActions  : IEditorActions;
      AMenus    : IEditorMenus
    );

    function CreateMainToolbar(
        AOwner  : TComponent;
        AParent : TWinControl
    ): TToolbar;

    function CreateSelectionToolbar(
        AOwner  : TComponent;
        AParent : TWinControl
    ): TToolbar;

    function CreateRightToolbar(
        AOwner  : TComponent;
        AParent : TWinControl
    ): TToolbar;

    property EdgeBorders: TEdgeBorders
      read FEdgeBorders write FEdgeBorders default DEFAULT_EDGE_BORDERS;

    property EdgeInner: TEdgeStyle
      read FEdgeInner write FEdgeInner default DEFAULT_EDGE_INNER;

    property EdgeOuter: TEdgeStyle
      read FEdgeOuter write FEdgeOuter default DEFAULT_EDGE_OUTER;

    property Transparant: Boolean
      read FTransparant write FTransparant default DEFAULT_TRANSPARANT;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TEditorToolbarsFactory.AfterConstruction;
begin
  inherited AfterConstruction;
  FEdgeBorders := DEFAULT_EDGE_BORDERS;
  FEdgeInner   := DEFAULT_EDGE_INNER;
  FEdgeOuter   := DEFAULT_EDGE_OUTER;
  FTransparant := DEFAULT_TRANSPARANT;
end;

constructor TEditorToolbarsFactory.Create(AActions: IEditorActions;
  AMenus: IEditorMenus);
begin
  inherited Create;
  FActions := AActions;
  FMenus   := AMenus;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TEditorToolbarsFactory.ApplyDefaultProperties(AToolbar: TToolbar);
begin
  AToolbar.EdgeBorders := EdgeBorders;
  AToolbar.EdgeInner   := EdgeInner;
  AToolbar.EdgeOuter   := EdgeOuter;
  AToolbar.Transparent := Transparant;
end;

function TEditorToolbarsFactory.CreateToolButton(AParent: TToolBar;
  AAction: TBasicAction; APopupMenu: TPopupMenu): TToolButton;
var
  TB: TToolButton;
begin
  TB := TToolButton.Create(AParent);
  TB.Parent := AParent;
  if not Assigned(AAction) then
  begin
    TB.Style := tbsDivider;
  end
  else
  begin
    if Assigned(APopupMenu) then
    begin
      TB.Style        := tbsDropDown;
      TB.DropdownMenu := APopupMenu;
    end;
    TB.Action := AAction;
  end;
  Result := TB;
end;

function TEditorToolbarsFactory.CreateToolButton(AParent: TToolBar;
  const AActionName: string; APopupMenu: TPopupMenu): TToolButton;
begin
  if AActionName = '' then
    Result := CreateToolButton(AParent, nil)
  else
    Result := CreateToolButton(AParent, FActions[AActionName], APopupMenu);
end;
{$ENDREGION}

{$REGION 'public methods'}
function TEditorToolbarsFactory.CreateMainToolbar(AOwner: TComponent;
  AParent: TWinControl): TToolbar;
var
  TB : TToolbar;
begin
  TB := TToolBar.Create(AOwner);
  ApplyDefaultProperties(TB);
  TB.Parent := AParent;
  TB.Images := FActions.ActionList.Images;
  TB.ButtonWidth:= 10;

  CreateToolButton(TB, 'actNew');
  CreateToolButton(TB, 'actOpen');
  CreateToolButton(TB, 'actSave');
  CreateToolButton(TB, 'actSaveAs');
  CreateToolButton(TB, 'actSaveAll');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actReload');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actCut');
  CreateToolButton(TB, 'actCopy');
  CreateToolButton(TB, 'actPaste');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actUndo');
  CreateToolButton(TB, 'actRedo');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actSearch');
  CreateToolButton(TB, 'actSearchReplace');
  CreateToolButton(TB);
//  CreateToolButton(TB, 'actToggleFoldLevel', FMenus.FoldPopupMenu);
  CreateToolButton(TB);
  CreateToolButton(TB, 'actToggleHighlighter', FMenus.HighlighterPopupMenu);
//  CreateToolButton(TB, 'actAutoGuessHighlighter');
//  CreateToolButton(TB);
//  CreateToolButton(TB, 'actShowCodeShaper');
//  CreateToolButton(TB, 'actShowCodeFilter');
//  CreateToolButton(TB, 'actShowCharacterMap');
//  CreateToolButton(TB, 'actShowPreview');
//  CreateToolButton(TB);
//  CreateToolButton(TB, 'actSmartSelect');
//  CreateToolButton(TB, 'actFormat');
//  CreateToolButton(TB);
//  CreateToolButton(TB, 'actSettings');
//  CreateToolButton(TB);
//  CreateToolButton(TB, 'actShowSpecialCharacters');
//  CreateToolButton(TB);
//  CreateToolButton(TB, 'actCreateDesktopLink');
//  CreateToolButton(TB);
//  CreateToolButton(TB, 'actAbout');
  Result := TB;
end;

function TEditorToolbarsFactory.CreateRightToolbar(AOwner: TComponent;
  AParent: TWinControl): TToolbar;
var
  TB : TToolbar;
begin
  TB := TToolBar.Create(AOwner);
  ApplyDefaultProperties(TB);
  TB.Parent := AParent;
  TB.Images := FActions.ActionList.Images;

//  CreateToolButton(TB, 'actShowViews');
//  CreateToolButton(TB, 'actSingleInstance');
//  CreateToolButton(TB, 'actStayOnTop');
//  CreateToolButton(TB, 'actClose');

  Result := TB;
end;

function TEditorToolbarsFactory.CreateSelectionToolbar(AOwner: TComponent;
  AParent: TWinControl): TToolbar;
var
  TB : TToolbar;
begin
  TB := TToolBar.Create(AOwner);
  ApplyDefaultProperties(TB);
  TB.Parent := AParent;
  TB.Images := FActions.ActionList.Images;

  CreateToolButton(TB, 'actAlignSelection');
  CreateToolButton(TB, 'actSortSelection');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actIndent');
  CreateToolButton(TB, 'actUnindent');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actMergeBlankLines');
  CreateToolButton(TB, 'actCompressSpace');
  CreateToolButton(TB, 'actCompressWhitespace');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actFormat');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actSyncEdit');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actQuoteLines');
  CreateToolButton(TB, 'actDeQuoteLines');
  CreateToolButton(TB, 'actQuoteSelection');
  CreateToolButton(TB, 'actDeQuoteSelection');
  CreateToolButton(TB, 'actQuoteLinesAndDelimit');
  CreateToolButton(TB, 'actPascalStringOfSelection');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actLowerCaseSelection');
  CreateToolButton(TB, 'actUpperCaseSelection');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actConvertTabsToSpaces');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actStripFirstChar');
  CreateToolButton(TB, 'actStripLastChar');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actStripComments');
  CreateToolButton(TB, 'actStripMarkup');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actToggleBlockCommentSelection');
  Result := TB;
end;
{$ENDREGION}

end.

