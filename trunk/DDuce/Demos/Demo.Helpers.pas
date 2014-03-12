{
  Copyright (C) 2013-2014 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit Demo.Helpers;

{$I ..\Source\DDuce.inc}

interface

uses
  Classes, Controls, Contnrs, DB, DBGrids, StdCtrls,

  VirtualTrees,

{$IFDEF DSHARP}
  DSharp.Windows.TreeViewPresenter, DSharp.Collections, DSharp.Bindings,
  DSharp.Bindings.Collections, DSharp.Core.DataTemplates, DSharp.Core.Events,
  DSharp.Core.Validations,
{$ENDIF}

{$IFDEF SPRING}
  Spring, Spring.Collections, Spring.Collections.Lists,
{$ENDIF}

  DDuce.Components.PropertyInspector, DDuce.Components.LogTree,
  DDuce.Components.GridView, DDuce.Components.DBGridView,
  DDuce.Components.VirtualDBGrid,

  Demo.Contact;

function CreateLogTree(
  AOwner  : TComponent;
  AParent : TWinControl
): TLogTree;

function CreateInspector(
  AOwner  : TComponent;
  AParent : TWinControl;
  AObject : TPersistent = nil
): TPropertyInspector;

function CreateVST(
        AOwner  : TComponent;
        AParent : TWinControl;
  const AName   : string = ''
): TVirtualStringTree;

{$IFDEF DSHARP}
function CreateTVP(
        AOwner    : TComponent;
        AVST      : TVirtualStringTree = nil;
        ASource   : IList = nil;
        ATemplate : IDataTemplate = nil;
        AFilter   : TFilterEvent = nil;
  const AName     : string = ''
): TTreeViewPresenter;

procedure InitializeTVP(
  ATVP      : TTreeViewPresenter;
  AVST      : TVirtualStringTree = nil;
  ASource   : IList = nil;
  ATemplate : IDataTemplate = nil;
  AFilter   : TFilterEvent = nil
);

procedure AddControlBinding(
        ABindingGroup   : TBindingGroup;
        ASource         : TObject;
  const ASourceProp     : string;
        AControl        : TControl;
  const AControlProp    : string = 'Text';
        AValidationRule : IValidationRule = nil
);
{$ENDIF}

function CreateDBGridView(
        AOwner      : TComponent;
        AParent     : TWinControl;
        ADataSource : TDataSource = nil;
  const AName       : string = ''
): TDBGridView;

{ Create standard VCL DB Grid. }

function CreateDBGrid(
        AOwner      : TComponent;
        AParent     : TWinControl;
        ADataSource : TDataSource = nil;
  const AName       : string = ''
): TDBGrid;

function CreateVirtualDBGrid(
        AOwner      : TComponent;
        AParent     : TWinControl;
        ADataSource : TDataSource = nil;
  const AName       : string = ''
): TVirtualDBGrid;

procedure FillObjectListWithContacts(
  AList  : TObjectList;
  ACount : Integer
);

procedure FillListWithContacts(
  AList  : IList;
  ACount : Integer
);

function CreateContactList(
  const ACount: Integer = 0
): IList<TContact>; overload;

function CreateRandomContact: TContact;

implementation

uses
  ImgList, Forms, Graphics, SysUtils, Rtti, Windows,

  DSharp.Core.Reflection, DSharp.Windows.ColumnDefinitions.ControlTemplate,

  DDuce.RandomData;

const
  DEFAULT_VST_SELECTIONOPTIONS = [
    { Prevent user from selecting with the selection rectangle in multiselect
      mode. }
//    toDisableDrawSelection,
    {  Entries other than in the main column can be selected, edited etc. }
    toExtendedFocus
    { Hit test as well as selection highlight are not constrained to the text
      of a node. }
//    toFullRowSelect,
    { Constrain selection to the same level as the selection anchor. }
//    toLevelSelectConstraint,
    { Allow selection, dragging etc. with the middle mouse button. This and
      toWheelPanning are mutual exclusive. }
//    toMiddleClickSelect,
    { Allow more than one node to be selected. }
//    toMultiSelect,
    {  Allow selection, dragging etc. with the right mouse button. }
//    toRightClickSelect,
    { Constrain selection to nodes with same parent. }
//    toSiblingSelectConstraint,
    { Center nodes vertically in the client area when scrolling into view. }
//    toCenterScrollIntoView
    { Simplifies draw selection, so a node's caption does not need to intersect
      with the selection rectangle. }
//    toSimpleDrawSelection
  ];
  DEFAULT_VST_MISCOPTIONS = [
    { Register tree as OLE accepting drop target }
//    toAcceptOLEDrop,
    { Show checkboxes/radio buttons. }
//    toCheckSupport,
    { Node captions can be edited. }
//    toEditable,
    { Fully invalidate the tree when its window is resized
      (CS_HREDRAW/CS_VREDRAW). }
//    toFullRepaintOnResize,
    { Use some special enhancements to simulate and support grid behavior. }
    toGridExtensions,
    { Initialize nodes when saving a tree to a stream. }
    toInitOnSave,
    { Tree behaves like TListView in report mode. }
//    toReportMode,
    { Toggle node expansion state when it is double clicked. }
    toToggleOnDblClick,
    { Support for mouse panning (wheel mice only). This option and
      toMiddleClickSelect are mutal exclusive, where panning has precedence. }
    toWheelPanning,
    { The tree does not allow to be modified in any way. No action is executed
      and node editing is not possible. }
//    toReadOnly,
    { When set then GetNodeHeight will trigger OnMeasureItem to allow variable
      node heights. }
    toVariableNodeHeight
    { Start node dragging by clicking anywhere in it instead only on the
      caption or image. Must be used together with toDisableDrawSelection. }
//    toFullRowDrag,
    { Allows changing a node's height via mouse. }
//    toNodeHeightResize,
    { Allows to reset a node's height to FDefaultNodeHeight via a double click. }
//    toNodeHeightDblClickResize,
    { Editing mode can be entered with a single click }
//    toEditOnClick,
    { Editing mode can be entered with a double click }
//    toEditOnDblClick
  ];
  DEFAULT_VST_PAINTOPTIONS = [
    { Avoid drawing the dotted rectangle around the currently focused node. }
    toHideFocusRect,
    { Selected nodes are drawn as unselected nodes if the tree is unfocused. }
//    toHideSelection,
    { Track which node is under the mouse cursor. }
//    toHotTrack,
    { Paint tree as would it always have the focus }
    toPopupMode,
    { Use the background image if there's one. }
    toShowBackground,
    { Display collapse/expand buttons left to a node. }
    toShowButtons,
    { Show the dropmark during drag'n drop operations. }
    toShowDropmark,
    { Display horizontal lines to simulate a grid. }
    toShowHorzGridLines,
    { Show static background instead of a tiled one. }
    toStaticBackground,
    { Show lines also at top level (does not show the hidden/internal root
      node). }
    toShowRoot,
    { Display tree lines to show hierarchy of nodes. }
    toShowTreeLines,
    { Display vertical lines (depending on columns) to simulate a grid. }
    toShowVertGridLines,
    { Draw UI elements (header, tree buttons etc.) according to the current
      theme if enabled (Windows XP+ only, application must be themed). }
    toThemeAware,
    { Enable alpha blending for ghosted nodes or those which are being
      cut/copied. }
    toUseBlendedImages,
    { Ghosted images are still shown as ghosted if unfocused (otherwise they
      become non-ghosted images). }
//    toGhostedIfUnfocused,
    { Display vertical lines over the full client area, not only the space
      occupied by nodes. This option only has an effect if toShowVertGridLines
      is enabled too. }
//    toFullVertGridLines,
    { Do not draw node selection, regardless of focused state. }
//    toAlwaysHideSelection,
    { Enable alpha blending for node selections. }
    toUseBlendedSelection,
    { Show simple static background instead of a tiled one. }
    toStaticBackground
    { Display child nodes above their parent. }
//    toChildrenAbove,
    { Draw the tree with a fixed indent. }
//    toFixedIndent,
    { Use the explorer theme if run under Windows Vista (or above). }
//    toUseExplorerTheme
    { Do not show tree lines if theming is used. }
//    toHideTreeLinesIfThemed,
    { Draw nodes even if they are filtered out. }
//    toShowFilteredNodes
  ];
  DEFAULT_VST_HEADEROPTIONS = [
    { Adjust a column so that the header never exceeds the client width of the
      owner control. }
    hoAutoResize,
    { Resizing columns with the mouse is allowed. }
    hoColumnResize,
    { Allows a column to resize itself to its largest entry. }
    hoDblClickResize,
    { Dragging columns is allowed. }
//    hoDrag,
    { Header captions are highlighted when mouse is over a particular column. }
//    hoHotTrack,
    { Header items with the owner draw style can be drawn by the application
      via event. }
//    hoOwnerDraw,
    { Header can only be dragged horizontally. }
    hoRestrictDrag,
    { Show application defined header hint. }
    hoShowHint,
    { Show header images. }
    hoShowImages,
    { Allow visible sort glyphs. }
    hoShowSortGlyphs,
    { Distribute size changes of the header to all columns, which are sizable
      and have the coAutoSpring option enabled. hoAutoResize must be enabled
      too. }
    hoAutoSpring,
    { Fully invalidate the header (instead of subsequent columns only) when a
      column is resized. }
//    hoFullRepaintOnResize,
    { Disable animated resize for all columns. }
    hoDisableAnimatedResize,
    { Allow resizing header height via mouse. }
//    hoHeightResize,
    { Allow the header to resize itself to its default height. }
//    hoHeightDblClickResize
    { Header is visible. }
    hoVisible
  ];
  DEFAULT_VST_STRINGOPTIONS = [
    { If set then the caption is automatically saved with the tree node,
      regardless of what is saved in the user data. }
//    toSaveCaptions,
    { Show static text in a caption which can be differently formatted than the
      caption but cannot be edited. }
//    toShowStaticText,
    { Automatically accept changes during edit if the user finishes editing
      other then VK_RETURN or ESC. If not set then changes are cancelled. }
    toAutoAcceptEditChange
  ];
  DEFAULT_VST_ANIMATIONOPTIONS = [
    { Expanding and collapsing a node is animated (quick window scroll). }
//    toAnimatedToggle,
    { Do some advanced animation effects when toggling a node. }
//    toAdvancedAnimatedToggle
  ];
  DEFAULT_VST_AUTOOPTIONS = [
    { Expand node if it is the drop target for more than a certain time. }
    toAutoDropExpand,
    { Nodes are expanded (collapsed) when getting (losing) the focus. }
    toAutoExpand,
    { Scroll if mouse is near the border while dragging or selecting. }
    toAutoScroll,
    { Scroll as many child nodes in view as possible after expanding a node. }
    toAutoScrollOnExpand,
    { Sort tree when Header.SortColumn or Header.SortDirection change or sort
      node if child nodes are added. }
    toAutoSort,
    { Large entries continue into next column(s) if there's no text in them
      (no clipping). }
//    toAutoSpanColumns,
    { Checkstates are automatically propagated for tri state check boxes. }
    toAutoTristateTracking,
    { Node buttons are hidden when there are child nodes, but all are invisible.}
//    toAutoHideButtons,
    { Delete nodes which where moved in a drag operation (if not directed
      otherwise). }
    toAutoDeleteMovedNodes,
    { Disable scrolling a node or column into view if it gets focused. }
//    toDisableAutoscrollOnFocus,
    { Change default node height automatically if the system's font scale is
      set to big fonts. }
    toAutoChangeScale,
    { Frees any child node after a node has been collapsed (HasChildren flag
      stays there). }
//    toAutoFreeOnCollapse,
    { Do not center a node horizontally when it is edited. }
    toDisableAutoscrollOnEdit,
    { When set then columns (if any exist) will be reordered from lowest index
      to highest index and vice versa when the tree's bidi mode is changed. }
    toAutoBidiColumnOrdering
  ];

function CreateLogTree(AOwner : TComponent; AParent : TWinControl)
  : TLogTree;
var
  VLT : TLogTree;
begin
  VLT                    := TLogTree.Create(AOwner);
  VLT.AlignWithMargins   := True;
  VLT.BorderStyle        := bsNone;
  VLT.Parent             := AParent;
  VLT.Align              := alClient;
  VLT.ShowImages         := True;
  VLT.Header.Options     := VLT.Header.Options + [hoAutoSpring];
  Result := VLT;
end;

function CreateInspector(AOwner : TComponent; AParent : TWinControl;
  AObject : TPersistent): TPropertyInspector;
var
  PI : TPropertyInspector;
begin
  PI                  := TPropertyInspector.Create(AOwner);
  PI.AlignWithMargins := True;
  PI.Parent           := AParent;
  PI.BorderStyle      := bsSingle;
  PI.PropKinds        := PI.PropKinds + [pkReadOnly];
  PI.Align            := alClient;
  PI.Splitter         := PI.Width div 2;
  if Assigned(AObject) then
  begin
    PI.Add(AObject);
    PI.UpdateItems;
  end;
  Result := PI;
end;

function CreateVST(AOwner: TComponent; AParent: TWinControl; const AName: string)
  : TVirtualStringTree;
var
  VST : TVirtualStringTree;
begin
  VST          := TVirtualStringTree.Create(AOwner);
  VST.AlignWithMargins := True;
  VST.Parent   := AParent;
  VST.HintMode := hmTooltip;
  VST.Align    := alClient;
  VST.DrawSelectionMode := smBlendedRectangle;
  VST.Header.Height := 18;
  VST.Header.Options               := DEFAULT_VST_HEADEROPTIONS;
  VST.TreeOptions.SelectionOptions := DEFAULT_VST_SELECTIONOPTIONS;
  VST.TreeOptions.MiscOptions      := DEFAULT_VST_MISCOPTIONS;
  VST.TreeOptions.PaintOptions     := DEFAULT_VST_PAINTOPTIONS;
  VST.TreeOptions.StringOptions    := DEFAULT_VST_STRINGOPTIONS;
  VST.TreeOptions.AnimationOptions := DEFAULT_VST_ANIMATIONOPTIONS;
  VST.TreeOptions.AutoOptions      := DEFAULT_VST_AUTOOPTIONS;
  Result := VST;
end;

{$IFDEF DSHARP}
function CreateTVP(AOwner: TComponent; AVST: TVirtualStringTree;
  ASource: IList; ATemplate: IDataTemplate; AFilter: TFilterEvent;
  const AName: string): TTreeViewPresenter;
var
  TVP: TTreeViewPresenter;
begin
  TVP := TTreeViewPresenter.Create(AOwner);
  InitializeTVP(TVP, AVST, ASource, ATemplate, AFilter);
  Result := TVP;
end;

procedure InitializeTVP(ATVP: TTreeViewPresenter; AVST: TVirtualStringTree;
  ASource: IList; ATemplate: IDataTemplate; AFilter: TFilterEvent);
var
  P : TRttiProperty;
  C : TRttiContext;
begin
  if Assigned(ASource) then // auto create column definitions
  begin
    for P in C.GetType(ASource.ItemType).GetProperties do
    begin
      with ATVP.ColumnDefinitions.Add(P.Name) do
        ValuePropertyName := P.Name;
    end;
  end;
  ATVP.TreeView := AVST;
  ATVP.SyncMode := False;
  ATVP.UseColumnDefinitions := True;
  ATVP.ListMode             := True;
  ATVP.View.ItemsSource     := ASource;
  if Assigned(ATemplate) then
    ATVP.View.ItemTemplate :=
      TColumnDefinitionsControlTemplate.Create(ATVP.ColumnDefinitions);
  if Assigned(AFilter) then
    ATVP.View.Filter.Add(AFilter);
end;

procedure AddControlBinding(ABindingGroup: TBindingGroup; ASource: TObject;
  const ASourceProp: string; AControl: TControl; const AControlProp: string;
  AValidationRule : IValidationRule);
begin
  with ABindingGroup.Bindings.Add do
  begin
    Source                := ASource;
    SourcePropertyName    := ASourceProp;
    Target                := AControl;
    TargetPropertyName    := AControlProp;
    BindingMode           := bmTwoWay;

    NotifyOnSourceUpdated := False;
    NotifyOnTargetUpdated := False;
    if Assigned(AValidationRule) then
      ValidationRules.Add(AValidationRule);
  end;
end;
{$ENDIF}

function CreateDBGridView(AOwner: TComponent; AParent: TWinControl;
  ADataSource: TDataSource; const AName: string): TDBGridView;
var
  GV: TDBGridView;
begin
  GV                  := TDBGridView.Create(AOwner);
  GV.Header.Flat      := False;
  GV.AlignWithMargins := True;
  GV.Parent           := AParent;
  GV.Align            := alClient;
  GV.CursorKeys       := GV.CursorKeys + [gkReturn];
  GV.GridStyle        := GV.GridStyle + [gsDotLines];
  GV.ColumnsFullDrag  := True;
  GV.DoubleBuffered   := True;
  GV.CheckBoxes       := True;
  GV.ShowFocusRect    := False;
  GV.CheckStyle       := csFlat;
  GV.ColumnClick      := True;
  GV.ShowIndicator    := False;
  //GV.ShowIndicator    := True;
  GV.DataSource       := ADataSource;
  GV.AutoSizeCols;
  Result := GV;
end;

function CreateDBGrid(AOwner: TComponent; AParent: TWinControl;
  ADataSource: TDataSource; const AName: string): TDBGrid;
var
  DBG: TDBGrid;
begin
  DBG                  := TDBGrid.Create(AOwner);
  DBG.AlignWithMargins := True;
  DBG.Parent           := AParent;
  DBG.Align            := alClient;
  DBG.DataSource       := ADataSource;
  Result := DBG;
end;

function CreateVirtualDBGrid(AOwner: TComponent; AParent: TWinControl;
  ADataSource: TDataSource = nil; const AName: string = ''): TVirtualDBGrid;
var
  VDBG: TVirtualDBGrid;
begin
  VDBG                      := TVirtualDBGrid.Create(AOwner);
  VDBG.AlignWithMargins     := True;
  VDBG.Parent               := AParent;
  VDBG.Align                := alClient;
  VDBG.DBOptions.DataSource := ADataSource;
  VDBG.DBOptions.AdvOptions := VDBG.DBOptions.AdvOptions - [aoStrippedRows];
  Result := VDBG;
end;

procedure FillObjectListWithContacts(AList: TObjectList; ACount : Integer);
var
  C : TContact;
//  P : TPhone;
  I : Integer;
begin
  if Assigned(AList) then
  begin
    AList.Clear;
    for I := 0 to ACount - 1 do
    begin
      C := TContact.Create;
      with C do
      begin
        FirstName   := RandomData.FirstName(gnMale);
        LastName    := RandomData.LastName;
        CompanyName := RandomData.CompanyName;
        Email       := RandomData.Email(FirstName, LastName);
        Address     := RandomData.Address;
        Number      := RandomData.Number(100);
//        if RandomData.Letter = 'a' then
//        begin
//          P := TPhone.Create;
//          P.Phone := RandomData.NumberString(8);
//          P.Kind  := RandomData.City;
//          C.Phones.Add(P);
//          P := TPhone.Create;
//          P.Phone := RandomData.NumberString(8);
//          P.Kind  := RandomData.City;
//          C.Phones.Add(P);
//          P := TPhone.Create;
//          P.Phone := RandomData.NumberString(8);
//          P.Kind  := RandomData.City;
//          C.Phones.Add(P);
//        end;
      end;
      AList.Add(C);
    end;
  end;
end;

procedure FillListWithContacts(AList: IList; ACount: Integer);
var
  I : Integer;
begin
  if Assigned(AList) then
  begin
    AList.Clear;
    for I := 0 to ACount - 1 do
    begin
      AList.Add(CreateRandomContact);
    end;
  end;
end;

function CreateContactList(const ACount: Integer): IList<TContact>;
begin
  Result := TContacts.Create;
  FillListWithContacts(Result.AsList, ACount);
end;

function CreateRandomContact: TContact;
var
  C: TContact;
begin
  C := TContact.Create;
  with C do
  begin
    FirstName   := RandomData.FirstName;
    LastName    := RandomData.LastName;
    CompanyName := RandomData.CompanyName;
    Email       := RandomData.Email(FirstName, LastName);
    Address     := RandomData.Address;
    Number      := RandomData.Number(100);
    BirthDate   := RandomData.BirthDate(1928, 1987);
    Country     := 'USA';
    Active      := RandomData.Bool;
//        if RandomData.Letter = 'a' then
//        begin
//          P := TPhone.Create;
//          P.Phone := RandomData.NumberString(8);
//          P.Kind  := RandomData.City;
//          C.Phones.Add(P);
//          P := TPhone.Create;
//          P.Phone := RandomData.NumberString(8);
//          P.Kind  := RandomData.City;
//          C.Phones.Add(P);
//          P := TPhone.Create;
//          P.Phone := RandomData.NumberString(8);
//          P.Kind  := RandomData.City;
//          C.Phones.Add(P);
//        end;
  end;
  Result := C;
end;

end.
