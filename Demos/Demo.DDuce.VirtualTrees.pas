{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Demo.DDuce.VirtualTrees;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  Vcl.ExtCtrls,

  zObjInspector,

  VirtualTrees;

type
  TfrmVirtualTrees = class(TForm)
    {$REGION 'designer controls'}
    pgcMain              : TPageControl;
    tsVSTTree            : TTabSheet;
    tsVSTGrid            : TTabSheet;
    tsVSTList            : TTabSheet;
    tsVSTTreeList        : TTabSheet;
    tsVSTTreeGrid        : TTabSheet;
    tsVST                : TTabSheet;
    tsSettings           : TTabSheet;
    pnlOptions           : TGridPanel;
    pnlVST               : TPanel;
    pnlVSTTree           : TPanel;
    pnlVSTGrid           : TPanel;
    pnlVSTList           : TPanel;
    pnlVSTTreeList       : TPanel;
    pnlVSTTreeGrid       : TPanel;
    pnlVSTHeader         : TPanel;
    pnlVSTTreeHeader     : TPanel;
    pnlVSTGridHeader     : TPanel;
    pnlVSTListHeader     : TPanel;
    pnlVSTTreeListHeader : TPanel;
    pnlVSTTreeGridHeader : TPanel;
    {$ENDREGION}

  private
    FVST           : TVirtualStringTree;
    FVSTTree       : TVirtualStringTree;
    FVSTList       : TVirtualStringTree;
    FVSTGrid       : TVirtualStringTree;
    FVSTTreeList   : TVirtualStringTree;
    FVSTTreeGrid   : TVirtualStringTree;
    FOIVST         : TzObjectInspector;
    FOIVSTTree     : TzObjectInspector;
    FOIVSTList     : TzObjectInspector;
    FOIVSTGrid     : TzObjectInspector;
    FOIVSTTreeList : TzObjectInspector;
    FOIVSTTreeGrid : TzObjectInspector;

    procedure FVirtualStringTreeInitNode(
      Sender : TBaseVirtualTree;
      ParentNode,
      Node : PVirtualNode;
      var InitialStates : TVirtualNodeInitStates
    );

    procedure FVirtualStringTreeFreeNode(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );

    procedure FVirtualStringTreeGetText(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      TextType     : TVSTTextType;
      var CellText : string
    );

  protected
    procedure InitializeTree(AVST: TVirtualStringTree);

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Factories.VirtualTrees, DDuce.Factories.zObjInspector,

  Demo.Factories, Demo.Contact;

{$REGION 'construction and destruction'}
procedure TfrmVirtualTrees.AfterConstruction;
begin
  inherited AfterConstruction;
  FVST         := TVirtualStringTreeFactory.Create(Self, tsVST);
  FVSTTree     := TVirtualStringTreeFactory.CreateTree(Self, tsVSTTree);
  FVSTGrid     := TVirtualStringTreeFactory.CreateGrid(Self, tsVSTGrid);
  FVSTList     := TVirtualStringTreeFactory.CreateList(Self, tsVSTList);
  FVSTTreeList := TVirtualStringTreeFactory.CreateTreeList(Self, tsVSTTreeList);
  FVSTTreeGrid := TVirtualStringTreeFactory.CreateTreeGrid(Self, tsVSTTreeGrid);

  InitializeTree(FVST);
  InitializeTree(FVSTTree);
  InitializeTree(FVSTGrid);
  InitializeTree(FVSTList);
  InitializeTree(FVSTTreeGrid);
  InitializeTree(FVSTTreeList);

  FOIVST := TzObjectInspectorFactory.Create(
    Self,
    pnlVST,
    FVST
  );

  FOIVSTTree := TzObjectInspectorFactory.Create(
    Self,
    pnlVSTTree,
    FVSTTree
  );
  FOIVSTGrid := TzObjectInspectorFactory.Create(
    Self,
    pnlVSTGrid,
    FVSTGrid
  );
  FOIVSTList := TzObjectInspectorFactory.Create(
    Self,
    pnlVSTList,
    FVSTList
  );
  FOIVSTTreeList := TzObjectInspectorFactory.Create(
    Self,
    pnlVSTTreeList,
    FVSTTreeList
  );
  FOIVSTTreeGrid := TzObjectInspectorFactory.Create(
    Self,
    pnlVSTTreeGrid,
    FVSTTreeGrid
  );
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmVirtualTrees.FVirtualStringTreeFreeNode(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  Sender.GetNodeData<TContact>(Node).Free;
end;

procedure TfrmVirtualTrees.FVirtualStringTreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  LContact: TContact;
begin
  LContact := Sender.GetNodeData<TContact>(Node);
  if Column = 0 then
    CellText := LContact.FirstName
  else if Column = 1 then
    CellText := LContact.Lastname
  else if Column = 2 then
    CellText := LContact.Email
  else if Column = 3 then
    CellText := LContact.CompanyName
  else if Column = 4 then
    CellText := LContact.Address
  else if Column = 5 then
    CellText := LContact.Number.ToString
  else
  begin
    CellText := '';
  end;
end;

procedure TfrmVirtualTrees.FVirtualStringTreeInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  Node.SetData(TDemoFactories.CreateRandomContact);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmVirtualTrees.InitializeTree(AVST: TVirtualStringTree);
begin
  with AVST do
  begin
    OnInitNode                := FVirtualStringTreeInitNode;
    OnGetText                 := FVirtualStringTreeGetText;
    OnFreeNode                := FVirtualStringTreeFreeNode;
    with Header.Columns.Add do
    begin
      Color    := clWhite;
      MaxWidth := 200;
      MinWidth := 100;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coResizable, coShowDropMark, coVisible, coSmartResize, coAllowFocus,
        coEditable];
      Position := 0;
      Width    := 200;
      Text := 'FirstName';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 200;
      MinWidth := 100;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
        coSmartResize, coAllowFocus, coEditable];
      Position := 1;
      Width    := 100;
      Text := 'LastName';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 200;
      MinWidth := 100;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
        coSmartResize, coAllowFocus, coEditable];
      Position := 2;
      Width    := 100;
      Text := 'Email';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 400;
      MinWidth := 100;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
        coSmartResize, coAllowFocus, coEditable];
      Position := 3;
      Width    := 100;
      Text := 'CompanyName';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 400;
      MinWidth := 100;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
        coSmartResize, coAllowFocus, coEditable];
      Position := 4;
      Width    := 100;
      Text := 'Address';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 200;
      MinWidth := 70;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
        coSmartResize, coAllowFocus, coEditable];
      Position := 5;
      Width    := 70;
      Text := 'Number';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 200;
      MinWidth := 70;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
        coSmartResize, coAllowFocus, coEditable];
      Position := 6;
      Width    := 200;
      Text := 'Active';
    end;
    RootNodeCount := 1000;
    Header.AutoFitColumns;
    Header.Options := Header.Options + [hoOwnerDraw];
  end;
end;
{$ENDREGION}

end.
