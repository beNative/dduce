{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Demo.DDuce.VTNode;

{ Example showing how to build a treeview using TVTNode<T> and a user defined
  data class. }

interface

{$REGION 'Documentation'}
{
  The virtual tree component consists of nodes which are declared as pointers
  (PVirtualNode) to a record type (TVirtualNode). User defined data of each
  node needs to be provided on demand (a.k.a. the virtual paradigm). The tree
  is instrumented by handling a set of events that are called when the tree
  is initialized or redrawn.

    PVirtualNode = ^TVirtualNode
    TVirtualNode = record
    ...
      Data: record end; // Data gets assigned user defined data
    ...
    end;

  TVTNode<T> is a parameterizable node class intended to be used as the Data
  member of each virtual node in a virtual treeview control.
  T is the user defined type of which each TVTNode instance has a reference to.

  TVTNode<T> has following members that allow us to build a treeview from code:
    Data: T;
       The user defined object with all data that needs to be shown. In this
       demo this is TMyData.
    VNode: PVirtualNode;
       References the tree node associated with the data.
    Nodes: IList<TVTNode<T>>;
       Holds children of the current node.

  The node type can be simplified by defining a type alias:
    type
      TMyNode = TVTNode<TMyData>;

  First we need a root node in order to build the tree viewer.
  In this example this is declared as follows:
    FRootNode: TMyNode;

  The tree control itself is declared as follows:
    FTree: TVirtualStringTree;

  At least the following event handlers need to be implemented by the treeview
  control:
    OnGetText
      Assign CellText using the user defined data (FTreeGetText).
    OnFreeNode
      Free the TVTNode object (FTreeFreeNode).
    OnFocusChanged
      Retrieve the data of the focused node (FTreeFocusChanged).

  The user defined data is accessed from the event handlers using the
  GetNodeData member of TVirtualStringTree and the Sender and Node parameters
  of the tree's event handlers:
      VTNode := Sender.GetNodeData<TMyNode>(Node);

  This demo demonstrates how to setup the tree with data and how to manipulate
  both nodes and the corresponding data in the treeview with the following
  actions:
    actBuildTree
    actAddChild
    actDeleteNode
    actMoveUp/actMoveDown
    actFullExpand/actFullCollapse
    actSetNodeText
}
{$ENDREGION}

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.ActnList, Vcl.StdCtrls,

  VirtualTrees,

  zObjInspector,

  DDuce.Components.VirtualTrees.Node,   DDuce.Components.SectionTree;

type
  TMyData = class
  private
    FText : string;

  public
    constructor Create(const AText: string);

    property Text: string
      read FText write FText;
  end;

type
  TMyNode = TVTNode<TMyData>;

type
  TfrmVTNode = class(TForm)
    {$REGION 'designer controls'}
    aclMain             : TActionList;
    actAddChildNode     : TAction;
    actBuildTree        : TAction;
    actCollapseNode     : TAction;
    actDeleteNode       : TAction;
    actExpandNode       : TAction;
    actFullCollapseNode : TAction;
    actFullCollapseTree : TAction;
    actFullExpandNode   : TAction;
    actFullExpandTree   : TAction;
    actMoveDownNode     : TAction;
    actMoveUpNode       : TAction;
    actSetNodeText      : TAction;
    btnAddChild         : TButton;
    btnBuildTree        : TButton;
    btnCollapseNode     : TButton;
    btnDeleteNode       : TButton;
    btnExpandNode       : TButton;
    btnFullCollapse     : TButton;
    btnFullCollapseNode : TButton;
    btnFullExpand       : TButton;
    btnFullExpandNode   : TButton;
    btnMoveDown         : TButton;
    btnMoveUp           : TButton;
    btnSetNodeText      : TButton;
    pnlMain             : TPanel;
    pnlObjectInspector  : TPanel;
    pnlTree             : TPanel;
    actSelectFirstChild: TAction;
    btnFocusFirstChild: TButton;
    actSelectLastChild: TAction;
    actSelectNextSibling: TAction;
    actSelectPreviousSibling: TAction;
    btnSelectLastChild: TButton;
    btnSelectNextSibling: TButton;
    btnSelectNextSibling1: TButton;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actAddChildNodeExecute(Sender: TObject);
    procedure actBuildTreeExecute(Sender: TObject);
    procedure actCollapseNodeExecute(Sender: TObject);
    procedure actDeleteNodeExecute(Sender: TObject);
    procedure actExpandNodeExecute(Sender: TObject);
    procedure actFullCollapseNodeExecute(Sender: TObject);
    procedure actFullCollapseTreeExecute(Sender: TObject);
    procedure actFullExpandNodeExecute(Sender: TObject);
    procedure actFullExpandTreeExecute(Sender: TObject);
    procedure actMoveDownNodeExecute(Sender: TObject);
    procedure actMoveUpNodeExecute(Sender: TObject);
    procedure actSetNodeTextExecute(Sender: TObject);
    procedure actSelectFirstChildExecute(Sender: TObject);
    procedure actSelectLastChildExecute(Sender: TObject);
    procedure actSelectPreviousSiblingExecute(Sender: TObject);
    procedure actSelectNextSiblingExecute(Sender: TObject);
    {$ENDREGION}

  private
    FTree            : TVirtualStringTree;
    FRootNode        : TMyNode;
    FObjectInspector : TzObjectInspector;

    procedure FTreeFreeNode(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
    procedure FTreeGetText(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      TextType     : TVSTTextType;
      var CellText : string
    );
    procedure FTreeFocusChanged(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    );

  protected
    function GetFocusedNode: TMyNode;

    function CanMoveUp: Boolean;
    function CanMoveDown: Boolean;
    function GetNode(const AVNode: PVirtualNode): TMyNode;
    procedure BuildTree;
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;

    property FocusedNode: TMyNode
      read GetFocusedNode;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Factories.VirtualTrees, DDuce.Factories.zObjInspector,
  DDuce.Logger, DDuce.Logger.Interfaces;

{$REGION 'TMyData'}
constructor TMyData.Create(const AText: string);
begin
  FText := AText;
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TfrmVTNode.AfterConstruction;
begin
  inherited AfterConstruction;
  Logger.Clear;
  FTree := TVirtualStringTreeFactory.CreateTree(Self, pnlTree);
  FTree.OnFreeNode     := FTreeFreeNode;
  FTree.OnGetText      := FTreeGetText;
  FTree.OnFocusChanged := FTreeFocusChanged;
  FObjectInspector     := TzObjectInspectorFactory.Create(
    Self,
    pnlObjectInspector,
    FTree
  );
  FObjectInspector.AlignWithMargins := True;
  BuildTree;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmVTNode.GetFocusedNode: TMyNode;
begin
  Result := FTree.GetNodeData<TMyNode>(FTree.FocusedNode);
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmVTNode.actAddChildNodeExecute(Sender: TObject);
begin
  if Assigned(FocusedNode) then
    FocusedNode.Add(TMyData.Create('Child node'));
end;

procedure TfrmVTNode.actBuildTreeExecute(Sender: TObject);
begin
  BuildTree;
end;

procedure TfrmVTNode.actCollapseNodeExecute(Sender: TObject);
begin
  if Assigned(FocusedNode) then
    FocusedNode.Collapse;
end;

procedure TfrmVTNode.actDeleteNodeExecute(Sender: TObject);
begin
  FTree.DeleteNode(FTree.FocusedNode);
end;

procedure TfrmVTNode.actExpandNodeExecute(Sender: TObject);
begin
  if Assigned(FocusedNode) then
    FocusedNode.Expand;
end;

procedure TfrmVTNode.actFullCollapseTreeExecute(Sender: TObject);
begin
  FTree.FullCollapse;
end;

procedure TfrmVTNode.actSelectFirstChildExecute(Sender: TObject);
begin
  if Assigned(FocusedNode) and Assigned(FocusedNode.FirstChildNode) then
    FocusedNode.FirstChildNode.Select;
end;

procedure TfrmVTNode.actSelectLastChildExecute(Sender: TObject);
begin
  if Assigned(FocusedNode) and Assigned(FocusedNode.LastChildNode) then
    FocusedNode.LastChildNode.Select;
end;

procedure TfrmVTNode.actSelectNextSiblingExecute(Sender: TObject);
begin
  if Assigned(FocusedNode) and Assigned(FocusedNode.NextSiblingNode) then
    FocusedNode.NextSiblingNode.Select;
end;

procedure TfrmVTNode.actSelectPreviousSiblingExecute(Sender: TObject);
begin
  if Assigned(FocusedNode) and Assigned(FocusedNode.PrevSiblingNode) then
    FocusedNode.PrevSiblingNode.Select;
end;

procedure TfrmVTNode.actFullCollapseNodeExecute(Sender: TObject);
begin
  if Assigned(FocusedNode) then
    FocusedNode.FullCollapse;
end;

procedure TfrmVTNode.actFullExpandTreeExecute(Sender: TObject);
begin
  FTree.FullExpand;
end;

procedure TfrmVTNode.actFullExpandNodeExecute(Sender: TObject);
begin
  if Assigned(FocusedNode) then
    FocusedNode.FullExpand;
end;

procedure TfrmVTNode.actMoveDownNodeExecute(Sender: TObject);
begin
  if Assigned(FTree.FocusedNode) then
  begin
    FTree.MoveTo(
      FTree.FocusedNode,
      FTree.FocusedNode.NextSibling,
      amInsertAfter,
      False
    );
  end;
end;

procedure TfrmVTNode.actMoveUpNodeExecute(Sender: TObject);
begin
  if Assigned(FTree.FocusedNode) then
  begin
    FTree.MoveTo(
      FTree.FocusedNode,
      FTree.FocusedNode.PrevSibling,
      amInsertBefore,
      False
    );
  end;
end;

procedure TfrmVTNode.actSetNodeTextExecute(Sender: TObject);
var
  LNode : TMyNode;
begin
  LNode := FocusedNode;
  if Assigned(LNode) then
  begin
    LNode.Data.Text := 'New text';
    FTree.InvalidateNode(LNode.VNode);
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmVTNode.FTreeFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  LNode : TMyNode;
begin
  LNode := GetNode(Node);
  if Assigned(LNode) then
  begin
    Logger.SendObject('VTNode', LNode);
    if Assigned(LNode.ParentNode) then
      Logger.SendObject('ParentNode', LNode.ParentNode);
    if Assigned(LNode.FirstChildNode) then
      Logger.SendObject('FirstChildNode', LNode.FirstChildNode);
    if Assigned(LNode.LastChildNode) then
      Logger.SendObject('LastChildNode', LNode.LastChildNode);
    if Assigned(LNode.NextSiblingNode) then
      Logger.SendObject('NextSiblingNode', LNode.NextSiblingNode);
    if Assigned(LNode.PrevSiblingNode) then
      Logger.SendObject('PrevSiblingNode', LNode.PrevSiblingNode);
    Logger.SendObject('Data', LNode.Data);
  end;
end;

procedure TfrmVTNode.FTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  GetNode(Node).Free;
end;

procedure TfrmVTNode.FTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
begin
  CellText :=  GetNode(Node).Data.Text;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TfrmVTNode.GetNode(const AVNode: PVirtualNode): TMyNode;
begin
  Result := FTree.GetNodeData<TMyNode>(AVNode);
end;

procedure TfrmVTNode.UpdateActions;
begin
  inherited UpdateActions;
  actMoveUpNode.Enabled   := CanMoveUp;
  actMoveDownNode.Enabled := CanMoveDown;
end;

procedure TfrmVTNode.BuildTree;
var
  LData : TMyData;
  LNode : TMyNode;
  I     : Integer;
  J     : Integer;
  S     : string;
begin
  FTree.BeginUpdate;
  try
    LData := TMyData.Create('Root node');
    FRootNode := TMyNode.Create(FTree, LData);
    FRootNode.Text := 'Root node';
    for I := 1 to 100 do
    begin
      S := Format('Child %d', [I]);
      LNode := FRootNode.Add(TMyData.Create(S));
        for J := 1 to 100 do
      begin
        S := Format('Child %d.%d', [I, J]);
          LNode.Add(TMyData.Create(S));
      end;
    end;
  finally
    FTree.EndUpdate;
  end;
end;

function TfrmVTNode.CanMoveDown: Boolean;
begin
  if Assigned(FTree.FocusedNode) then
    Result := Assigned(FTree.FocusedNode.NextSibling)
  else
    Result := False;
end;

function TfrmVTNode.CanMoveUp: Boolean;
begin
  if Assigned(FTree.FocusedNode) then
  begin
    Result := Assigned(FTree.FocusedNode.PrevSibling);
  end
  else
    Result := False;
end;
{$ENDREGION}

end.
