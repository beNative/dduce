{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

  First we need a root node in order to build the tree viewer. In this
  example this is declared as follows:
     FVTRoot: TVTNode<TMyData>;

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
      VTNode := Sender.GetNodeData<TVTNode<TMyData>>(Node);

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

  DDuce.Components.VirtualTrees.Node;

type
  TMyData = class
  private
    FText : string;

  public
    constructor Create(const AText: string);

    property Text: string
      read FText write FText;
  end;

  TfrmVTNode = class(TForm)
    {$REGION 'designer controls'}
    aclMain            : TActionList;
    actAddChild        : TAction;
    actBuildTree       : TAction;
    actDeleteNode      : TAction;
    actFullCollapse    : TAction;
    actFullExpand      : TAction;
    actMoveDown        : TAction;
    actMoveUp          : TAction;
    actSetNodeText     : TAction;
    btnAddChild        : TButton;
    btnBuildTree       : TButton;
    btnDeleteNode      : TButton;
    btnFullCollapse    : TButton;
    btnFullExpand      : TButton;
    btnMoveDown        : TButton;
    btnMoveUp          : TButton;
    btnSetNodeText     : TButton;
    pnlMain            : TPanel;
    pnlObjectInspector : TPanel;
    pnlTree            : TPanel;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actDeleteNodeExecute(Sender: TObject);
    procedure actAddChildExecute(Sender: TObject);
    procedure actSetNodeTextExecute(Sender: TObject);
    procedure actBuildTreeExecute(Sender: TObject);
    procedure actFullExpandExecute(Sender: TObject);
    procedure actFullCollapseExecute(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    {$ENDREGION}

  private
    FTree            : TVirtualStringTree;
    FVTRoot          : TVTNode<TMyData>;
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
    function CanMoveUp: Boolean;
    function CanMoveDown: Boolean;

    procedure BuildTree;
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;

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

{$REGION 'action handlers'}
procedure TfrmVTNode.actAddChildExecute(Sender: TObject);
var
  LVTNode : TVTNode<TMyData>;
begin
  LVTNode := FTree.GetNodeData<TVTNode<TMyData>>(FTree.FocusedNode);
  if Assigned(LVTNode) then
    LVTNode.Add(TMyData.Create('Child node'));
end;

procedure TfrmVTNode.actBuildTreeExecute(Sender: TObject);
begin
  BuildTree;
end;

procedure TfrmVTNode.actDeleteNodeExecute(Sender: TObject);
begin
  FTree.DeleteNode(FTree.FocusedNode);
end;

procedure TfrmVTNode.actFullCollapseExecute(Sender: TObject);
begin
  FTree.FullCollapse;
end;

procedure TfrmVTNode.actFullExpandExecute(Sender: TObject);
begin
  FTree.FullExpand;
end;

procedure TfrmVTNode.actMoveDownExecute(Sender: TObject);
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

procedure TfrmVTNode.actMoveUpExecute(Sender: TObject);
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
  LVTNode : TVTNode<TMyData>;
begin
  LVTNode := FTree.GetNodeData<TVTNode<TMyData>>(FTree.FocusedNode);
  if Assigned(LVTNode) then
  begin
    LVTNode.Data.Text := 'New text';
    FTree.InvalidateNode(LVTNode.VNode);
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmVTNode.FTreeFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  LVTNode : TVTNode<TMyData>;
begin
  LVTNode := Sender.GetNodeData<TVTNode<TMyData>>(Node);
  if Assigned(LVTNode) then
  begin
    Logger.SendObject('VTNode', LVTNode);
    Logger.SendObject('Data', LVTNode.Data);
  end;
end;

procedure TfrmVTNode.FTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  LVTNode : TVTNode<TMyData>;
begin
  LVTNode := Sender.GetNodeData<TVTNode<TMyData>>(Node);
  LVTNode.Free;
end;

procedure TfrmVTNode.FTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  LVTNode : TVTNode<TMyData>;
begin
  LVTNode  := Sender.GetNodeData<TVTNode<TMyData>>(Node);
  CellText := LVTNode.Data.Text;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmVTNode.UpdateActions;
begin
  inherited UpdateActions;
  actMoveUp.Enabled   := CanMoveUp;
  actMoveDown.Enabled := CanMoveDown;
end;

procedure TfrmVTNode.BuildTree;
var
  LData   : TMyData;
  LVTNode : TVTNode<TMyData>;
  I       : Integer;
  J       : Integer;
  S       : string;
begin
  LData := TMyData.Create('Root node');
  FVTRoot := TVTNode<TMyData>.Create(FTree, LData);
  FVTRoot.Text := 'Root node';
  for I := 1 to 100 do
  begin
    S := Format('Child %d', [I]);
    LVTNode := FVTRoot.Add(TMyData.Create(S));
    for J := 1 to 100 do
    begin
      S := Format('Child %d.%d', [I, J]);
      LVTNode.Add(TMyData.Create(S));
    end;
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
