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

unit Demo.DDuce.VTNode;

{ Example showing how to build a treeview using TVTNode<T> and a user defined
  data class. }

interface

{$REGION 'Documentation'}
{ PVirtualNode = ^TVirtualNode
  TVirtualNode = record
  ...
    Data: record end; // Data gets assigned user defined data
  ...
  end;

  TVTNode<T> is a parameterizable node class intended to be used as the Data
  member of each virtual node in the virtual treeview.
  T is the user defined type of which each TVTNode instance has a reference to.

  TVTNode<T> has following members that allow us to build a treeview from code:
    Data: T;  // holds the user defined data with all needed information to show
              // in the tree.
    VNode: PVirtualNode; // points back to the native tree node associated with
                         // the data.
    Nodes: IList<TVTNode<T>>; // list of child nodes for the current node

  At least following two event handlers need to be implemented by the treeview.
    OnGetText
      Assign CellText using the user defined data.

    OnFreeNode
      Free the TVTNode object.

  The user defined data is accessed from the event handlers using the
  GetNodeData member of TVirtualStringTree and the Sender and Node parameters
  from the tree's event handler:
      VTNode := Sender.GetNodeData<TVTNode<TMyData>>(Node);
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
    actDeleteNode      : TAction;
    actSetNodeText     : TAction;
    btnAddChild        : TButton;
    btnDeleteNode      : TButton;
    btnSetNodeText     : TButton;
    pnlMain            : TPanel;
    pnlObjectInspector : TPanel;
    pnlTree            : TPanel;
    actBuildTree       : TAction;
    actFullExpand      : TAction;
    actFullCollapse    : TAction;
    btnBuildTree       : TButton;
    btnFullExpand      : TButton;
    btnFullCollapse    : TButton;
    {$ENDREGION}

    procedure actDeleteNodeExecute(Sender: TObject);
    procedure actAddChildExecute(Sender: TObject);
    procedure actSetNodeTextExecute(Sender: TObject);
    procedure actBuildTreeExecute(Sender: TObject);
    procedure actFullExpandExecute(Sender: TObject);
    procedure actFullCollapseExecute(Sender: TObject);

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
    procedure BuildTree;

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

{$REGION 'public methods'}
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
{$ENDREGION}

end.
