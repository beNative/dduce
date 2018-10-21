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

{ PVirtualNode = ^TVirtualNode
  TVirtualNode = record
  ...
    Data: record end; // Data gets assigned user defined data
  ...
  end;

  TVTNode<T> is a parameterizable node class intended to be used as the data
  member of each virtual node in the treeviewer.
  T is the user defined type of which each TVTNode instance has a reference to.

  TVTNode<T> has following members that allow us to build a treeview from code:
    Data: T;  // holds the user defined data with all needed information to show
              // in the tree.
    VNode: PVirtualNode; // points back to the native tree node associated with
                         // the data.
    Nodes: IList<TVTNode<T>>; // list of child nodes for the current node

  At lease following event handlers need to be implemented by the treeview.
  The user defined data is accessed from the event handlers using the
  GetNodeData member of TVirtualStringTree and the Sender and Node parameters
  from the tree's event handler:
      VTNode := Sender.GetNodeData<TVTNode<TMyData>>(Node);

    OnGetText
      Assign CellText using the user defined data.

    OnFreeNode
      Free the TVTNode object.
}

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  VirtualTrees,

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
  private
    FTree: TVirtualStringTree;
    FVTRoot : TVTNode<TMyData>;

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

  public
    procedure BuildTree;
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Factories.VirtualTrees;

{$REGION 'construction and destruction'}
procedure TfrmVTNode.AfterConstruction;
begin
  inherited AfterConstruction;
  FTree := TVirtualStringTreeFactory.CreateTree(Self, Self);
  FTree.OnFreeNode := FTreeFreeNode;
  FTree.OnGetText  := FTreeGetText;
  BuildTree;
end;
{$ENDREGION}

{$REGION 'event handlers'}
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
  LVTNode := Sender.GetNodeData<TVTNode<TMyData>>(Node);
  CellText := LVTNode.Data.Text;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmVTNode.BuildTree;
var
  LData   : TMyData;
  LVTNode : TVTNode<TMyData>;
begin
  LData := TMyData.Create('Root');
  FVTRoot := TVTNode<TMyData>.Create(FTree, LData);
  LData := TMyData.Create('Sub1');
  FVTRoot.Add(LData);
  LData := TMyData.Create('Sub2');
  FVTRoot.Add(LData);
  LData := TMyData.Create('Sub3');
  LVTNode := FVTRoot.Add(LData);
  LVTNode.CheckType := ctCheckBox;
  LData := TMyData.Create('Sub4');
  LVTNode := FVTRoot.Add(LData);
  LVTNode.Add(TMyData.Create('SubSub1'));
end;
{$ENDREGION}

{$REGION 'TMyData'}
constructor TMyData.Create(const AText: string);
begin
  FText := AText;
end;
{$ENDREGION}

end.
