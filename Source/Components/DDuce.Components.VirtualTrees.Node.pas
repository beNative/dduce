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

{$I DDuce.inc}

unit DDuce.Components.VirtualTrees.Node;

interface

uses
  Spring, Spring.Collections,

  VirtualTrees;

{
  Documentation
    TVTNode is a type designed to be used as the data where each treenode in a
    treeview is pointing to.
    For any treenode (of type PVirtualNode) this can be obtained by the following
    method defined in TBaseVirtualStringTree:
          function GetNodeData<T>(pNode: PVirtualNode): T;

    type
      TMyData = class
        ...
      end;
}
type
  TVTNode<T> = class
  private
    FVNode      : PVirtualNode;
    FTree       : TCustomVirtualStringTree;
    FNodes      : Lazy<IList<TVTNode<T>>>;
    FData       : T;
    FText       : string;
    FHint       : string;
    FImageIndex : Integer;

  protected
    {$REGION 'property access methods'}
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    function GetLevel: Integer;
    function GetIndex: Integer;
    function GetImageIndex: Integer;
    procedure SetImageIndex(const Value: Integer);
    function GetHint: string;
    procedure SetHint(const Value: string);
    function GetCheckState: TCheckState;
    procedure SetCheckState(const Value: TCheckState);
    function GetCheckType: TCheckType;
    procedure SetCheckType(const Value: TCheckType);
    function GetVNode: PVirtualNode;
    procedure SetVNode(const Value: PVirtualNode);
    function GetData: T;
    procedure SetData(const Value: T);
    function GetNodes: IList<TVTNode<T>>;
    function GetCount: UInt32;
    function GetText: string; virtual;
    procedure SetText(const Value: string); virtual;
    {$ENDREGION}

  public
    constructor Create(
      ATree       : TCustomVirtualStringTree;
      const AData : T;
      const AText : string = ''
    ); overload; virtual;
    constructor Create(
      ATree       : TCustomVirtualStringTree;
      const AText : string = ''
    ); overload; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function Add(const AData: T): TVTNode<T>;

    property VNode: PVirtualNode
      read GetVNode write SetVNode;

    property Nodes: IList<TVTNode<T>>
      read GetNodes;

    property Count: UInt32
      read GetCount;

    property Data: T
      read GetData write SetData;

    property CheckState: TCheckState
      read GetCheckState write SetCheckState;

    property CheckType: TCheckType
      read GetCheckType write SetCheckType;

    property ImageIndex: Integer
     read GetImageIndex write SetImageIndex;

    property Index: Integer
      read GetIndex;

    property Level: Integer
      read GetLevel;

    property Text: string
      read GetText write SetText;

    property Hint: string
      read GetHint write SetHint;

    property Visible: Boolean
      read GetVisible write SetVisible;
  end;

implementation

uses
  System.SysUtils;

{$REGION 'construction and destruction'}
constructor TVTNode<T>.Create(ATree: TCustomVirtualStringTree; const AData: T;
  const AText: string);
begin
  FTree  := ATree;
  FData  := AData;
  FText  := AText;
end;

constructor TVTNode<T>.Create(ATree: TCustomVirtualStringTree; const AText: string);
begin
  FTree  := ATree;
  FData  := Default(T);
  FText  := AText;
end;

procedure TVTNode<T>.AfterConstruction;
begin
  inherited AfterConstruction;
  FNodes.Create(function: IList<TVTNode<T>>
    begin
      Result := TCollections.CreateList<TVTNode<T>>;
    end
  );
end;

procedure TVTNode<T>.BeforeDestruction;
begin
  if GetTypekind(T) = tkClass then
    FreeAndNil(FData);
  FTree  := nil;
  FNodes := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TVTNode<T>.GetCheckState: TCheckState;
begin
  Result := VNode.CheckState;
end;

procedure TVTNode<T>.SetCheckState(const Value: TCheckState);
begin
  VNode.CheckState := Value;
end;

function TVTNode<T>.GetCheckType: TCheckType;
begin
  Result := VNode.CheckType;
end;

procedure TVTNode<T>.SetCheckType(const Value: TCheckType);
begin
  if Assigned(VNode) then
    VNode.CheckType := Value;
end;

function TVTNode<T>.GetCount: UInt32;
begin
  Result := VNode.ChildCount;
end;

function TVTNode<T>.GetData: T;
begin
  Result := FData;
end;

procedure TVTNode<T>.SetData(const Value: T);
begin
  FData := Value;
end;

function TVTNode<T>.GetHint: string;
begin
  Result := FHint;
end;

procedure TVTNode<T>.SetHint(const Value: string);
begin
  FHint := Value;
end;

function TVTNode<T>.GetImageIndex: Integer;
begin
  Result := FImageIndex;
end;

procedure TVTNode<T>.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
end;

function TVTNode<T>.GetIndex: Integer;
begin
  Result := VNode.Index;
end;

function TVTNode<T>.GetLevel: Integer;
begin
  Result := FTree.GetNodeLevel(VNode);
end;

function TVTNode<T>.GetText: string;
begin
  Result := FText;
end;

procedure TVTNode<T>.SetText(const Value: string);
begin
  FText := Value;
end;

function TVTNode<T>.GetNodes: IList<TVTNode<T>>;
begin
  Result := FNodes.Value;
end;

function TVTNode<T>.GetVisible: Boolean;
begin
  Result := FTree.IsVisible[VNode];
end;

procedure TVTNode<T>.SetVisible(const Value: Boolean);
begin
  FTree.IsVisible[VNode] := Value;
end;

function TVTNode<T>.GetVNode: PVirtualNode;
begin
  Result := FVNode;
end;

procedure TVTNode<T>.SetVNode(const Value: PVirtualNode);
begin
  FVNode := Value;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TVTNode<T>.Add(const AData: T): TVTNode<T>;
var
  LVTNode : TVTNode<T>;
  LVNode  : PVirtualNode;
begin
  if FVNode = nil then // create root node if it does not exist
    FVNode := FTree.AddChild(nil, Self);
  LVTNode := TVTNode<T>.Create(FTree, AData);
  Nodes.Add(LVTNode);
  LVNode := FTree.AddChild(VNode, LVTNode);
  LVTNode.VNode := LVNode;
  Result := LVTNode;
end;
{$ENDREGION}

end.
