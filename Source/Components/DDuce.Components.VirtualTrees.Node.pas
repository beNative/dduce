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
  TVTNode<T> = class;

  TVTNodeEnumerator<T> = record
  strict private
    FVTNode : TVTNode<T>;
    FTree   : TCustomVirtualStringTree;
  public
    constructor Create(
      ATree   : TCustomVirtualStringTree;
      AVTNode : TVTNode<T>
    );

    function GetCurrent: TVTNode<T>;
    function MoveNext: Boolean;
    property Current: TVTNode<T>
      read GetCurrent;
  end;

  TVTNode<T> = class
  private
    FVNode      : PVirtualNode;
    FTree       : TCustomVirtualStringTree;
    FData       : T;
    FText       : string;
    FHint       : string;
    FImageIndex : Integer;
    FCheckState : TCheckState;
    FCheckType  : TCheckType;

  protected
    {$REGION 'property access methods'}
    function GetItem(AIndex: UInt32): TVTNode<T>;
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
    function GetChildCount: UInt32;
    function GetText: string; virtual;
    procedure SetText(const Value: string); virtual;
    {$ENDREGION}

    function GetEnumerator : TVTNodeEnumerator<T>;

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
    procedure BeforeDestruction; override;

    function Add(const AData: T): TVTNode<T>;

    property VNode: PVirtualNode
      read GetVNode write SetVNode;

    property ChildCount: UInt32
      read GetChildCount;

    property Data: T
      read GetData write SetData;

    property CheckState: TCheckState
      read GetCheckState write SetCheckState;

    property CheckType: TCheckType
      read GetCheckType write SetCheckType;

    property ImageIndex: Integer
      read GetImageIndex write SetImageIndex;

    property Items[AIndex: UInt32]: TVTNode<T>
      read GetItem; default;

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

procedure TVTNode<T>.BeforeDestruction;
begin
  if GetTypekind(T) = tkClass then
    FreeAndNil(FData);
  FTree  := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TVTNode<T>.GetCheckState: TCheckState;
begin
  if Assigned(VNode) then
    FCheckState := VNode.CheckState;
  Result := FCheckState;
end;

procedure TVTNode<T>.SetCheckState(const Value: TCheckState);
begin
  FCheckState := Value;
  if Assigned(VNode) then
    VNode.CheckState := Value;
end;

function TVTNode<T>.GetCheckType: TCheckType;
begin
  if Assigned(VNode) then
    FCheckType := VNode.CheckType;
  Result := FCheckType;
end;

procedure TVTNode<T>.SetCheckType(const Value: TCheckType);
begin
  FCheckType := Value;
  if Assigned(VNode) then
    VNode.CheckType := Value;
end;

function TVTNode<T>.GetChildCount: UInt32;
begin
  if Assigned(VNode) then
    Result := VNode.ChildCount
  else
    Result := 0;
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
  if Assigned(VNode) then
    Result := VNode.Index
  else
    Result := 0;
end;

function TVTNode<T>.GetItem(AIndex: UInt32): TVTNode<T>;
var
  I	 : Integer;
	VN : PVirtualNode;
begin
  Guard.CheckIndex(VNode.ChildCount, AIndex);
	VN := VNode.FirstChild;
  for I := 0 to AIndex - 1 do
  begin
    Guard.CheckNotNull(VN, 'VN');
    VN := VN.NextSibling;
  end;
	Result := FTree.GetNodeData<TVTNode<T>>(VN);
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
  if Value <> VNode then
  begin
    FVNode := Value;
    if Assigned(FVNode) then
    begin
      FVNode.CheckState := FCheckState;
      FVNode.CheckType  := FCheckType;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TVTNode<T>.GetEnumerator: TVTNodeEnumerator<T>;
begin
  Result := TVTNodeEnumerator<T>.Create(FTree, Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
function TVTNode<T>.Add(const AData: T): TVTNode<T>;
var
  LVTNode : TVTNode<T>;
  LVNode  : PVirtualNode;
begin
  if not Assigned(VNode) then // create root node if it does not exist
  begin
    VNode := FTree.AddChild(nil, Self);
  end;
  LVTNode := TVTNode<T>.Create(FTree, AData);
  //Nodes.Add(LVTNode);
  LVNode := FTree.AddChild(VNode, LVTNode);
  LVTNode.VNode := LVNode;
  Result := LVTNode;
end;
{$ENDREGION}

{$REGION 'TVTNodeEnumerator<T>'}
constructor TVTNodeEnumerator<T>.Create(ATree: TCustomVirtualStringTree;
  AVTNode: TVTNode<T>);
begin
  FTree := ATree;
  FVTNode := AVTNode;
end;

function TVTNodeEnumerator<T>.GetCurrent: TVTNode<T>;
begin

end;

function TVTNodeEnumerator<T>.MoveNext: Boolean;
begin

end;
{$ENDREGION}

end.
