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

unit DDuce.ValueList;

{ A valuelist based on a virtual treeview. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  Spring.Collections,

  VirtualTrees,

  DDuce.DynamicRecord, DDuce.ValueList.Node;

type
  TfrmValueList = class(TForm)
  private
    FValueList : TVirtualStringTree;
    FData      : DynamicRecord;
    FNodes     : IList<TValueListNode>;

    procedure FValueListInitNode(
      Sender            : TBaseVirtualTree;
      ParentNode,
      Node              : PVirtualNode;
      var InitialStates : TVirtualNodeInitStates
    );
    procedure FValueListInitChildren(
      Sender         : TBaseVirtualTree;
      Node           : PVirtualNode;
      var ChildCount : Cardinal
    );
    procedure FValueListGetText(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      TextType     : TVSTTextType;
      var CellText : string
    );

    procedure FValueListBeforeCellPaint(
      Sender          : TBaseVirtualTree;
      TargetCanvas    : TCanvas;
      Node            : PVirtualNode;
      Column          : TColumnIndex;
      CellPaintMode   : TVTCellPaintMode;
      CellRect        : TRect;
      var ContentRect : TRect
    );

  protected
    function GetData: IDynamicRecord;
    procedure SetData(const Value: IDynamicRecord);

    procedure InitializeTreeViewer;

  public
    procedure AfterConstruction; override;

    property Data: IDynamicRecord
      read GetData write SetData;

  end;

implementation

{$R *.dfm}

uses
  Spring,

  DDuce.Factories.VirtualTrees,

  DDuce.Logger, DDuce.Logger.Channels.WinIPC,
  DDuce.ObjectInspector.zObjectInspector;

{$REGION 'construction and destruction'}
procedure TfrmValueList.AfterConstruction;
begin
  inherited AfterConstruction;
  FNodes := TCollections.CreateObjectList<TValueListNode>;
  InitializeTreeViewer;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmValueList.GetData: IDynamicRecord;
begin
  Result := FData;
end;

procedure TfrmValueList.SetData(const Value: IDynamicRecord);
begin
  FData.Assign(Value);
  FValueList.NodeDataSize := SizeOf(TValueListNode);
  FValueList.RootNodeCount := FData.Count;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmValueList.FValueListBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  N : TValueListNode;
begin
  N := Sender.GetNodeData<TValueListNode>(Node);
  ContentRect.Offset(2, 0);
  if Column = 0 then
  begin
    TargetCanvas.Brush.Color := clCream;
    if Assigned(N.Field) then
    begin
      TargetCanvas.FillRect(Rect(0, 0, 16, CellRect.Height));
    end
    else
    begin
      TargetCanvas.FillRect(Rect(0, 0, 24, CellRect.Height));
    end;
 end;
end;

procedure TfrmValueList.FValueListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  N : TValueListNode;
begin
  N := Sender.GetNodeData<TValueListNode>(Node);
  Guard.CheckNotNull(N, 'N');
  if Column = 0 then
  begin
    CellText := N.Name
  end
  else if Column = 1 then
    CellText := N.Value.ToString;
end;

procedure TfrmValueList.FValueListInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  N : TValueListNode;
  SN : TValueListNode;
begin
  N := Sender.GetNodeData<TValueListNode>(Node);
  ChildCount := N.Count;
  if N.Count > 0 then
  begin
    for SN in N.Nodes do
    begin
      SN.VTNode := Sender.AddChild(Node, SN);
    end;
  end;
end;

procedure TfrmValueList.FValueListInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  N  : TValueListNode;
  SN : TValueListNode;
  I  : Integer;
begin
  if ParentNode = nil then
    InitialStates := InitialStates + [ivsHasChildren, ivsExpanded];
  N := TValueListNode.Create;
  FNodes.Add(N);
  N.VTNode := Node;
  N.Field := FData.Items[Node.Index];

  Node.SetData(N);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmValueList.InitializeTreeViewer;
begin
  FValueList := TVirtualStringTreeFactory.CreateTreeList(Self, Self);
  FValueList.OnInitNode        := FValueListInitNode;
  FValueList.OnInitChildren    := FValueListInitChildren;
  FValueList.OnGetText         := FValueListGetText;
  FValueList.OnBeforeCellPaint := FValueListBeforeCellPaint;

  FValueList.Header.AutoSizeIndex := 0;
  FValueList.LineMode := lmBands;

  with FValueList do
  begin
    with Header.Columns.Add do
    begin
      Color    := clWhite;
      MaxWidth := 200;
      MinWidth := 100;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coResizable, coShowDropMark, coVisible, coSmartResize, coAllowFocus,
        coEditable];
      Position := 1;
      Indent   := 4;
      Width    := 200;
      Text := 'Name';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 800;
      MinWidth := 100;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
        coSmartResize, coAllowFocus, coEditable];
      Position := 2;
      Width    := 100;
      Text := 'Value';
    end;
    Header.MainColumn := 0;
  end;
  FValueList.Indent := 8;
  FValueList.Header.Options := FValueList.Header.Options - [hoVisible];
//  InspectComponent(FValueList);
end;
{$ENDREGION}

initialization
  Logger.Channels.Add(TWinIPCChannel.Create);

end.
