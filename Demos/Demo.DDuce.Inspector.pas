{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I ..\Source\DDuce.inc}

unit Demo.DDuce.Inspector;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.DBGrids,
  Vcl.ImgList, System.Actions, Vcl.ActnList, Vcl.ExtCtrls, Vcl.Menus,
  Vcl.StdCtrls,
  Data.DB,

  DDuce.Components.GridView, DDuce.Components.Inspector,
  DDuce.Components.PropertyInspector,

  Demo.Contact;

type
  TfrmInspector = class(TForm)
    pnlLeft            : TPanel;
    pnlRight           : TPanel;
    splVertical        : TSplitter;
    dscMain            : TDataSource;
    pmMain             : TPopupMenu;
    mniHideEmptyFields : TMenuItem;
    aclMain            : TActionList;
    actHideEmptyFields : TAction;
    pnlInspector       : TPanel;
    grdMain            : TDBGrid;
    splHorizontal      : TSplitter;

    procedure dscMainDataChange(Sender: TObject; Field: TField);

  private
    FDataSet           : TDataSet;
    FInspector         : TInspector;
    FPropertyInspector : TPropertyInspector;

    procedure FInspectorGetCellText(
          Sender : TObject;
          Cell   : TGridCell;
      var Value  : string
    );
    procedure FInspectorSetEditText(
          Sender : TObject;
          Cell   : TGridCell;
      var Value  : string
    );
    procedure FInspectorGetCellReadOnly(
          Sender       : TObject;
          Cell         : TGridCell;
      var CellReadOnly : Boolean
    );
    procedure FInspectorEditCanModify(
          Sender    : TObject;
          Cell      : TGridCell;
      var CanModify : Boolean
    );
    procedure FInspectorGetEditStyle(
          Sender : TObject;
          Cell   : TGridCell;
      var Style  : TGridEditStyle
    );
    procedure FInspectorGetCheckKind(
          Sender    : TObject;
          Cell      : TGridCell;
      var CheckKind : TGridCheckKind
    );
    procedure FInspectorGetCheckAlignment(
          Sender         : TObject;
          Cell           : TGridCell;
      var CheckAlignment : TAlignment
    );
    procedure FInspectorGetCheckState(
          Sender     : TObject;
          Cell       : TGridCell;
      var CheckState : TCheckBoxState
    );
    procedure FInspectorCheckClick(Sender: TObject; Cell: TGridCell);
    procedure FInspectorEditCanShow(
          Sender  : TObject;
          Cell    : TGridCell;
      var CanShow : Boolean
    );

  protected
    procedure UpdateView;

    function IsCellReadOnly(const ACell: TGridCell): Boolean; inline;
    function IsCellCheckBox(const ACell: TGridCell): Boolean; inline;

    function FieldOf(const ACell: TGridCell): TField;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  Demo.Factories, Demo.Data;

{$REGION 'construction and destruction'}
procedure TfrmInspector.AfterConstruction;
begin
  inherited AfterConstruction;
  FDataSet := Demo.Data.Data.DataSet;

  FInspector := TDemoFactories.CreateInspector(Self, pnlInspector);
  FInspector.OnGetCellText       := FInspectorGetCellText;
  FInspector.OnSetEditText       := FInspectorSetEditText;
  FInspector.OnGetCellReadOnly   := FInspectorGetCellReadOnly;
  FInspector.OnEditCanModify     := FInspectorEditCanModify;
  FInspector.OnGetEditStyle      := FInspectorGetEditStyle;
  FInspector.OnGetCheckKind      := FInspectorGetCheckKind;
  FInspector.OnGetCheckAlignment := FInspectorGetCheckAlignment;
  FInspector.OnGetCheckState     := FInspectorGetCheckState;
  FInspector.OnCheckClick        := FInspectorCheckClick;
  FInspector.OnEditCanShow       := FInspectorEditCanShow;
  FPropertyInspector :=
    TDemoFactories.CreatePropertyInspector(Self, pnlLeft, FInspector);
  dscMain.DataSet := FDataSet;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmInspector.dscMainDataChange(Sender: TObject; Field: TField);
begin
  if not Assigned(Field) then
    UpdateView;
end;

procedure TfrmInspector.FInspectorGetCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);
begin
  if Cell.Col = 0 then
    Value := FieldOf(Cell).FieldName
  else
  begin
    if IsCellCheckBox(Cell) then
      Value := ''
    else
      Value := FieldOf(Cell).AsString;
  end;
end;

procedure TfrmInspector.FInspectorGetCheckAlignment(Sender: TObject;
  Cell: TGridCell; var CheckAlignment: TAlignment);
begin
  CheckAlignment := taCenter;
end;

procedure TfrmInspector.FInspectorGetCheckKind(Sender: TObject;
  Cell: TGridCell; var CheckKind: TGridCheckKind);
begin
  if IsCellCheckBox(Cell) then
    CheckKind := gcCheckBox;
end;

procedure TfrmInspector.FInspectorGetCheckState(Sender: TObject;
  Cell: TGridCell; var CheckState: TCheckBoxState);
begin
  if IsCellCheckBox(Cell) then
  begin
    if FieldOf(Cell).AsBoolean then
      CheckState := cbChecked
    else
      CheckState := cbUnchecked;
  end;
end;

procedure TfrmInspector.FInspectorGetEditStyle(Sender: TObject;
  Cell: TGridCell; var Style: TGridEditStyle);
begin
  Style := geEllipsis;
end;

procedure TfrmInspector.FInspectorSetEditText(Sender: TObject;
  Cell: TGridCell; var Value: String);
begin
  if not IsCellReadOnly(Cell) and (Value <> FieldOf(Cell).AsString) then
  begin
    FDataSet.Edit;
    FDataSet.Fields[Cell.Row].AsString := Value;
    FDataSet.Post;
  end;
end;

procedure TfrmInspector.FInspectorGetCellReadOnly(Sender: TObject;
  Cell: TGridCell; var CellReadOnly: Boolean);
begin
  CellReadOnly := IsCellReadOnly(Cell);
end;

procedure TfrmInspector.FInspectorCheckClick(Sender: TObject; Cell: TGridCell);
var
  F: TField;
begin
  inherited;
  if IsCellCheckBox(Cell) then
  begin
    F := FieldOf(Cell);
    FDataSet.Edit;
    if F.IsNull then
       F.Value := 1
    else
    begin
      if F.Value = 0 then
        F.Value := 1
      else
        F.Value := 0
    end;
    if FDataSet.State in dsEditModes then
      FDataSet.Post;
  end;
end;

procedure TfrmInspector.FInspectorEditCanModify(Sender: TObject;
  Cell: TGridCell; var CanModify: Boolean);
begin
  CanModify := not IsCellReadOnly(Cell);
end;

procedure TfrmInspector.FInspectorEditCanShow(Sender: TObject; Cell: TGridCell;
  var CanShow: Boolean);
begin
  CanShow := not IsCellCheckBox(Cell);
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TfrmInspector.FieldOf(const ACell: TGridCell): TField;
begin
  if ACell.Row < FDataSet.FieldCount then
  begin
    Result := FDataSet.Fields[ACell.Row];
  end
  else
    Exit(nil);
end;

function TfrmInspector.IsCellCheckBox(const ACell: TGridCell): Boolean;
begin
  Result := (ACell.Col = 1) and (FieldOf(ACell) is TBooleanField);
end;

function TfrmInspector.IsCellReadOnly(const ACell: TGridCell): Boolean;
begin
  Result := (ACell.Col = 0) or IsCellCheckBox(ACell) or FieldOf(ACell).ReadOnly;
end;

procedure TfrmInspector.UpdateView;
begin
  FInspector.LockUpdate;
  try
    FInspector.Rows.Count := FDataSet.FieldCount;
    FInspector.UpdateEditContents(False);
  finally
    FInspector.UnLockUpdate(True);
  end;
end;
{$ENDREGION}

end.
