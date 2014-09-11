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

unit Demo.DDuce.DynamicRecord;

{
  TODO: check initialization of records
  - using ZeroMem
  - using RTTI on record fields (using Default())
}

interface

{$I ..\Source\DDuce.inc}

uses
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ActnList, Vcl.Grids, Vcl.DBGrids,
  Vcl.ComCtrls, Vcl.ExtCtrls,
  Data.DB,
  Datasnap.DBClient,
  Winapi.Windows,

  Spring,

  DDuce.Components.GridView, DDuce.Components.Inspector,

  DDuce.DynamicRecord,

  Demo.Contact, System.Rtti;

type
  TTestClass = class
  private
    FTestBoolean          : Boolean;
    FTestChar             : Char;
    FTestDateTime         : TDateTime;
    FTestDouble           : Double;
    FTestInteger          : Integer;
    FTestString           : string;
    FTestNullableBoolean  : Nullable<Boolean>;
    FTestNullableDateTime : Nullable<TDateTime>;
    FTestNullableDouble   : Nullable<Double>;
    FTestNullableInteger  : Nullable<Integer>;
    FTestNullableString   : Nullable<string>;
    FTestNullableChar     : Nullable<Char>;

  public
    property TestBoolean: Boolean
      read FTestBoolean write FTestBoolean;
    property TestChar: Char
      read FTestChar write FTestChar;
    property TestDateTime: TDateTime
      read FTestDateTime write FTestDateTime;
    property TestDouble: Double
      read FTestDouble write FTestDouble;
    property TestInteger: Integer
      read FTestInteger write FTestInteger;
    property TestNullableBoolean: Nullable<Boolean>
      read FTestNullableBoolean write FTestNullableBoolean;
    property TestNullableDateTime: Nullable<TDateTime>
      read FTestNullableDateTime write FTestNullableDateTime;
    property TestNullableDouble: Nullable<Double>
      read FTestNullableDouble write FTestNullableDouble;
    property TestNullableInteger: Nullable<Integer>
      read FTestNullableInteger write FTestNullableInteger;
    property TestNullableString: Nullable<string>
      read FTestNullableString write FTestNullableString;
    property TestString: string
      read FTestString write FTestString;
    property TestNullableChar: Nullable<Char>
      read FTestNullableChar write FTestNullableChar;
  end;

type
  TTestRecord = record
  private
    FTestBoolean          : Boolean;
    FTestChar             : Char;
    FTestDateTime         : TDateTime;
    FTestDouble           : Double;
    FTestInteger          : Integer;
    FTestString           : string;
    FTestNullableBoolean  : Nullable<Boolean>;
    FTestNullableDateTime : Nullable<TDateTime>;
    FTestNullableDouble   : Nullable<Double>;
    FTestNullableInteger  : Nullable<Integer>;
    FTestNullableString   : Nullable<string>;
    FTestNullableChar     : Nullable<Char>;

    property TestBoolean: Boolean
      read FTestBoolean write FTestBoolean;
    property TestChar: Char
      read FTestChar write FTestChar;
    property TestDateTime: TDateTime
      read FTestDateTime write FTestDateTime;
    property TestDouble: Double
      read FTestDouble write FTestDouble;
    property TestInteger: Integer
      read FTestInteger write FTestInteger;
    property TestNullableBoolean: Nullable<Boolean>
      read FTestNullableBoolean write FTestNullableBoolean;
    property TestNullableDateTime: Nullable<TDateTime>
      read FTestNullableDateTime write FTestNullableDateTime;
    property TestNullableDouble: Nullable<Double>
      read FTestNullableDouble write FTestNullableDouble;
    property TestNullableInteger: Nullable<Integer>
      read FTestNullableInteger write FTestNullableInteger;
    property TestNullableString: Nullable<string>
      read FTestNullableString write FTestNullableString;
    property TestString: string
      read FTestString write FTestString;
    property TestNullableChar: Nullable<Char>
      read FTestNullableChar write FTestNullableChar;
  end;

type
  TfrmDynamicRecords = class(TForm)
    {$REGION 'designer controls'}
    aclMain            : TActionList;
    actTestAssign      : TAction;
    actTestAssignTo    : TAction;
    actToStrings       : TAction;
    btnTestAssign      : TButton;
    btnTestAssignTo    : TButton;
    btnTestAssignTo1   : TButton;
    dscTest            : TDataSource;
    dsTest             : TClientDataSet;
    grdpnl1            : TGridPanel;
    grdTest            : TDBGrid;
    lblContact         : TLabel;
    lblRecord          : TLabel;
    lblStrings         : TLabel;
    lblTestClass       : TLabel;
    lblTestRecord      : TLabel;
    lblTestTRecord     : TLabel;
    pgcMain            : TPageControl;
    pnlRecordInspector : TPanel;
    tsContactObject    : TTabSheet;
    tsDataSet          : TTabSheet;
    tsTestClass        : TTabSheet;
    tsTestRecord       : TTabSheet;
    tsTRecord          : TTabSheet;
    actTestData        : TAction;
    btnTestData        : TButton;
    {$ENDREGION}

    procedure actTestAssignExecute(Sender: TObject);
    procedure dscTestDataChange(Sender: TObject; Field: TField);
    procedure actToStringsExecute(Sender: TObject);
    procedure actTestDataExecute(Sender: TObject);


  private
    FContact     : TContact;
    FTestRecord  : TTestRecord;
    FTestClass   : TTestClass;
    FTestTRecord : TRecord;
    FInspector   : TInspector;
    FRecord      : TRecord;
    FUpdate      : Boolean;
    FStrings     : TStrings;

    function CreateInspector(AParent : TWinControl): TInspector;
    procedure CreateContact;
    procedure CreateTestClass;
    procedure CreateTestRecord;
    procedure CreateTestTRecord;

    procedure InspectorGetCellText(
          Sender : TObject;
          Cell   : TGridCell;
      var Value  : string
    );
    procedure InspectorSetEditText(
          Sender : TObject;
          Cell   : TGridCell;
      var Value  : string
    );
    procedure InspectorGetEditStyle(
          Sender  : TObject;
          Cell    : TGridCell;
      var Style   : TGridEditStyle
    );
    procedure InspectorGetEditText(
          Sender : TObject;
          Cell   : TGridCell;
      var Value  : string
    );

    procedure Changed;

    procedure ExecuteFromDataSet;
    procedure ExecuteAssignTContact;
    procedure ExecuteAssignTTestClass;
    procedure ExecuteAssignTTestRecord;
    procedure ExecuteAssignTRecord;

  protected
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

uses
  System.TypInfo,

  Demo.Utils;

{$REGION 'construction and destruction'}
procedure TfrmDynamicRecords.AfterConstruction;
begin
  inherited;
  FInspector := CreateInspector(pnlRecordInspector);
  FStrings := TStringList.Create;
  CreateContact;
  CreateTestClass;
  CreateTestRecord;
  CreateTestTRecord;
  Changed;
end;

procedure TfrmDynamicRecords.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FStrings);
  FreeAndNil(FContact);
  FreeAndNil(FTestClass);
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmDynamicRecords.actTestAssignExecute(Sender: TObject);
begin
  FRecord.Clear;
  if pgcMain.ActivePage = tsDataSet then
  begin
    ExecuteFromDataSet;
  end
  else if pgcMain.ActivePage = tsContactObject then
  begin
    ExecuteAssignTContact;
  end
  else if pgcMain.ActivePage = tsTestClass then
  begin
    ExecuteAssignTTestClass;
  end
  else if pgcMain.ActivePage = tsTestRecord then
  begin
    ExecuteAssignTTestRecord;
  end
  else if pgcMain.ActivePage = tsTRecord then
  begin
    ExecuteAssignTRecord;
  end
end;

procedure TfrmDynamicRecords.actTestDataExecute(Sender: TObject);
var
  V: Variant;
begin
  V := 50;
  FRecord.Data.MyInt := 10;
  FRecord.Data.MyVar := V;
  Changed;
end;

procedure TfrmDynamicRecords.actToStringsExecute(Sender: TObject);
begin
  FRecord.ToStrings(FStrings);
  Changed;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmDynamicRecords.InspectorGetCellText(Sender: TObject;
  Cell: TGridCell; var Value: string);
begin
  if Cell.Col = 0 then
  begin
    Value := FRecord.Items[Cell.Row].Name;
  end
  else
  begin
    Value := FRecord.ToString(FRecord.Items[Cell.Row].Name);
  end;
end;

procedure TfrmDynamicRecords.InspectorGetEditStyle(Sender: TObject;
  Cell: TGridCell; var Style: TGridEditStyle);
begin
  Style := geSimple;
end;

procedure TfrmDynamicRecords.InspectorGetEditText(Sender: TObject;
  Cell: TGridCell; var Value: string);
begin
  if Cell.Col = 0 then
  begin
    Value := FRecord.Items[Cell.Row].Name;
  end
  else
  begin
    Value := FRecord.ToString(FRecord.Items[Cell.Row].Name);
  end;
end;

procedure TfrmDynamicRecords.InspectorSetEditText(Sender: TObject;
  Cell: TGridCell; var Value: String);
begin
  if Cell.Col = 1 then
  begin
    FRecord.Items[Cell.Row].Value := Value;
    FUpdate := True;
  end;
end;

procedure TfrmDynamicRecords.dscTestDataChange(Sender: TObject; Field: TField);
begin
  if Field = nil then
  begin
    ExecuteFromDataSet;
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmDynamicRecords.CreateTestRecord;
var
  V: TValue;
begin
  V := TValue.From<TTestRecord>(FTestRecord);
  ZeroMemory(@FTestRecord, SizeOf(FTestRecord));
  FTestRecord.TestString := 'string';
  FTestRecord.TestNullableString := 'nullablestring';
  FTestRecord.TestDouble := PI;
  FTestRecord.TestInteger:= 554;
end;

procedure TfrmDynamicRecords.CreateTestTRecord;
begin
  FTestTRecord.Data.SomeString   := 'FTestTRecord.Data.SomeString';
  FTestTRecord.Data.SomeDateTime := Now;
  FTestTRecord.Data.Number       := 5;
  FTestTRecord.Data.Bool         := True;
end;

procedure TfrmDynamicRecords.CreateTestClass;
begin
  if Assigned(FTestClass) then
    FreeAndNil(FTestClass);
  FTestClass := TTestClass.Create;
  FTestClass.TestString := 'string';
  FTestClass.TestNullableString := 'nullablestring';
  FTestClass.TestNullableString := Null;
end;

procedure TfrmDynamicRecords.CreateContact;
begin
  if Assigned(FContact) then
    FreeAndNil(FContact);
  FContact := TContact.Create;
  FContact.FirstName := 'Tim';
  FContact.LastName := 'Sinaeve';
  FContact.Number := 5;
  FContact.BirthDate := Now;
end;

function TfrmDynamicRecords.CreateInspector(AParent : TWinControl): TInspector;
begin
//  Result := TInspector.Create(Self);
//  Result.Parent         := AParent;
//  Result.Align          := alClient;
//  Result.OnGetCellText  := InspectorGetCellText;
//  Result.OnGetEditStyle := InspectorGetEditStyle;
//  Result.OnSetEditText  := InspectorSetEditText;
//  Result.OnGetEditText  := InspectorGetEditText;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmDynamicRecords.ExecuteAssignTContact;
begin
  FRecord.Assign(FContact);
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignTRecord;
begin
  FRecord.Assign(FTestTRecord);
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignTTestClass;
begin
  FRecord.Assign(FTestClass);
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignTTestRecord;
begin
  FRecord.Assign(TValue.From(FTestRecord), False, True);
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteFromDataSet;
begin
  FRecord.FromDataSet(dsTest);
  FUpdate := True;
end;

procedure TfrmDynamicRecords.Changed;
begin
  FUpdate := True;
end;

procedure TfrmDynamicRecords.UpdateActions;
begin
  inherited;
  if FUpdate then
  begin
    lblRecord.Caption := FRecord.ToString;
//    FInspector.Rows.Count := FRecord.Count;
//    FInspector.Invalidate;
//    FInspector.UpdateEditContents(False);

    lblContact.Caption := AsPropString(FContact);
    lblTestClass.Caption := AsPropString(FTestClass);
    lblTestRecord.Caption := AsFieldString(TValue.From(FTestRecord));
    lblTestTRecord.Caption := FTestTRecord.ToString;

    lblStrings.Caption := FStrings.Text;
    FUpdate := False;
  end;
end;
{$ENDREGION}

end.
