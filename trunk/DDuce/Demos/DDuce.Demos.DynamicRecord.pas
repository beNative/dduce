{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Demos.DynamicRecord;

{
//  if  R['waarde'].IsEmpty then //  [DCC Fatal Error] Concepts.Forms.DynamicRecords.pas(123): F2084 Internal Error: C13394
//  // Resolved in Build: : 15.0.3722.28600

}

{
  TODO: check initialization of records
  - using ZeroMem
  - using RTTI on record fields (using Default())
}

//*****************************************************************************

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, Grids, DBGrids, ComCtrls, DB, DBClient,
  ExtCtrls, System.Actions,

  Spring,

  DDuce.Components.GridView, DDuce.Components.Inspector,

  DDuce.DynamicRecord,

  DDuce.Demos.Contact;

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

//=============================================================================

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
    btn1: TButton;
    {$ENDREGION}

    procedure actTestAssignExecute(Sender: TObject);
    procedure dscTestDataChange(Sender: TObject; Field: TField);
    procedure actToStringsExecute(Sender: TObject);
    procedure actTestDataExecute(Sender: TObject);
    procedure btn1Click(Sender: TObject);

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

    procedure InspectorGetCellText(Sender: TObject; Cell: TGridCell; var Value: string);
    procedure InspectorSetEditText(Sender: TObject; Cell: TGridCell;
      var Value: string);
    procedure InspectorGetEditStyle(Sender: TObject; Cell: TGridCell; var Style: TGridEditStyle);
    procedure InspectorGetEditText(Sender: TObject; Cell: TGridCell; var Value: string);

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

//*****************************************************************************

implementation

{$R *.dfm}

uses
  TypInfo, Rtti, StrUtils,

  DDuce.Reflect,

  DDuce.Demos.Utils;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TfrmDynamicRecords.AfterConstruction;
begin
  inherited;
  ReportMemoryLeaksOnShutdown := True;
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

procedure TfrmDynamicRecords.btn1Click(Sender: TObject);
begin
  if Assigned(Reflect.Properties(FContact)) then
  begin
    Reflect.Properties(FContact)['FirstName'] := 'Michael';
     ShowMessage(Reflect.Properties(FContact).ToString);
  end;
    //ShowMessage(Reflect.Properties(FContact)['FirstName'].ToString);
    ShowMessage(Reflect.Properties(FContact).ToString);
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

{$REGION 'action handlers'}
//*****************************************************************************
// action handlers                                                       BEGIN
//*****************************************************************************

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

//*****************************************************************************
// action handlers                                                         END
//*****************************************************************************
{$ENDREGION}

{$REGION 'event handlers'}
//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

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
//
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
//    ExecuteFromDataSet;
  end;
end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************
{$ENDREGION}

{$REGION 'private methods'}
//*****************************************************************************
// private methods                                                       BEGIN
//*****************************************************************************

procedure TfrmDynamicRecords.CreateTestRecord;
var
  V: TValue;
begin
  V := TValue.From<TTestRecord>(FTestRecord);
  ZeroMemory(@FTestRecord, SizeOf(FTestRecord));
  FTestRecord.TestString := 'string';
  FTestRecord.TestNullableString := 'nullablestring';
  FTestRecord.TestDouble := 3.14;
  FTestRecord.TestInteger:= 554;
end;

procedure TfrmDynamicRecords.CreateTestTRecord;
begin
  FTestTRecord.Data.SomeString   := 'FTestTRecord.Data.SomeString';
  FTestTRecord.Data.SomeDateTime := 8.695;
  //FTestTRecord['SomeDateTime'] := Now;
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
  Result := TInspector.Create(Self);
  Result.Parent         := AParent;
  Result.Align          := alClient;
  Result.OnGetCellText  := InspectorGetCellText;
  Result.OnGetEditStyle := InspectorGetEditStyle;
  Result.OnSetEditText  := InspectorSetEditText;
  Result.OnGetEditText  := InspectorGetEditText;
end;

//*****************************************************************************
// private methods                                                         END
//*****************************************************************************
{$ENDREGION}

{$REGION 'protected methods'}
//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

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
  FRecord.Assign(TValue.From<TTestRecord>(FTestRecord), False, True);
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
    FInspector.Rows.Count := FRecord.Count;
    FInspector.Invalidate;
    FInspector.UpdateEditContents(False);

    lblContact.Caption := AsPropString(FContact);
    lblTestClass.Caption := AsPropString(FTestClass);
    lblTestRecord.Caption := AsFieldString(TValue.From<TTestRecord>(FTestRecord));
    lblTestTRecord.Caption := FTestTRecord.ToString;

    lblStrings.Caption := FStrings.Text;
    FUpdate := False;
  end;
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************
{$ENDREGION}

end.
