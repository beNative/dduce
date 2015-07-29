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

unit Demo.DDuce.DynamicRecord;

{ This form demonstrates the basics of the TRecord data structure.
  It also features the following components:
    - TInspector (DDuce.Components.Inspector)
 }

{
  TODO: check initialization of records
  - TRecord<T>
  - using ZeroMem
  - using RTTI on record fields (using Default())
}

interface

{$I ..\Source\DDuce.inc}

uses
  Winapi.Windows,
  System.SysUtils, System.Variants, System.Classes, System.Actions, System.Rtti,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ActnList, Vcl.Grids, Vcl.DBGrids,
  Vcl.ComCtrls, Vcl.ExtCtrls,
  Data.DB,
  Datasnap.DBClient,

  Spring,

  DDuce.Components.GridView, DDuce.Components.Inspector,

  DDuce.DynamicRecord,

  Demo.Contact;

type
  TTestClass = class
  strict private
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
  strict private
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
  TfrmDynamicRecords = class(TForm)
    {$REGION 'designer controls'}
    aclMain                   : TActionList;
    actTestAssign             : TAction;
    actTestAssignTo           : TAction;
    actToStrings              : TAction;
    btnTestAssign             : TButton;
    btnTestAssignTo           : TButton;
    btnTestAssignTo1          : TButton;
    dscTest                   : TDataSource;
    dsTest                    : TClientDataSet;
    pnlBottom                 : TGridPanel;
    grdTest                   : TDBGrid;
    lblContact                : TLabel;
    lblTestClass              : TLabel;
    lblTestRecord             : TLabel;
    lblTestTRecord            : TLabel;
    pgcMain                   : TPageControl;
    pnlRecordInspector        : TPanel;
    tsContactObject           : TTabSheet;
    tsDataSet                 : TTabSheet;
    tsTestClass               : TTabSheet;
    tsTestRecord              : TTabSheet;
    tsTRecord                 : TTabSheet;
    actTestData               : TAction;
    btnTestData               : TButton;
    pnlRecordInspectorHeader  : TPanel;
    pnlBottomRight            : TPanel;
    pnlRightBottomHeader      : TPanel;
    pnlTRecordRepresentations : TGridPanel;
    grpAsCommaText            : TGroupBox;
    grpAsDelimitedText        : TGroupBox;
    grpToStrings              : TGroupBox;
    grpToString               : TGroupBox;
    lblAsCommaText            : TLabel;
    lblAsDelimitedText        : TLabel;
    lblToStrings              : TLabel;
    lblToString               : TLabel;
    chkQuoteValues            : TCheckBox;
    edtDelimiter              : TLabeledEdit;
    edtQuoteChar              : TLabeledEdit;
    chkAlignValues            : TCheckBox;
    {$ENDREGION}

    procedure actTestAssignExecute(Sender: TObject);
    procedure actTestDataExecute(Sender: TObject);
    procedure actToStringsExecute(Sender: TObject);
    procedure chkAlignValuesClick(Sender: TObject);
    procedure chkQuoteValuesClick(Sender: TObject);
    procedure dscTestDataChange(Sender: TObject; Field: TField);
    procedure edtDelimiterChange(Sender: TObject);
    procedure edtQuoteCharChange(Sender: TObject);

  private
    FContact          : TContact;
    FTestRecord       : TTestRecord;
    FTestClass        : TTestClass;
    FTestTRecord      : TRecord;
    FInspector        : TInspector;
    FRecord           : TRecord;
    FUpdate           : Boolean;
    FStrings          : TStrings;

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
          Sender : TObject;
          Cell   : TGridCell;
      var Style  : TGridEditStyle
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
  Vcl.Dialogs,

  Demo.Utils;

{$REGION 'construction and destruction'}
procedure TfrmDynamicRecords.AfterConstruction;
begin
  inherited AfterConstruction;
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
  inherited BeforeDestruction;
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
    Value := FRecord.Items[Cell.Row].Value.ToString;
  end;
end;

procedure TfrmDynamicRecords.InspectorSetEditText(Sender: TObject;
  Cell: TGridCell; var Value: String);
begin
  if Cell.Col = 1 then
  begin
    FRecord.Items[Cell.Row].Value := Value;
    Changed;
  end;
end;

procedure TfrmDynamicRecords.dscTestDataChange(Sender: TObject; Field: TField);
begin
  if Field = nil then
  begin
    ExecuteFromDataSet;
  end;
end;

procedure TfrmDynamicRecords.edtDelimiterChange(Sender: TObject);
begin
  Changed;
end;

procedure TfrmDynamicRecords.edtQuoteCharChange(Sender: TObject);
begin
  Changed;
end;

procedure TfrmDynamicRecords.chkAlignValuesClick(Sender: TObject);
begin
  Changed;
end;

procedure TfrmDynamicRecords.chkQuoteValuesClick(Sender: TObject);
begin
  Changed;
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
  FTestRecord.TestNullableString := 'NullableString';
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
  FTestClass.TestNullableString := 'NullableString';
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
  if FUpdate then
  begin
    FInspector.Rows.Count := FRecord.Count;
    FInspector.UpdateEditText;
    FInspector.Invalidate;
    lblContact.Caption     := AsPropString(FContact);
    lblTestClass.Caption   := AsPropString(FTestClass);
    lblTestRecord.Caption  := AsFieldString(TValue.From(FTestRecord));
    lblTestTRecord.Caption := FTestTRecord.ToString;

    FRecord.ToStrings(FStrings);
    lblAsCommaText.Caption     := FRecord.AsCommaText;
    lblAsDelimitedText.Caption := FRecord.AsDelimitedText(
      edtDelimiter.Text,
      chkQuoteValues.Checked,
      edtQuoteChar.Text[1]
    );
    lblToString.Caption := FRecord.ToString(chkAlignValues.Checked);
    lblToStrings.Caption := FStrings.Text;
    FUpdate := False;
  end;
  inherited UpdateActions;
end;
{$ENDREGION}

end.
