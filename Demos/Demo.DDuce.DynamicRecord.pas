 {
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

{ This form demonstrates the basics of the DynamicRecord data structure.
  It also features the following components:
    - TInspector (DDuce.Components.Inspector)
}

interface

uses
  Winapi.Windows,
  System.SysUtils, System.Variants, System.Classes, System.Actions, System.Rtti,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ActnList, Vcl.Grids, Vcl.DBGrids,
  Vcl.ComCtrls, Vcl.ExtCtrls,
  Data.DB,
  Datasnap.DBClient,

  Spring, // for using Nullable types.

  DDuce.Components.GridView, DDuce.Components.ValueList,

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

  TManagedClass = DynamicRecord<TTestClass>;

type
  TfrmDynamicRecords = class(TForm)
    {$REGION 'designer fields'}
    aclMain                                   : TActionList;
    actAssignFDynamicRecord1ToFDynamicRecord2 : TAction;
    actAssignFDynamicRecord1ToFRecord1        : TAction;
    actAssignFDynamicRecord1ToFRecord2        : TAction;
    actAssignFDynamicRecord2ToFDynamicRecord1 : TAction;
    actAssignFDynamicRecord2ToFRecord1        : TAction;
    actAssignFDynamicRecord2ToFRecord2        : TAction;
    actAssignFieldValueToDynamicRecord1       : TAction;
    actAssignFieldValueToDynamicRecord2       : TAction;
    actAssignFieldValueToFRecord1             : TAction;
    actAssignFieldValueToFRecord2             : TAction;
    actAssignFRecord1ToFDynamicRecord1        : TAction;
    actAssignFRecord1ToFDynamicRecord2        : TAction;
    actAssignFRecord1ToFRecord2               : TAction;
    actAssignFRecord2ToFDynamicRecord1        : TAction;
    actAssignFRecord2ToFDynamicRecord2        : TAction;
    actAssignFRecord2ToFRecord1               : TAction;
    actCustomTest                             : TAction;
    actFDynamicRecord1Clear                   : TAction;
    actFDynamicRecord2Clear                   : TAction;
    actFRecord1Clear                          : TAction;
    actFRecord2Clear                          : TAction;
    actTestAssign                             : TAction;
    actTestAssignTo                           : TAction;
    actTestData                               : TAction;
    actToStrings                              : TAction;
    btnAssignFDynamicRecord1ToFDynamicRecord2 : TButton;
    btnAssignFDynamicRecord1ToFRecord1        : TButton;
    btnAssignFDynamicRecord1ToFRecord2        : TButton;
    btnAssignFDynamicRecord2ToFDynamicRecord1 : TButton;
    btnAssignFDynamicRecord2ToFRecord1        : TButton;
    btnAssignFDynamicRecord2ToFRecord2        : TButton;
    btnAssignFieldValueToDynamicRecord1       : TButton;
    btnAssignFieldValueToDynamicRecord2       : TButton;
    btnAssignFieldValueToFRecord1             : TButton;
    btnAssignFieldValueToFRecord2             : TButton;
    btnAssignFRecord1ToFDynamicRecord1        : TButton;
    btnAssignFRecord1ToFDynamicRecord2        : TButton;
    btnAssignFRecord1ToFRecord4               : TButton;
    btnAssignFRecord2ToFDynamicRecord1        : TButton;
    btnAssignFRecord2ToFDynamicRecord2        : TButton;
    btnAssignFRecord2ToFRecord1               : TButton;
    btnClearFRecord1                          : TButton;
    btnCustomTest                             : TButton;
    btnFDynamicRecord1Clear1                  : TButton;
    btnFDynamicRecord2Clear                   : TButton;
    btnFRecord2Clear1                         : TButton;
    btnTestAssign                             : TButton;
    btnTestAssignTo                           : TButton;
    btnTestAssignTo1                          : TButton;
    btnTestData                               : TButton;
    chkAlignValues                            : TCheckBox;
    chkQuoteValues                            : TCheckBox;
    dscTest                                   : TDataSource;
    edtDelimiter                              : TLabeledEdit;
    edtFieldName                              : TLabeledEdit;
    edtQuoteChar                              : TLabeledEdit;
    edtValue                                  : TLabeledEdit;
    grdTest                                   : TDBGrid;
    grpAsCommaText                            : TGroupBox;
    grpAsDelimitedText                        : TGroupBox;
    grpToString                               : TGroupBox;
    grpToStrings                              : TGroupBox;
    lbl00                                     : TLabel;
    lbl01                                     : TLabel;
    lbl02                                     : TLabel;
    lbl03                                     : TLabel;
    lbl04                                     : TLabel;
    lbl15                                     : TLabel;
    lbl20                                     : TLabel;
    lbl30                                     : TLabel;
    lbl40                                     : TLabel;
    lblContact                                : TLabel;
    lblFDynamicRecord1                        : TLabel;
    lblFDynamicRecord2                        : TLabel;
    lblFRecord1                               : TLabel;
    lblFRecord2                               : TLabel;
    lblTestClass                              : TLabel;
    lblTestRecord                             : TLabel;
    lblTestTRecord                            : TLabel;
    mmoAsCommaText                            : TMemo;
    mmoAsDelimitedText                        : TMemo;
    mmoToString                               : TMemo;
    mmoToStrings                              : TMemo;
    pgcMain                                   : TPageControl;
    pnlAssignments                            : TGridPanel;
    pnlBottom                                 : TGridPanel;
    pnlBottomRight                            : TPanel;
    pnlField                                  : TPanel;
    pnlRecordInspector                        : TPanel;
    pnlRecordInspectorHeader                  : TPanel;
    pnlRightBottomHeader                      : TPanel;
    pnlTop                                    : TPanel;
    pnlTRecordRepresentations                 : TGridPanel;
    splHorizontal: TSplitter;
    tsAssignments                             : TTabSheet;
    tsContactObject                           : TTabSheet;
    tsDataSet                                 : TTabSheet;
    tsTestClass                               : TTabSheet;
    tsTestRecord                              : TTabSheet;
    tsDynamicRecord                           : TTabSheet;
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure dscTestDataChange(Sender: TObject; Field: TField);
    procedure chkQuoteValuesClick(Sender: TObject);
    procedure edtQuoteCharChange(Sender: TObject);
    procedure edtDelimiterChange(Sender: TObject);
    procedure chkAlignValuesClick(Sender: TObject);
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actTestAssignExecute(Sender: TObject);
    procedure actToStringsExecute(Sender: TObject);
    procedure actTestDataExecute(Sender: TObject);
    procedure actFDynamicRecord1ClearExecute(Sender: TObject);
    procedure actFDynamicRecord2ClearExecute(Sender: TObject);
    procedure actAssignFieldValueToFRecord1Execute(Sender: TObject);
    procedure actAssignFRecord1ToFDynamicRecord1Execute(Sender: TObject);
    procedure actAssignFRecord1ToFRecord2Execute(Sender: TObject);
    procedure actFRecord1ClearExecute(Sender: TObject);
    procedure actFRecord2ClearExecute(Sender: TObject);
    procedure actAssignFieldValueToFRecord2Execute(Sender: TObject);
    procedure actAssignFDynamicRecord1ToFDynamicRecord2Execute(Sender: TObject);
    procedure actAssignFieldValueToDynamicRecord2Execute(Sender: TObject);
    procedure actCustomTestExecute(Sender: TObject);
    procedure actAssignFieldValueToDynamicRecord1Execute(Sender: TObject);
    procedure actAssignFRecord2ToFDynamicRecord2Execute(Sender: TObject);
    procedure actAssignFRecord2ToFDynamicRecord1Execute(Sender: TObject);
    procedure actAssignFRecord1ToFDynamicRecord2Execute(Sender: TObject);
    procedure actAssignFDynamicRecord1ToFRecord2Execute(Sender: TObject);
    procedure actAssignFDynamicRecord1ToFRecord1Execute(Sender: TObject);
    procedure actAssignFDynamicRecord2ToFRecord2Execute(Sender: TObject);
    procedure actAssignFDynamicRecord2ToFDynamicRecord1Execute(Sender: TObject);
    procedure actAssignFDynamicRecord2ToFRecord1Execute(Sender: TObject);
    procedure actAssignFRecord2ToFRecord1Execute(Sender: TObject);
    {$ENDREGION}

  private
    FContact          : TContact;
    FTestRecord       : TTestRecord;
    FTestClass        : TTestClass;
    FTestTRecord      : DynamicRecord;
    FTestManagedClass : TManagedClass;
    FValueList        : TValueList;
    FRecord           : DynamicRecord;
    FUpdate           : Boolean;
    FStrings          : TStrings;
    FDataSet          : TClientDataSet;

    // the test dummies to play with.
    FRec1  : DynamicRecord;
    FRec2  : DynamicRecord;
    FIntf1 : IDynamicRecord;
    FIntf2 : IDynamicRecord;

    procedure CreateContact;
    procedure CreateTestClass;
    procedure CreateTestRecord;
    procedure CreateTestTRecord;
    procedure CreateTestManagedClass;
    function CreateTestDataSet: TClientDataSet;

    procedure Changed;
    procedure UpdateControls;

    procedure ExecuteFromDataSet;
    procedure ExecuteAssignTContact;
    procedure ExecuteAssignTTestClass;
    procedure ExecuteAssignTTestRecord;
    procedure ExecuteAssignTRecord;

    procedure ExecuteAssignFieldValueToFRecord1;
    procedure ExecuteAssignFieldValueToFRecord2;
    procedure ExecuteAssignFieldValueToFDynamicRecord1;
    procedure ExecuteAssignFieldValueToFDynamicRecord2;

    procedure ExecuteAssignFRecord1ToFRecord2;
    procedure ExecuteAssignFRecord1ToFDynamicRecord1;
    procedure ExecuteAssignFRecord1ToFDynamicRecord2;

    procedure ExecuteAssignFRecord2ToFRecord1;
    procedure ExecuteAssignFRecord2ToFDynamicRecord1;
    procedure ExecuteAssignFRecord2ToFDynamicRecord2;

    procedure ExecuteAssignFDynamicRecord1ToFRecord1;
    procedure ExecuteAssignFDynamicRecord1ToFRecord2;
    procedure ExecuteAssignFDynamicRecord1ToFDynamicRecord2;

    procedure ExecuteAssignFDynamicRecord2ToFRecord1;
    procedure ExecuteAssignFDynamicRecord2ToFRecord2;
    procedure ExecuteAssignFDynamicRecord2ToFDynamicRecord1;

  protected
    procedure UpdateActions; override;

    procedure UpdateRefCountDisplay;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

uses
  System.TypInfo,
  Vcl.Dialogs,

  DDuce.Utils, DDuce.RandomData;

{$REGION 'construction and destruction'}
procedure TfrmDynamicRecords.AfterConstruction;
begin
  inherited AfterConstruction;
  FValueList                  := TValueList.Create(Self);
  FValueList.Parent           := pnlRecordInspector;
  FValueList.Align            := alClient;
  FValueList.ShowHeader       := False;
  FValueList.ShowHint         := True;
  FValueList.MultiSelect      := False;
  FValueList.BorderStyle      := bsNone;
  FValueList.Editable         := False;
  FStrings   := TStringList.Create;
  CreateContact;
  CreateTestClass;
  CreateTestRecord;
  CreateTestTRecord;
  CreateTestManagedClass;
  FDataSet := CreateTestDataSet;
  dscTest.DataSet := FDataSet;
  FRec1.Clear;
  FIntf1 := DynamicRecord.CreateDynamicRecord;
  FIntf2 := DynamicRecord.CreateDynamicRecord;
  Changed;
end;

procedure TfrmDynamicRecords.BeforeDestruction;
begin
  FreeAndNil(FDataSet);
  FreeAndNil(FStrings);
  FreeAndNil(FContact);
  FreeAndNil(FTestClass);
  FIntf1 := nil;
  FIntf2 := nil;
  inherited BeforeDestruction;
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
  else if pgcMain.ActivePage = tsDynamicRecord then
  begin
    ExecuteAssignTRecord;
  end
end;

procedure TfrmDynamicRecords.actAssignFDynamicRecord1ToFDynamicRecord2Execute(
  Sender: TObject);
begin
  ExecuteAssignFDynamicRecord1ToFDynamicRecord2;
end;

procedure TfrmDynamicRecords.actAssignFRecord1ToFDynamicRecord1Execute(
  Sender: TObject);
begin
  ExecuteAssignFRecord1ToFDynamicRecord1;
end;

procedure TfrmDynamicRecords.actAssignFRecord1ToFRecord2Execute(
  Sender: TObject);
begin
  ExecuteAssignFRecord1ToFRecord2;
end;

procedure TfrmDynamicRecords.actAssignFieldValueToDynamicRecord1Execute(
  Sender: TObject);
begin
  ExecuteAssignFieldValueToFDynamicRecord1;
end;

procedure TfrmDynamicRecords.actAssignFieldValueToDynamicRecord2Execute(
  Sender: TObject);
begin
  ExecuteAssignFieldValueToFDynamicRecord2;
end;

procedure TfrmDynamicRecords.actAssignFieldValueToFRecord1Execute(Sender: TObject);
begin
  ExecuteAssignFieldValueToFRecord1;
end;

procedure TfrmDynamicRecords.actAssignFieldValueToFRecord2Execute(Sender: TObject);
begin
  ExecuteAssignFieldValueToFRecord2;
end;

procedure TfrmDynamicRecords.actAssignFRecord2ToFDynamicRecord2Execute(
  Sender: TObject);
begin
  ExecuteAssignFRecord2ToFDynamicRecord2;
end;

procedure TfrmDynamicRecords.actAssignFRecord2ToFDynamicRecord1Execute(
  Sender: TObject);
begin
  ExecuteAssignFRecord2ToFDynamicRecord1;
end;

procedure TfrmDynamicRecords.actAssignFRecord1ToFDynamicRecord2Execute(
  Sender: TObject);
begin
  ExecuteAssignFRecord1ToFDynamicRecord2;
end;

procedure TfrmDynamicRecords.actAssignFDynamicRecord1ToFRecord2Execute(
  Sender: TObject);
begin
  ExecuteAssignFDynamicRecord1ToFRecord2;
end;

procedure TfrmDynamicRecords.actAssignFDynamicRecord1ToFRecord1Execute(
  Sender: TObject);
begin
  ExecuteAssignFDynamicRecord1ToFRecord1;
end;

procedure TfrmDynamicRecords.actAssignFDynamicRecord2ToFRecord2Execute(
  Sender: TObject);
begin
  ExecuteAssignFDynamicRecord2ToFRecord2;
end;

procedure TfrmDynamicRecords.actAssignFDynamicRecord2ToFDynamicRecord1Execute(
  Sender: TObject);
begin
  ExecuteAssignFDynamicRecord2ToFDynamicRecord1;
end;

procedure TfrmDynamicRecords.actAssignFDynamicRecord2ToFRecord1Execute(
  Sender: TObject);
begin
  ExecuteAssignFDynamicRecord2ToFRecord1;
end;

procedure TfrmDynamicRecords.actAssignFRecord2ToFRecord1Execute(
  Sender: TObject);
begin
  ExecuteAssignFRecord2ToFRecord1;
end;

procedure TfrmDynamicRecords.actCustomTestExecute(Sender: TObject);
var
  R  : DynamicRecord;
  Rg : DynamicRecord<TContact>;
  //Rn : TRecord;
  //F : IDynamicField;
  //O : TContact;
begin
  CreateContact;
  R.From(FContact);
  ShowMessage(R.ToString);
//  R.Data.FirstName := 'John';
//  R.Data.LastName  := 'Doe';
  ShowMessage(Rg.ToString);
end;

procedure TfrmDynamicRecords.actFDynamicRecord1ClearExecute(Sender: TObject);
begin
  if Assigned(FIntf1) then
  begin
    FIntf1.Clear;
    Changed;
  end;
end;

procedure TfrmDynamicRecords.actFDynamicRecord2ClearExecute(Sender: TObject);
begin
  if Assigned(FIntf2) then
  begin
    FIntf2.Clear;
    Changed;
  end;
end;

procedure TfrmDynamicRecords.actFRecord1ClearExecute(Sender: TObject);
begin
  FRec1.Clear;
  Changed;
end;

procedure TfrmDynamicRecords.actFRecord2ClearExecute(Sender: TObject);
begin
  FRec2.Clear;
  Changed;
end;

procedure TfrmDynamicRecords.actTestDataExecute(Sender: TObject);
var
  V: Variant;
begin
  V := 50;
  // assign arbitrary data
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
var
  R: DynamicRecord;
begin
  FTestTRecord.Data.SomeString   := 'FTestTRecord.Data.SomeString';
  FTestTRecord.Data.SomeDateTime := Now;
  FTestTRecord.Data.Number       := 5;
  FTestTRecord.Data.Bool         := True;
  R := DynamicRecord.Create;
  R.Assign(FTestTRecord);
  R.Data.Number := 6;
end;

procedure TfrmDynamicRecords.CreateTestClass;
begin
  if Assigned(FTestClass) then
    FreeAndNil(FTestClass);
  FTestClass := TTestClass.Create;
  FTestClass.TestString := 'string';
  FTestClass.TestNullableString := 'NullableString';
  FTestClass.TestNullableDateTime := nil;
end;

function TfrmDynamicRecords.CreateTestDataSet: TClientDataSet;
var
  DS           : TClientDataSet;
  I            : Integer;
  LRecordCount : Integer;
begin
  LRecordCount := 1000;
  DS := TClientDataSet.Create(nil);
  with DS.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Name     := 'FirstName';
  end;
  with DS.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Name     := 'LastName';
  end;
  with DS.FieldDefs.AddFieldDef do
  begin
    DataType := ftInteger;
    Name     := 'Age';
  end;
  with DS.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Size     := 40;
    Name     := 'CompanyName';
  end;
  with DS.FieldDefs.AddFieldDef do
  begin
    DataType := ftString;
    Size     := 80;
    Name     := 'Address';
  end;
  DS.CreateDataSet;
  for I := 0 to Pred(LRecordCount) do
  begin
    DS.Append;
    DS.FieldByName('FirstName').AsString   := RandomData.FirstName;
    DS.FieldByName('LastName').AsString    := RandomData.LastName;
    DS.FieldByName('Age').AsInteger        := RandomData.Number(20, 68);
    DS.FieldByName('CompanyName').AsString := RandomData.CompanyName;
    DS.FieldByName('Address').AsString     := RandomData.Address;
    DS.Post;
  end;
  DS.First;
  Result := DS;
end;

procedure TfrmDynamicRecords.CreateTestManagedClass;
begin
  FTestManagedClass.Data.TestBoolean  := True;
  FTestManagedClass.Data.TestChar     := 'Y';
  FTestManagedClass.Data.TestDateTime := Now;
  FTestManagedClass.Data.TestInteger  := 5;
  FTestManagedClass.Data.TestString   := 'foo';
end;

procedure TfrmDynamicRecords.CreateContact;
begin
  if Assigned(FContact) then
    FreeAndNil(FContact);
  FContact := TContact.Create;
  FContact.FirstName := 'John';
  FContact.LastName  := 'Doe';
  FContact.CompanyName := 'Calligram Laboratories';
  FContact.Address   := 'Highway''s creek 48';
  FContact.Country   := 'USA';
  FContact.Number    := 5;
  FContact.BirthDate := Now - 20000;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmDynamicRecords.ExecuteAssignTContact;
begin
  FRecord.From(FContact);
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignTRecord;
begin
  FRecord.Assign(FTestTRecord);
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignTTestClass;
begin
  FRecord.From(FTestClass);
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignTTestRecord;
begin
  FRecord.From(FTestRecord);
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignFieldValueToFDynamicRecord1;
begin
  FIntf1[edtFieldName.Text] := edtValue.Text;
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignFieldValueToFDynamicRecord2;
begin
  FIntf2[edtFieldName.Text] := edtValue.Text;
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignFieldValueToFRecord1;
begin
  FRec1[edtFieldName.Text] := edtValue.Text;
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignFieldValueToFRecord2;
begin
  FRec2[edtFieldName.Text] := edtValue.Text;
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignFRecord2ToFRecord1;
begin
  FRec1 := FRec2;
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignFDynamicRecord1ToFRecord1;
begin
  FRec1 := FIntf1;
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignFDynamicRecord2ToFRecord1;
begin
  FRec1 := FIntf2;
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignFRecord1ToFRecord2;
begin
  FRec2 := FRec1;
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignFDynamicRecord1ToFRecord2;
begin
  FRec2 := FIntf1;
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignFDynamicRecord2ToFRecord2;
begin
  FRec2 := FIntf2;
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignFRecord1ToFDynamicRecord1;
begin
  FIntf1 := FRec1;
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignFRecord2ToFDynamicRecord1;
begin
  FIntf1 := FRec2;
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignFDynamicRecord2ToFDynamicRecord1;
begin
  FIntf1 := FIntf2;
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignFRecord1ToFDynamicRecord2;
begin
  FIntf2 := FRec1;
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignFRecord2ToFDynamicRecord2;
begin
  FIntf2 := FRec2;
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteAssignFDynamicRecord1ToFDynamicRecord2;
begin
  FIntf2 := FIntf1;
  Changed;
end;

procedure TfrmDynamicRecords.ExecuteFromDataSet;
begin
  FRecord.FromDataSet(FDataSet);
  Changed;
end;

procedure TfrmDynamicRecords.Changed;
begin
  FUpdate := True;
end;

procedure TfrmDynamicRecords.UpdateActions;
begin
  inherited UpdateActions;
  UpdateControls;
  UpdateRefCountDisplay;
end;

procedure TfrmDynamicRecords.UpdateControls;
begin
  if FUpdate then
  begin
//    FInspector.Rows.Count := FRecord.Count;
    //FInspector.InvalidateGrid;
    FValueList.Data        := FRecord;
    lblContact.Caption     := AsPropString(FContact);
    lblTestClass.Caption   := AsPropString(FTestClass);
    lblTestRecord.Caption  := AsFieldString(TValue.From(FTestRecord));
    lblTestTRecord.Caption := FTestTRecord.ToString;

    FRecord.ToStrings(FStrings);
    mmoAsCommaText.Text     := FRecord.ToCommaText;
    mmoAsDelimitedText.Text := FRecord.ToDelimitedText(
      edtDelimiter.Text,
      chkQuoteValues.Checked,
      edtQuoteChar.Text[1]
    );
    mmoToString.Text  := FRecord.ToString(chkAlignValues.Checked);
    mmoToStrings.Text := FStrings.Text;

    UpdateRefCountDisplay;
    FUpdate := False;
  end;
end;

procedure TfrmDynamicRecords.UpdateRefCountDisplay;
begin
  if Assigned(FIntf1) then
  begin
    lblFDynamicRecord1.Caption := FIntf1.ToString;
  end;
  if Assigned(FIntf2) then
  begin
    lblFDynamicRecord2.Caption := FIntf2.ToString;
  end;
  lblFRecord1.Caption      := FRec1.ToString;
  lblFRecord2.Caption      := FRec2.ToString;
end;
{$ENDREGION}

end.
