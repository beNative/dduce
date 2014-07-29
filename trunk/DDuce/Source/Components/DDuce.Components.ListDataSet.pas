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

{
  This code depends on units in the DSharp library for Delphi by Stefan Glienke.
  You may obtain a copy of this library at
  http://code.google.com/p/delphisorcery/
}

unit DDuce.Components.ListDataSet;

{$I ..\DDuce.inc}

interface

uses
  System.Classes, System.SysUtils, System.Rtti,
  Data.Db,

{$IFDEF SPRING}
  Spring, Spring.Collections,
{$ENDIF}

{$IFDEF DSHARP}
  DSharp.Collections, DSharp.Core.Events,
{$ENDIF}

  DDuce.Components.VirtualDataSet;

type
  TListDataset = class(TCustomVirtualDataset, IList)
  strict private
    FList             : IList;
    FDataPropertyName : string;
    FUpdateCount      : Integer;

    procedure SetList(const Value: IList);
    procedure SetDataPropertyName(Value: string);
    function GetCurrent: TValue;

    procedure LoadFieldDefsFromRtti(
      AList      : IList;
      AFieldDefs : TFieldDefs
    );

  protected
    function GetRecordCount: Integer; override;

    // Overrides
    procedure DoDeleteRecord(Index: Integer); override;
    procedure DoGetFieldValue(
          Field : TField;
          Index : Integer;
      var Value : TValue
    ); override;
    procedure DoPostData(Index: Integer); override;

    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;
    procedure MasterChanged(Sender: TObject); override;
    procedure RefreshDataListFromSource;
    procedure OnListChanged(Sender: TObject);

  public
    constructor Create(
      AOwner : TComponent;
      AList  : IList = nil
    ); reintroduce; virtual;
    procedure BeforeDestruction; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    function Locate(
      const KeyFields : string;
      const KeyValues : Variant;
            Options   : TLocateOptions
    ): Boolean; override;

  published
    property List: IList
      read FList write SetList implements IList;

    property Current: TValue
      read GetCurrent;

    property DataPropertyName: string
      read FDataPropertyName write SetDataPropertyName;

    property MasterSource;
    property FieldDefs;
    property Fields;

    // Events
    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterRefresh;
    property AfterScroll;
    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeRefresh;
    property BeforeScroll;
    property OnDeleteError;
    property OnDeleteRecord;
    property OnEditError;
    property OnFilterRecord;
    property OnGetFieldValue;
    property OnGetRecordCount;
    property OnNewRecord;
    property OnLookupValue;
    property OnLocate;
    property OnPostData;
    property OnPostError;
  end;

  TListDataset<T> = class(TListDataSet, IList<T>)
  strict private
    FList : IList<T>;

    procedure SetList(const Value: IList<T>);
    function GetCurrent: T;

    procedure CollectionChanged(Sender: TObject; const Item: T;
      Action: TCollectionChangedAction);

  public
    constructor Create(
      AOwner : TComponent;
      AList  : IList<T> = nil
    ); reintroduce; virtual;
    procedure BeforeDestruction; override;

  published
    property List: IList<T>
      read FList write SetList implements IList<T>;

    property Current: T
      read GetCurrent;
  end;

implementation

uses
  System.TypInfo,

  DDuce.Reflect, DDuce.Logger;

resourcestring
  SDataListNotAssigned = 'List property not set, cannot open Dataset.';

{$REGION 'TListDataset'}
{$REGION 'construction and destruction'}
constructor TListDataset.Create(AOwner: TComponent; AList: IList);
begin
  inherited Create(AOwner);
  FList := AList;
end;

procedure TListDataset.BeforeDestruction;
begin
  FList := nil;
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TListDataset.SetList(const Value: IList);
begin
  if Assigned(Value) then
  begin
    FList := Value;
    InternalOpen;
  end;
end;

procedure TListDataset.SetDataPropertyName(Value: string);
begin
  CheckInactive;
  if Value <> FDataPropertyName then
    FDataPropertyName := Value;
end;

function TListDataset.GetCurrent: TValue;
begin
  if Assigned(List) then
    Result := List[CurrentRecord]
  else
    Result := TValue.Empty;
end;

function TListDataset.GetRecordCount: Integer;
begin
  if FList <> nil then
    Result := FList.Count
  else
    Result := -1;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TListDataset.DoDeleteRecord(Index: Integer);
begin
  BeginUpdate;
  try
    FList.Delete(Index);
  finally
    EndUpdate;
  end;
end;

procedure TListDataset.DoGetFieldValue(Field: TField; Index: Integer;
  var Value: TValue);
var
  O: TObject;
begin
  if Index < FList.Count then
  begin
    O := FList[Index].AsObject;
    if Field.Index >= 0 then
      Value := Reflect.Properties(O).Values[Field.FieldName]
    else
      Value := TValue.Empty;
  end;
end;

procedure TListDataset.DoPostData(Index: Integer);
var
  O  : TObject;
  F  : TField;
  TD : PTypeData;
begin
  O := nil;
  BeginUpdate;
  try
    if State in dsEditModes then
    begin
      if State = dsEdit then
      begin
        O := FList[Index].AsObject;
      end
      else if State = dsInsert then
      begin
        {$IFDEF DSHARP}
        TD := GetTypeData(FList.ItemType);
        {$ENDIF}
        {$IFDEF SPRING}
        TD := GetTypeData(FList.ElementType);
        {$ENDIF}
        O := TD.ClassType.Create;
        // in newer Delphis we can use:
        //    O := FList.ItemType.TypeData.ClassType.Create
        if Index <> -1 then
          FList.Insert(Index, O)
        else
          FList.Add(O);
      end;
      for F in ModifiedFields do
      begin
        Reflect.Properties(O).Values[F.FieldName] :=
          TValue.FromVariant(F.AsVariant);
      end;
    end;
  finally
    EndUpdate;
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TListDataset.LoadFieldDefsFromRtti(AList: IList;
  AFieldDefs: TFieldDefs);
var
  C  : TRttiContext;
  P  : TRttiProperty;
  FT : TFieldType;
  FS : Integer;
  FD : TFieldDef;
  S  : string;
  T  : PTypeInfo;
begin
  if not Assigned(AList) then
    Exit;

  {$IFDEF DSHARP}
  T := AList.ItemType;
  {$ENDIF}
  {$IFDEF SPRING}
  T := AList.ElementType;
  {$ENDIF}

  for P in C.GetType(T).GetProperties do
  begin
    FT := ftUnknown;
    FS := 0;
    S := P.PropertyType.QualifiedName;
    case P.PropertyType.TypeKind of
      tkInteger:
      begin
        FT := ftInteger;
      end;
      tkEnumeration:
      begin
        if S = 'System.Boolean' then
          FT := ftBoolean
        else
        begin
          FT := ftWideString;
          FS := 250;
        end;
      end;
      tkFloat:
      begin
        if S = 'System.TDateTime' then
          FT := ftDateTime
        else if S = 'System.TDate' then
          FT := ftDate
        else if S = 'System.TTime' then
          FT := ftTime
        else
          FT := ftFloat;
      end;
      tkChar:
      begin
        FT := ftFixedChar;
        FS := 1;
      end;
      tkWChar:
      begin
        FT := ftFixedWideChar;
        FS := 2;
      end;
      tkString, tkLString:
      begin
        FT := ftString;
        FS := 2500;
      end;
      tkWString, tkUString:
      begin
        FT := ftWideString;
        FS := 2500;
      end;
      tkSet:
      begin
        FT := ftWideString;
        FS := 250;
      end;
      tkArray, tkVariant:
      begin
        FT := ftVariant;
      end;
      tkInt64:
      begin
        FT := ftLargeint;
      end;
      tkClass, tkMethod, tkRecord, tkInterface, tkDynArray, tkClassRef,
        tkPointer, tkProcedure, tkUnknown:
      begin
        FT := ftUnknown;
      end;
    end;

    if FT <> ftUnknown then // create Field definitiion for current property
    begin
      FD             := AFieldDefs.AddFieldDef;
      FD.Name        := P.Name;
      FD.DisplayName := P.Name;
      FD.DataType    := FT;
      if FS <> 0 then
        FD.Size := FS;
      if not P.IsWritable then
        FD.Attributes := FD.Attributes + [faReadonly];
    end;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TListDataset.InternalInitFieldDefs;
begin
  FieldDefs.Clear;
  if Fields.Count > 0 then
    LoadFieldDefsFromFields(Fields, FieldDefs)
  else
    LoadFieldDefsFromRtti(FList, FieldDefs);
end;

procedure TListDataset.InternalOpen;
begin
  if Assigned(MasterSource) and (FDataPropertyName <> '') then
    RefreshDataListFromSource
  else if not Assigned(List) then
    VirtualDatasetError(SDataListNotAssigned, Self);

  InternalFirst;

  BookmarkSize := SizeOf(Integer);

  FieldDefs.Updated := False;
  FieldDefs.Update;

  Fields.Clear;
  //if DefaultFields then
    CreateFields;

  BindFields(True);
  RecordBufferSize := SizeOf(TRecordInfo) + (Fields.Count * SizeOf(Variant));
end;

function TListDataset.IsCursorOpen: Boolean;
begin
  Result := FList <> nil;
end;

procedure TListDataset.MasterChanged(Sender: TObject);
begin
  RefreshDataListFromSource;
  inherited;
end;

procedure TListDataset.OnListChanged(Sender: TObject);
begin
  if Active then
    Refresh;
end;

procedure TListDataset.RefreshDataListFromSource;
var
  F : TField;
  I : IInterface;
  L : IList;
begin
  FList := nil;
  if (MasterSource <> nil) and (FDataPropertyName <> '') then
  begin
    F := MasterDataLink.Dataset.FindField(FDataPropertyName);
    if (F <> nil) and (F is TInterfaceField) then
    begin
      I := (F as TInterfaceField).Value;
      if Supports(I, IList, L) then
        FList := L;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TListDataset.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TListDataset.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
end;

function TListDataset.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
begin
  Result := False;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TListDataset<T>'}
{$REGION 'construction and destruction'}
constructor TListDataset<T>.Create(AOwner: TComponent; AList: IList<T>);
begin
  inherited Create(AOwner);
  List := AList;
end;

procedure TListDataset<T>.BeforeDestruction;
begin
  if Assigned(FList) then
    {$IFDEF DSHARP}
    FList.OnCollectionChanged.Remove(CollectionChanged);
    {$ENDIF}
    {$IFDEF SPRING}
    FList.OnChanged.Remove(CollectionChanged);
    {$ENDIF}
  FList := nil;
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TListDataset<T>.SetList(const Value: IList<T>);
begin
  if Value <> List then
  begin
    if Assigned(List) then
      {$IFDEF DSHARP}
      FList.OnCollectionChanged.Remove(CollectionChanged);
      {$ENDIF}
      {$IFDEF SPRING}
      FList.OnChanged.Remove(CollectionChanged);
      {$ENDIF}
    FList := Value;
    {$IFDEF DSHARP}
    FList.OnCollectionChanged.Add(CollectionChanged);
    {$ENDIF}
    {$IFDEF SPRING}
    FList.OnChanged.Add(CollectionChanged);
    {$ENDIF}
    inherited List := Value.AsList;
  end;
end;

function TListDataset<T>.GetCurrent: T;
begin
  if Assigned(List) then
    Result := List[CurrentRecord]
  else
    Result := Default(T);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TListDataset<T>.CollectionChanged(Sender: TObject; const Item: T;
  Action: TCollectionChangedAction);
begin
  if Active and not (State in dsEditModes) then
    Refresh;
end;
{$ENDREGION}
{$ENDREGION}

end.
