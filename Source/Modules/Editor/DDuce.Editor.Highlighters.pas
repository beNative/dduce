{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit DDuce.Editor.Highlighters;

{ Holds settings that are specific to each supported highlighter. }

{ REMARKS about TBCEditorHighlighter:
  - a TBCEditorHighlighter requires a TBCEditor component to be able to load
    its properties from the definition file.
  - It uses a TBCEditorHighlighterJSONImporter object to load its settings. It
    is not possible to save its settings back to JSON.
  - It is not intended as a persistable settings object, but is rather a helper
    object that works closely with the associated editor instance to render its
    text.

 }

interface

uses
  System.SysUtils, System.Classes, System.Contnrs,

  DDuce.Editor.Utils, DDuce.Editor.CodeFormatters, DDuce.Editor.CodeTags,

  BCEditor.Highlighter,

  DDuce.Logger;

type
  THighlighters        = class;

  { THighlighterItem  }

  THighlighterItem = class(TComponent)
  private
    FBlockCommentEndTag   : string;
    FBlockCommentStartTag : string;
    FCodeFormatter        : ICodeFormatter;
    FDescription          : string;
    FFormatterSupport     : Boolean;
    FLayoutFileName       : string;
    FLineCommentTag       : string;
    FDefaultFilter        : string;
    FHighlighter          : string;
    FSmartSelectionTags   : TCodeTags;
    FFileExtensions       : TStringList;
    FUseCommonAttributes  : Boolean;
    FBCEditorHighlighter  : TBCEditorHighlighter;

    // private property access methods
    function GetDefaultFilter: string;
    function GetFileExtensions: string;
    function GetIndex: Integer;
    procedure SetDefaultFilter(AValue: string);
    procedure SetFileExtensions(AValue: string);
    procedure SetFormatterSupport(const AValue: Boolean);
    procedure SetSmartSelectionTags(AValue: TCodeTags);

  public
    // constructors and destructors
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Assign(Source: TPersistent); override;

    property CodeFormatter: ICodeFormatter
      read FCodeFormatter write FCodeFormatter;

    property Index: Integer
      read GetIndex;

    { Set of tags that are used for the SmartSelect feature of the editor. }
    property SmartSelectionTags: TCodeTags
      read FSmartSelectionTags write SetSmartSelectionTags;

  published
    property Highlighter: string
      read FHighlighter write FHighlighter;

    property FormatterSupport: Boolean
      read FFormatterSupport write SetFormatterSupport;

    { Comma seperated list of file extensions associated with the highlighter. }
    property FileExtensions: string
      read GetFileExtensions write SetFileExtensions;

    property DefaultFilter: string
      read GetDefaultFilter write SetDefaultFilter;

    property Description: string
      read FDescription write FDescription;

    property LayoutFileName: string
      read FLayoutFileName write FLayoutFileName;

  end;

  THighlighterItemClass = class of THighlighterItem;

  { THighlighters }

  THighlighters = class(TComponent)
  type
    THighlighterEnumerator = class
    private
      FHighlighters : THighlighters;
      FPosition     : Integer;

      function GetCurrent: THighlighterItem;
    public
      constructor Create(AHighlighters: THighlighters);

      function MoveNext: Boolean;

      property Current: THighlighterItem
        read GetCurrent;
    end;

    // property access methods
    function GetCount: Integer;
    function GetFileFilter: string;
    function GetItem(Index: Integer): THighlighterItem;
    function GetItemByName(const AName: string): THighlighterItem;
    procedure SetItem(Index: Integer; const Value: THighlighterItem);
    procedure SetItemByName(const AName: string; const AValue: THighlighterItem);

  public
    // constructors and destructors
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function Add: THighlighterItem;
    function Exists(const AName: string): Boolean;

    function GetEnumerator: THighlighterEnumerator;

    function IndexOf(const AName: string): Integer; virtual;
    function Find(const AName: string): THighlighterItem;
    function FindHighlighterForFileType(const AFileExt: string): THighlighterItem;

//    procedure RegisterHighlighter(
//            ASynHighlighterClass  : TSynHighlighterClass;
//            ASynHighlighter       : TSynCustomHighlighter;  // To ASSIGN the settings!!!
//      const AName                 : string;       // unique name
//      const AFileExtensions       : string = '';  // comma separated list
//      const ALineCommentTag       : string = '';
//      const ABlockCommentStartTag : string = '';
//      const ABlockCommentEndTag   : string = '';
//      const ACodeFormatter        : ICodeFormatter = nil;
//      const ADescription          : string = '';  // highlighter description
//      const ALayoutFileName       : string = ''   // only for TSynUNIHighlighter
//    ); virtual;

    // public properties
    { Provides indexed access to the list of items. }
    property Items[Index: Integer]: THighlighterItem
      read GetItem write SetItem; default;

    property Count: Integer
      read GetCount;

    property ItemsByName[const AName: string]: THighlighterItem
      read GetItemByName write SetItemByName;

    property FileFilter: string
      read GetFileFilter;
  end;

implementation

uses
  System.StrUtils,
  Vcl.Forms, Vcl.Dialogs;

{$REGION 'THighlighterEnumerator'}
function THighlighters.THighlighterEnumerator.GetCurrent: THighlighterItem;
begin
  Result := FHighlighters[FPosition];
end;

constructor THighlighters.THighlighterEnumerator.Create(AHighlighters: THighlighters);
begin
  FHighlighters := AHighlighters;
  FPosition := -1;
end;

function THighlighters.THighlighterEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FHighlighters.Count;
end;
{$ENDREGION}

{$REGION 'THighlighters'}
{$REGION 'construction and destruction'}
procedure THighlighters.AfterConstruction;
begin
  inherited AfterConstruction;
end;

procedure THighlighters.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function THighlighters.GetItem(Index: Integer): THighlighterItem;
begin
  Result := Components[Index] as THighlighterItem;
end;

procedure THighlighters.SetItem(Index: Integer; const Value: THighlighterItem);
begin
  Components[Index].Assign(Value);
end;

function THighlighters.GetFileFilter: string;
var
  S: string;
  HI: THighlighterItem;
begin
  S := '';
  for HI in Self do
  begin
    if HI.DefaultFilter <> '' then
      S := S + HI.DefaultFilter + '|';
  end;
  Result := S;
end;

function THighlighters.GetCount: Integer;
begin
  Result := ComponentCount;
end;

function THighlighters.GetItemByName(const AName: string): THighlighterItem;
begin
  Result := Find(AName);
  if not Assigned(Result) then
    Result := Find('None');
end;

procedure THighlighters.SetItemByName(const AName: string; const AValue: THighlighterItem);
var
  Item: THighlighterItem;
begin
  Item := Find(AName);
  if Assigned(Item) then
    Item.Assign(AValue);
end;
{$ENDREGION}

{$REGION 'public methods'}
{ Adds a new THighlighterItem instance to the list. }

function THighlighters.Add: THighlighterItem;
begin
  Result := THighlighterItem.Create(Self);
end;

function THighlighters.Exists(const AName: string): Boolean;
begin
  Result := Assigned(Find(AName));
end;

function THighlighters.GetEnumerator: THighlighterEnumerator;
begin
  Result := THighlighterEnumerator.Create(Self);
end;

function THighlighters.IndexOf(const AName: string): Integer;
var
  I: Integer;
  B: Boolean;
begin
  I := 0;
  B := False;
  while not B and (I < ComponentCount) do
  begin
    B := SameText(Components[I].Name, AName);
    if not B then
      Inc(I);
  end;
  if B then
    Result := I
  else
    Result := -1;
end;

function THighlighters.Find(const AName: string): THighlighterItem;
var
  I: Integer;
begin
  I := IndexOf(AName);
  if I < 0 then
    Result := nil
  else
    Result := Items[I];
end;

{ Finds the corresponding highlighteritem for a given file extension. }

function THighlighters.FindHighlighterForFileType(const AFileExt: string): THighlighterItem;
var
  I  : Integer;
  //HL : TSynCustomHighlighter;
  S  : string;
begin
//  Result := nil;
//  S := LowerCase(AFileExt);
//  for I := 0 to Count - 1 do
//  begin
//    HL :=  Items[I].SynHighlighter;
//    if Assigned(HL) then
//    begin
//      if IsWordPresent(S, Items[I].FileExtensions, [','])
//        or IsWordPresent(S, HL.DefaultFilter, [',','.', ';']) then
//      begin
//        Result := Items[I];
//      end;
//    end;
//  end;
end;

{ Registers a new highlighter or updates an exiting one if the corresponding
  properties are not assigned yet. }

  { TODO: ASynHighlighter is of no use? }

//procedure THighlighters.RegisterHighlighter(ASynHighlighterClass:
//  TSynHighlighterClass; ASynHighlighter: TSynCustomHighlighter;
//  const AName: string; const AFileExtensions: string;
//  const ALineCommentTag: string; const ABlockCommentStartTag: string;
//  const ABlockCommentEndTag: string; const ACodeFormatter: ICodeFormatter;
//  const ADescription: string; const ALayoutFileName: string);
//var
//  HI : THighlighterItem;
//begin
//  HI := Find(AName);
//  if not Assigned(HI) then
//  begin
//    HI := Add;
//    HI.Name        := AName;
//    HI.Highlighter := AName;
//    Logger.Send('Created highlighter %s', [AName]);
//  end;
//  if ADescription <> '' then
//    HI.Description := ADescription;
//  HI.SynHighlighterClass := ASynHighlighterClass;
//  HI.CodeFormatter       := ACodeFormatter;
//  if HI.LineCommentTag = '' then
//    HI.LineCommentTag := ALineCommentTag;
//  if HI.BlockCommentStartTag = '' then
//    HI.BlockCommentStartTag := ABlockCommentStartTag;
//  if HI.BlockCommentEndTag = '' then
//    HI.BlockCommentEndTag   := ABlockCommentEndTag;
//  if HI.LayoutFileName = '' then
//    HI.LayoutFileName := ALayoutFileName;
//  if HI.FileExtensions = '' then
//    HI.FileExtensions := AFileExtensions;
//end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'THighlighterItem'}

{$REGION 'construction and destruction'}
procedure THighlighterItem.AfterConstruction;
begin
  inherited AfterConstruction;
  FFileExtensions            := TStringList.Create;
  FFileExtensions.Duplicates := dupIgnore;
  FFileExtensions.Sorted     := True;
  FSmartSelectionTags        := TCodeTags.Create(nil);

  //FBCEditorHighlighter       := TBCEditorHighlighter.C

  FUseCommonAttributes       := True;
end;

procedure THighlighterItem.BeforeDestruction;
begin
  FCodeFormatter := nil;
  FreeAndNil(FFileExtensions);
  FreeAndNil(FSmartSelectionTags);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function THighlighterItem.GetDefaultFilter: string;
begin
  Result := FDefaultFilter;
end;

procedure THighlighterItem.SetDefaultFilter(AValue: string);
begin
  if AValue <> DefaultFilter then
  begin
    FDefaultFilter := AValue;
  end;
end;

function THighlighterItem.GetIndex: Integer;
begin
  Result := ComponentIndex;
end;

function THighlighterItem.GetFileExtensions: string;
begin
  Result := FFileExtensions.CommaText;
end;

procedure THighlighterItem.SetFileExtensions(AValue: string);
begin
  if AValue <> FileExtensions then
  begin
    FFileExtensions.CommaText := AValue;
  end;
end;

procedure THighlighterItem.SetFormatterSupport(const AValue: Boolean);
begin
  if AValue <> FormatterSupport then
  begin
    FFormatterSupport := AValue;
  end;
end;

procedure THighlighterItem.SetSmartSelectionTags(AValue: TCodeTags);
begin
  FSmartSelectionTags.Assign(AValue);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure THighlighterItem.Assign(Source: TPersistent);
var
  HLI: THighlighterItem;
begin
  if (Source <> Self) and (Source is THighlighterItem) then
  begin
    HLI := THighlighterItem(Source);
    SmartSelectionTags.Assign(HLI.SmartSelectionTags);
    Highlighter          := HLI.Highlighter;
    Description          := HLI.Description;
    LayoutFileName       := HLI.LayoutFileName;
    FileExtensions       := HLI.FileExtensions;
    SmartSelectionTags   := HLI.SmartSelectionTags;
    DefaultFilter        := HLI.DefaultFilter;
    FormatterSupport     := HLI.FormatterSupport;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}
{$ENDREGION}

initialization
  RegisterClass(THighlighters);
  RegisterClass(THighlighterItem);

end.

