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

interface

uses
  System.SysUtils, System.Classes, System.Contnrs,

  DDuce.Editor.Utils, DDuce.Editor.CodeFormatters, DDuce.Editor.CodeTags,

  DDuce.Logger;

type
  //TSynHighlighterClass = class of TSynCustomHighlighter;
  THighlighters        = class;

  { THighlighterItem }

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
//    FSynHighlighter       : TSynCustomHighlighter;
//    FSynHighlighterClass  : TSynHighlighterClass;
    FFileExtensions       : TStringList;
    FUseCommonAttributes  : Boolean;

    // private property access methods
    function GetDefaultFilter: string;
    function GetFileExtensions: string;
    function GetIndex: Integer;
//    function GetSynHighlighter: TSynCustomHighlighter;
    procedure SetDefaultFilter(AValue: string);
    procedure SetFileExtensions(AValue: string);
    procedure SetFormatterSupport(const AValue: Boolean);
    procedure SetSmartSelectionTags(AValue: TCodeTags);
//    procedure SetSynHighlighterClass(AValue: TSynHighlighterClass);
    procedure SetUseCommonAttributes(AValue: Boolean);

  public
    // constructors and destructors
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

//    procedure InitSynHighlighter(
//      { Optional instance to assign the settings to. }
//      ASynHighlighter: TSynCustomHighlighter = nil
//    );
    procedure Assign(Source: TPersistent); override;
    function AsString: string;

//    property SynHighlighterClass: TSynHighlighterClass
//      read FSynHighlighterClass write SetSynHighlighterClass;

    property CodeFormatter: ICodeFormatter
      read FCodeFormatter write FCodeFormatter;

//    property SynHighlighter: TSynCustomHighlighter
//      read GetSynHighlighter;

    property Index: Integer
      read GetIndex;

    { Set of tags that are used for the SmartSelect feature of the editor. }
    property SmartSelectionTags: TCodeTags
      read FSmartSelectionTags write SetSmartSelectionTags;

  published
    property Highlighter: string
      read FHighlighter write FHighlighter;

    { Character sequence that designates the start of a block comment. }
    property BlockCommentStartTag: string
      read FBlockCommentStartTag write FBlockCommentStartTag;

    { Character sequence that designates the end of a block comment. }
    property BlockCommentEndTag: string
      read FBlockCommentEndTag write FBlockCommentEndTag;

    { Character sequence that designates a line comment. }
    property LineCommentTag: string
      read FLineCommentTag write FLineCommentTag;

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

    { Assign common attribute settings. }
    property UseCommonAttributes: Boolean
      read FUseCommonAttributes write SetUseCommonAttributes default True;
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
    function AsString: string;
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

function THighlighters.AsString: string;
var
  I : Integer;
  S : string;
begin
  S := '';
  for I :=  0 to Count - 1 do
  begin
    S := S + LineEnding + Items[I].AsString;
  end;
  Result := S;
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
  //FSmartSelectionTags.Name := 'SmartSelectionTags';

  FUseCommonAttributes       := True;
end;

procedure THighlighterItem.BeforeDestruction;
begin
  FCodeFormatter := nil;
  //if Assigned(FSynHighlighter) then
  //  FreeAndNil(FSynHighlighter);
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

//function THighlighterItem.GetSynHighlighter: TSynCustomHighlighter;
//var
//  I : Integer;
//  B : Boolean;
//begin
//  if (FSynHighlighter=nil) and (ComponentCount > 0) then
//  begin
//    I := 0;
//    B := False;
//    while not B and (I < ComponentCount) do
//    begin
//      if Components[I] is TSynCustomHighlighter then
//      begin
//        B := True;
//        FSynHighlighter := TSynCustomHighlighter(Components[0]);
//      end
//      else
//        Inc(B);
//    end;
//  end;
//  Result := FSynHighlighter;
//end;

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

//procedure THighlighterItem.SetSynHighlighterClass(AValue: TSynHighlighterClass);
//begin
//  if AValue <> SynHighlighterClass then
//  begin
//    FSynHighlighterClass := AValue;
//  end;
//end;

procedure THighlighterItem.SetUseCommonAttributes(AValue: Boolean);
begin
  if AValue <> UseCommonAttributes then
  begin
    FUseCommonAttributes := AValue;
  end;
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
//    SynHighlighterClass  := HLI.SynHighlighterClass;
//    SynHighlighter.Assign(HLI.SynHighlighter);
    SmartSelectionTags.Assign(HLI.SmartSelectionTags);
    Highlighter          := HLI.Highlighter;
    Description          := HLI.Description;
    LayoutFileName       := HLI.LayoutFileName;
    BlockCommentEndTag   := HLI.BlockCommentEndTag;
    BlockCommentStartTag := HLI.BlockCommentStartTag;
    LineCommentTag       := HLI.LineCommentTag;
    FileExtensions       := HLI.FileExtensions;
    SmartSelectionTags   := HLI.SmartSelectionTags;
    DefaultFilter        := HLI.DefaultFilter;
    FormatterSupport     := HLI.FormatterSupport;
    UseCommonAttributes  := HLI.UseCommonAttributes;
  end
  else
    inherited Assign(Source);
end;

function THighlighterItem.AsString: string;
const
  DATA =
    'SynHighlighter = %s' + #13#10 +
    'Name           = %s' + #13#10 +
    'Description    = %s' + #13#10 +
    'LayoutFileName = %s';
begin
  //Result := Format(DATA, [SynHighlighter.ClassName, Highlighter, Description, LayoutFileName]);
end;

//procedure THighlighterItem.InitSynHighlighter(ASynHighlighter: TSynCustomHighlighter);
//begin
//  if not Assigned(SynHighlighter) and Assigned(SynHighlighterClass) then
//  begin
//    FSynHighlighter := SynHighlighterClass.Create(Self);
//    FSynHighlighter.Name := 'SynHighlighter';
//  end;
//  if Assigned(ASynHighlighter) then
//    SynHighlighter.Assign(ASynHighlighter);
//
//  if (SynHighlighterClass = TSynUniSyn) and FileExistsUTF8(LayoutFileName) then
//  begin
//    if Assigned(SynHighlighter) then
//      TSynUniSyn(SynHighlighter).LoadFromFile(LayoutFileName);
//  end;
//end;
{$ENDREGION}
{$ENDREGION}

initialization
  RegisterClass(THighlighters);
  RegisterClass(THighlighterItem);

end.

