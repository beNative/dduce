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

unit DDuce.Editor.Commands;

{ Implements IEditorCommands which represents a set of commands that can be
  executed on the active editor view.

  Commands are intended to be called by actions and are typically associated
  with an IEditorView instance.
}

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils,

  DDuce.Editor.Interfaces;

type

  { TEditorCommands }

  TEditorCommands = class(TComponent, IEditorCommands)
  strict private
    function GetEvents: IEditorEvents;
    function GetManager: IEditorManager;
    function GetSearchEngine: IEditorSearchEngine;
    function GetSelection: IEditorSelection;
    function GetSettings: IEditorSettings;
    function GetView: IEditorView;

    function StripComments(
      const AString      : string;
      const AHighlighter : string
    ): string;
    function MergeBlankLines(const AString: string): string;
    function GuessHighlighterType(
      const AText: string
    ): string; overload;
    function IsXML(
      const AString: string
    ): Boolean;

    function IsPAS(
      const AString: string
    ): Boolean;

    function IsSQL(
      const AString: string
    ): Boolean;

    function IsLOG(
      const AString: string
    ): Boolean;

    function IsLFM(
      const AString: string
    ): Boolean;

    function IsHTML(
      const AString: string
    ): Boolean;

  strict protected
    procedure OpenFileAtCursor;
    procedure ToggleHighlighter;
    procedure AssignHighlighter(const AName: string);
    procedure CopyToClipboard;
    procedure CreateDesktopLink;

    procedure CompressSpace;
    procedure CompressWhitespace;
    procedure UpperCaseSelection;
    procedure LowerCaseSelection;
    procedure PascalStringFromSelection;
    procedure QuoteLinesInSelection(ADelimit : Boolean = False);
    procedure DequoteLinesInSelection;
    procedure QuoteSelection;
    procedure DequoteSelection;
    procedure Base64FromSelection(ADecode: Boolean = False);
    procedure URLFromSelection(ADecode: Boolean = False);
    procedure XMLFromSelection(ADecode: Boolean = False);
    procedure ConvertTabsToSpacesInSelection;
    procedure SyncEditSelection;
    procedure Save;
    function SaveFile(
      const AFileName   : string = '';
            AShowDialog : Boolean = False
    ): Boolean;
    procedure SaveAll;
    procedure AdjustFontSize(AOffset: Integer);

    procedure AlignSelection(
      const AToken                  : string;
            ACompressWS             : Boolean;
            AInsertSpaceBeforeToken : Boolean;
            AInsertSpaceAfterToken  : Boolean;
            AAlignInParagraphs      : Boolean
    );
    procedure MergeBlankLinesInSelection;
    procedure StripCommentsFromSelection;
    procedure StripMarkupFromSelection;
    procedure StripCharsFromSelection(
      AFirst : Boolean;
      ALast  : Boolean
    );
    procedure GuessHighlighterType; overload;
    procedure Indent;
    procedure UnIndent;
    procedure UpdateCommentSelection(ACommentOn, AToggle: Boolean);
    procedure ToggleBlockCommentSelection;
    procedure InsertTextAtCaret(const AText: string);
    procedure FormatCode;
    procedure SortStrings;
    procedure SmartSelect;
    function SelectBlockAroundCursor(
      const AStartTag        : string;
      const AEndTag          : string;
            AIncludeStartTag : Boolean;
            AIncludeEndTag   : Boolean
    ): Boolean;

    procedure FindNext;
    procedure FindPrevious;

    property Selection: IEditorSelection
      read GetSelection;

    property View: IEditorView
      read GetView;

    property Events: IEditorEvents
      read GetEvents;

    property Settings: IEditorSettings
      read GetSettings;

    property SearchEngine: IEditorSearchEngine
      read GetSearchEngine;

    property Manager: IEditorManager
      read GetManager;

  public
    procedure AfterConstruction; override;

  end;

implementation

uses
  Winapi.ShlObj,
  System.Math, System.StrUtils,
  Vcl.Forms,

  //FileUtil, Base64,

  //SynPluginSyncroEdit, SynEditKeyCmds, SynEditTypes,

  BCEditor.Editor.KeyCommands,

  //ts.Core.Utils,

  //DDuce.Editor.Highlighters,
  DDuce.Editor.Resources,
//  ts.Editor.CommentStripper,
//  ts.Editor.SortStrings.Settings,

  DDuce.Editor.Utils;

{$REGION'construction and destruction' /fold}
procedure TEditorCommands.AfterConstruction;
begin
  inherited AfterConstruction;
end;
{$ENDREGION}

{$REGION'property access mehods' /fold}
function TEditorCommands.GetEvents: IEditorEvents;
begin
  Result := Manager.Events;
end;

function TEditorCommands.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

function TEditorCommands.GetSearchEngine: IEditorSearchEngine;
begin
  Result := Manager.SearchEngine;
end;

function TEditorCommands.GetSelection: IEditorSelection;
begin
  Result := Manager.ActiveView.Selection;
end;

function TEditorCommands.GetSettings: IEditorSettings;
begin
  Result := Manager.Settings;
end;

function TEditorCommands.GetView: IEditorView;
begin
  Result := Manager.ActiveView;
end;

{ TODO: Use interfaces like (cfr. ICodeFormatter) }

function TEditorCommands.StripComments(const AString: string;
  const AHighlighter: string): string;
//var
//  SSIn  : TStringStream;
//  SSOut : TStringStream;
//  CS    : TCustomCommentStripper;
//  C     : Char;
//  S     : string;
begin
//  CS := nil;
//  if AnsiMatchStr(AHighlighter, [HL_PAS]) then
//    CS := TPasCommentStripper.Create(nil)
//  else if AnsiMatchStr(AHighlighter, [HL_CPP, HL_JAVA, HL_CS]) then
//    CS := TCPPCommentStripper.Create(nil);
//  if Assigned(CS) then
//  begin
//    try
//      SSIn := TStringStream.Create('');
//      try
//        SSIn.WriteString(AString);
//        C := #0;
//        SSIn.Write(C, 1);
//        SSIn.Position := 0;
//        SSOut := TStringStream.Create('');
//        try
//          CS.InStream  := SSIn;
//          CS.OutStream := SSOut;
//          CS.Parse;
//          SSOut.Position := 0;
//          S := SSOut.ReadString(SSOut.Size);
//          S := MergeBlankLines(S);
//          Result := S;
//        finally
//          SSOut.Free;
//        end;
//      finally
//        SSIn.Free;
//      end;
//    finally
//      CS.Free;
//    end;
//  end
//  else
//    Result := AString;
end;

function TEditorCommands.MergeBlankLines(const AString: string): string;
var
  SL : TStrings;
begin
  SL := TStringList.Create;
  try
    SL.Text := AString;
    DDuce.Editor.Utils.MergeBlankLines(SL);
    // remove first blank line
    if (SL.Count > 0) and (Trim(SL[0]) = '') then
      SL.Delete(0);
    // remove last blank line
    if (SL.Count > 0) and (Trim(SL[SL.Count - 1]) = '') then
      SL.Delete(SL.Count - 1);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TEditorCommands.GuessHighlighterType(const AText: string): string;
var
  SL : TStringList;
  S  : string;
begin
  Result := '';
  if Length(AText) > 0 then
  begin
    SL := TStringList.Create;
    try
      SL.Text := Copy(AText, 0, 2000);
      if SL.Count > 0 then
      begin
        S := Trim(SL[0]);
        if IsXML(S) then
          Result := HL_XML
        else
        begin
          S := SL.Text;
          if IsPAS(S) then
            Result := HL_PAS
          else if IsLFM(S) then
            Result := HL_LFM
        end
        //if IsLOG(AText) then
        //  Result := HL_LOG

        //else if IsHTML(AText) then
        //  Result := HL_HTML
        //else if IsXML(AText) then
        //  Result := HL_XML
        //else if IsSQL(AText) then
        //  Result := HL_SQL;
      end
    finally
      SL.Free;
    end;
  end;
end;

function TEditorCommands.IsXML(const AString: string): Boolean;
const
  MATCH = '^\<\?xml version\=.+\?\>$';
begin
  Result := MatchRegExpr(AString, MATCH, False);
end;

function TEditorCommands.IsPAS(const AString: string): Boolean;
const
  MATCH =  '^(unit|program|package|library) .+;$';
var
  SL : TStrings;
  S  : string;
  B  : Boolean;
begin
  Result := False;
  S := StripComments(AString, HL_PAS);
  SL := TStringList.Create;
  try
    SL.Text := S;
    B := False;
    while not B and (SL.Count > 0) do
    begin
      if Trim(SL[0]) = '' then
      begin
        SL.Delete(0);
        B := False;
      end
      else
        B := True;
    end;
    if SL.Count > 0 then
      S := Trim(SL[0]);
    Result := MatchRegExpr(S, MATCH, False);
  finally
    SL.Free;
  end;
end;

function TEditorCommands.IsSQL(const AString: string): Boolean;
begin
  Result := False;
end;

function TEditorCommands.IsLOG(const AString: string): Boolean;
begin
  Result := False;
end;

function TEditorCommands.IsLFM(const AString: string): Boolean;
begin
  Result := False;
end;

function TEditorCommands.IsHTML(const AString: string): Boolean;
begin
  Result := False;
end;
{$ENDREGION}

{$REGION'protected methods' /fold}
procedure TEditorCommands.OpenFileAtCursor;
var
  FN : string;
begin
  FN := ExtractFilePath(View.FileName)
    + View.CurrentWord + ExtractFileExt(View.FileName);
  if FileExists(FN) then
    Events.DoNew(FN);
end;

procedure TEditorCommands.ToggleHighlighter;
//var
//  I : Integer;
//  N : Integer;
begin
//  if Assigned(View.HighlighterItem) then
//  begin
//    I := View.HighlighterItem.Index;
//    N := Manager.Highlighters.Count;
//    View.HighlighterItem := Manager.Highlighters[(I + 1) mod N];
//    Settings.HighlighterType := View.HighlighterItem.Name;
//  end;
end;

procedure TEditorCommands.AssignHighlighter(const AName: string);
//var
//  HLI : THighlighterItem;
begin
//  if Assigned(Manager.Highlighters) then
//  begin
//    HLI := Manager.Highlighters.ItemsByName[AName];
//    if Assigned(HLI) then
//      View.HighlighterItem := HLI
//    else
//      raise Exception.CreateFmt('Highlighter %s not found!', [AName]);
//  end;
end;

procedure TEditorCommands.CopyToClipboard;
begin
  View.Editor.CopyToClipboard;
end;

procedure TEditorCommands.CreateDesktopLink;
{$IFDEF Windows}
var
  PIDL     : LPItemIDList;
  InFolder : array[0..MAX_PATH] of Char;
  SL       : TShellLink;
{$ENDIF}
begin
{$IFDEF Windows}
  PIDL := nil;
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL) ;
  SHGetPathFromIDList(PIDL, InFolder) ;
  SL.Filename := InFolder + '\' + ExtractFileName(View.FileName) + '.lnk';
  SL.WorkingDir := ExtractFilePath(SL.Filename);
  SL.ShortcutTo := Application.ExeName;
  SL.Parameters := View.FileName;
  CreateShellLink(SL);
{$ENDIF}
end;

procedure TEditorCommands.CompressSpace;
begin
  Selection.Store;
  Selection.Text := DDuce.Editor.Utils.CompressSpace(Selection.Text);
  Selection.Restore;
end;

procedure TEditorCommands.CompressWhitespace;
begin
  Selection.Store;
  Selection.Text := DDuce.Editor.Utils.CompressWhitespace(Selection.Text);
  Selection.Restore;
end;

procedure TEditorCommands.UpperCaseSelection;
begin
  View.Editor.CommandProcessor(ecUpperCaseBlock, #0, nil);


//  Selection.Store;
//  Selection.Text := UpperCase(Selection.Text);
//  Selection.Restore;
//  View.Modified := True;

end;

procedure TEditorCommands.LowerCaseSelection;
begin
  View.Editor.CommandProcessor(ecLowerCaseBlock, #0, nil);
//  Selection.Store;
//  Selection.Text := LowerCase(Selection.Text);
//  Selection.Restore;
//  View.Modified := True;
end;

procedure TEditorCommands.PascalStringFromSelection;
begin
  Selection.Store;
  Selection.Text := PascalStringOf(Selection.Text);
  Selection.Restore;
  View.Modified := True;
end;

procedure TEditorCommands.QuoteLinesInSelection(ADelimit: Boolean);
begin
  Selection.Store;
  if ADelimit then
    Selection.Text := QuoteLinesAndDelimit(Selection.Text)
  else
    Selection.Text := QuoteLines(Selection.Text);
  Selection.Restore;
  View.Modified := True;
end;

procedure TEditorCommands.DequoteLinesInSelection;
begin
  Selection.Store;
  Selection.Text := DequoteLines(Selection.Text);
  Selection.Restore;
  View.Modified := True;
end;

procedure TEditorCommands.QuoteSelection;
begin
  Selection.Store;
  Selection.Text := AnsiQuotedStr(Selection.Text, '''');
  Selection.Restore;
  View.Modified := True;
end;

procedure TEditorCommands.DequoteSelection;
begin
  Selection.Store;
  Selection.Text := AnsiDequotedStr(Selection.Text, '''');
  Selection.Restore;
  View.Modified := True;
end;

procedure TEditorCommands.Base64FromSelection(ADecode: Boolean);
begin
//  Selection.Store(True, True);
//  if ADecode then
//    Selection.Text := DecodeStringBase64(Selection.Text)
//  else
//    Selection.Text := EncodeStringBase64(Selection.Text);
//  Selection.Restore;
//  View.Modified := True;
end;

procedure TEditorCommands.URLFromSelection(ADecode: Boolean);
begin
//  Selection.Store(True, True);
//  if ADecode then
//    Selection.Text := URLDecode(Selection.Text)
//  else
//    Selection.Text := URLEncode(Selection.Text);
//  Selection.Restore;
//  View.Modified := True;
end;

procedure TEditorCommands.XMLFromSelection(ADecode: Boolean);
begin
  Selection.Store(True, True);
  if ADecode then
    Selection.Text := XMLDecode(Selection.Text)
  else
    Selection.Text := XMLEncode(Selection.Text);
  Selection.Restore;
  View.Modified := True;
end;

procedure TEditorCommands.ConvertTabsToSpacesInSelection;
begin
  Selection.Store(True, True);
  //Selection.Text := TabsToSpaces(Selection.Text, View.Editor.TabWidth); // -> use settings?
  Selection.Restore;
  View.Modified := True;
end;

procedure TEditorCommands.SyncEditSelection;
begin
  //View.Editor.CommandProcessor(ecSynPSyncroEdStart, '', nil);
end;

procedure TEditorCommands.Save;
begin
  if View.IsFile then
    SaveFile(View.FileName)
  else
  begin
    View.Save;
  end;
end;

function TEditorCommands.SaveFile(const AFileName: string; AShowDialog: Boolean
  ): Boolean;
begin
  // { TODO -oTS : Migrate implementation to here. }.
  Result := Manager.SaveFile(AFileName, AShowDialog);
end;

procedure TEditorCommands.SaveAll;
var
  V : IEditorView;
begin
  for V in Manager.Views do
  begin
    if V.Modified then
    begin
      if V.IsFile then
      begin
        Manager.SaveFile(V.FileName);
      end
      else
        V.Save;
    end;
  end;
end;

procedure TEditorCommands.AdjustFontSize(AOffset: Integer);
begin
  Settings.EditorFont.Size := Settings.EditorFont.Size + AOffset;
  Settings.Apply; // needed to trigger settings changed event.
end;

{ TODO -oTS : Align in paragraphs does not work!
  TODO -oTS : Align to leftmost/rightmost token not implemented!
}

procedure TEditorCommands.AlignSelection(const AToken: string;
  ACompressWS: Boolean; AInsertSpaceBeforeToken: Boolean;
  AInsertSpaceAfterToken: Boolean; AAlignInParagraphs: Boolean);
begin
  if View.SelectionAvailable then
  begin
    Selection.Store(True, True);
    AlignLines(
      Selection.Lines,
      AToken,
      ACompressWS,
      AInsertSpaceBeforeToken,
      AInsertSpaceAfterToken
    );
    Selection.Restore;
  end;
end;

procedure TEditorCommands.MergeBlankLinesInSelection;
begin
  View.SelectedText := MergeBlankLines(View.SelectedText);
end;

procedure TEditorCommands.StripCommentsFromSelection;
begin
  //View.SelectedText := StripComments()
end;

{ TODO -oTS : Not working! }

procedure TEditorCommands.StripMarkupFromSelection;
begin
  View.SelectedText := StripMarkup(View.SelectedText);
end;

{ REMARK:
    Whitespace is ignored. This routine strips the first/last non-space char
    from each line in the selection.
}

procedure TEditorCommands.StripCharsFromSelection(AFirst: Boolean;
  ALast: Boolean);
begin
  View.SelectedText := StripChars(View.SelectedText, AFirst, ALast);
end;

procedure TEditorCommands.GuessHighlighterType;
var
  S : string;
begin
  S := GuessHighlighterType(View.Text);
  if S <> '' then
    AssignHighlighter(S);
end;

procedure TEditorCommands.Indent;
begin
  View.Editor.CommandProcessor(ecBlockIndent, #0, nil);
end;

procedure TEditorCommands.UnIndent;
begin
  View.Editor.CommandProcessor(ecBlockUnindent, #0, nil);
end;

{ Comments or uncomments selected code lines based on the line comment tag of
  the active highlighter. }

// TS TODO: use Selection, and keep selection after updating selected block

procedure TEditorCommands.UpdateCommentSelection(ACommentOn, AToggle: Boolean);
var
  OldCaretPos    : TPoint;
  WasSelAvail    : Boolean;
  BlockBeginLine : Integer;
  BlockEndLine   : Integer;
  CommonIndent   : Integer;
  Prefix         : string;
  PrefixLength   : Integer;

  function FirstNonBlankPos(const AText: string; AStart: Integer = 1): Integer;
  var
    I: Integer;
  begin
    for I := AStart to Length(AText) do
      if (AText[I] <> #32) and (AText[I] <> #9) then
        Exit(I);
    Result := -1;
  end;

  function MinCommonIndent: Integer;
  var
    I, J: Integer;
  begin
    if CommonIndent = 0 then
    begin
      CommonIndent := Max(FirstNonBlankPos(View.Lines[BlockBeginLine - 1]), 1);
      for I := BlockBeginLine + 1 to BlockEndLine do
      begin
        J := FirstNonBlankPos(View.Lines[I - 1]);
        if (J < CommonIndent) and (J > 0) then
          CommonIndent := J;
      end;
    end;
    Result := CommonIndent;
  end;

  function InsertPos(ALine: Integer): Integer;
  begin
    if not WasSelAvail then
      Result := MinCommonIndent
    else
      Result := 1;
//      case Selection.SelectionMode of
//        smColumn: // CommonIndent is not used otherwise
//        begin
//          if CommonIndent = 0 then
//            CommonIndent := Min(View.Editor.LogicalToPhysicalPos(Selection.BlockBegin).X,
//              View.Editor.LogicalToPhysicalPos(Selection.BlockEnd).X);
//          Result := View.Editor.PhysicalToLogicalPos(Point(CommonIndent, ALine)).X;
//        end;
//        smNormal:
//        begin
//          if Selection.BlockBegin.Y = Selection.BlockEnd.Y then
//            Result := Selection.BlockBegin.X
//          else
//            Result := MinCommonIndent;
//        end;
//        else
//          Result := 1;
//      end;
  end;

  function DeletePos(ALine: Integer): Integer;
  var
    S: string;
    T: string;
    N: Integer;
  begin
    S := View.Lines[ALine - 1];
    N := Length(S);
    Result := FirstNonBlankPos(S, InsertPos(ALine));
//    if (Selection.SelectionMode = smColumn) and ((Result < 1) or (Result > N - 1)) then
//      Result := N - 1;
    Result := Max(1, Result);
    T := System.Copy(S, Result, PrefixLength);
    if (N < Result + 1) or (T <> Prefix) then
      Result := -1;
  end;

var
  I             : Integer;
  NonBlankStart : Integer;
  BB            : TPoint;
  BE            : TPoint;
begin
  if Settings.ReadOnly then
    Exit;

//  Prefix := View.HighlighterItem.LineCommentTag;
  PrefixLength := Length(Prefix);

  if PrefixLength = 0 then
    ToggleBlockCommentSelection
  else
  begin
    OldCaretPos := View.CaretXY;
    Selection.Store;
    WasSelAvail := View.SelectionAvailable;
    CommonIndent := 0;

    BlockBeginLine := Selection.BlockBegin.Y;
    BlockEndLine   := Selection.BlockEnd.Y;
//    if (Selection.BlockEnd.X = 1) and (BlockEndLine > BlockBeginLine)
//      and (View.SelectionMode <> smLine) then
//      Dec(BlockEndLine);

    if AToggle then
    begin
      ACommentOn := False;
      for I := BlockBeginLine to BlockEndLine do
      begin
        if DeletePos(I) < 0 then
        begin
          ACommentOn := True;
          Break;
        end;
      end;
    end;

    View.BeginUpdate;
    //View.SelectionMode := smNormal;

    BB := Selection.BlockBegin;
    BE := Selection.BlockEnd;
    if ACommentOn then
    begin
//      for I := BlockEndLine downto BlockBeginLine do
//        View.Editor.TextBetweenPoints[Point(InsertPos(I), I), Point(InsertPos(I), I)] := Prefix;
      if OldCaretPos.X > InsertPos(OldCaretPos.Y) then
        OldCaretPos.X := OldCaretPos.X + PrefixLength;
      if BB.X > InsertPos(BB.Y) then
       BB.X := BB.X + PrefixLength;
      if BE.X > InsertPos(BE.Y) then
        BE.X := BE.X + PrefixLength;
    end
    else
    begin
      for I := BlockEndLine downto BlockBeginLine do
      begin
        NonBlankStart := DeletePos(I);
        if NonBlankStart < 1 then
          continue;
//        View.Editor.TextBetweenPoints[Point(NonBlankStart, I),
//          Point(NonBlankStart + PrefixLength, I)] := '';
        if (OldCaretPos.Y = I) and (OldCaretPos.X > NonBlankStart) then
          OldCaretPos.x := Max(OldCaretPos.X - PrefixLength, NonBlankStart);
        if (BB.Y = I) and (BB.X > NonBlankStart) then
          BB.X := Max(BB.X - PrefixLength, NonBlankStart);
        if (BE.Y = I) and (BE.X > NonBlankStart) then
          BE.X := Max(BE.X - PrefixLength, NonBlankStart);
      end;
    end;
    Selection.BlockBegin := BB;
    Selection.BlockEnd   := BE;
    View.EndUpdate;
    Selection.Text := View.SelectedText;
    View.CaretXY       := OldCaretPos;
    Selection.Ignore;
    Selection.Clear;
  end;
end;

{ Comments/uncomments the selected block with the block comment tags for the
  current highlighter. }

procedure TEditorCommands.ToggleBlockCommentSelection;
//var
//  S  : string;
//  S1 : string;
//  S2 : string;
//  N1 : Integer;
//  N2 : Integer;
//  HI : THighlighterItem;
begin
//  HI := View.HighlighterItem;
//  if Assigned(HI) and View.SelAvail and (HI.BlockCommentStartTag <> '') then
//  begin
//    Selection.Store(True, True);
//    N1 := Length(HI.BlockCommentStartTag);
//    N2 := Length(HI.BlockCommentEndTag);
//    S := Selection.Text;
//    S1 := System.Copy(S, 1, N1);
//    S2 := System.Copy(S, Length(S) - N2 + 1, Length(S));
//    if (S1 = HI.BlockCommentStartTag)
//      and (S2 = HI.BlockCommentEndTag) then
//    begin
//      Selection.Text := System.Copy(S, N1 + 1, Length(S) - N2 - N1);
//    end
//    else
//    begin
//      Selection.Text := HI.BlockCommentStartTag
//        + Selection.Text + HI.BlockCommentEndTag;
//    end;
//    Selection.Restore;
//    View.Modified := True;
//  end;
end;

procedure TEditorCommands.InsertTextAtCaret(const AText: string);
begin
  View.Editor.InsertBlock(
    View.Editor.SelectionBeginPosition,
    View.Editor.SelectionEndPosition,
    PWideChar(AText),
    True
  );
end;

procedure TEditorCommands.FormatCode;
//var
//  HI : THighlighterItem;
begin
//  HI := View.HighlighterItem;
//  if Assigned(HI) then
//    if Assigned(HI.CodeFormatter) then
//    begin
//      if not View.SelAvail then
//      begin
//        View.SelectAll;
//      end;
//      Selection.Store;
//      if Length(Trim(Selection.Text))>0 then
//         Selection.Text := HI.CodeFormatter.Format(Selection.Text);
//      Selection.Restore;
//    end
//    else
//      raise Exception.Create('No codeformatter for current highlighter');
end;

procedure TEditorCommands.SortStrings;
//var
//  SSS: TSortStringsSettings;
begin
//  SSS := Settings.ToolSettings
//    .ItemsByClass[TSortStringsSettings] as TSortStringsSettings;
//  Selection.Store;
//  Selection.Text := ts.Editor.Utils.SortStrings(
//    Selection.Text,
//    SSS.SortDirection,
//    SSS.SortScope,
//    SSS.CaseSensitive,
//    SSS.IgnoreSpaces
//  );
//  Selection.Restore;
end;

{ Makes a smart selection of a block around the cursor. }

{ TODO -oTS : Make this configurable per highlighter (see unit
  ts.Editor.CodeTags).  }

procedure TEditorCommands.SmartSelect;
//var
//  HI : THighlighterItem;
begin
//  HI := View.HighlighterItem;
//  if Assigned(HI) then
//  begin
//    if HI.Name = 'XML' then
//      SelectBlockAroundCursor('>', '<', False, False)
//    else if HI.Name = 'PAS' then
//      SelectBlockAroundCursor('begin', 'end', True, True)
//    else if HI.Name = 'LOG' then
//      SelectBlockAroundCursor('<XMLRoot>', '</XMLRoot>', True, True);
//  end;
end;

{ Selects block of code around cursor between AStartTag and AEndTag. Used by
  the SmartSelect procedure.

  TODO:
    - support for nested AStartTag and AEndTag (ignore sublevels)
}

function TEditorCommands.SelectBlockAroundCursor(const AStartTag: string;
  const AEndTag: string; AIncludeStartTag: Boolean;
  AIncludeEndTag: Boolean): Boolean;
var
  Pos : Integer;
  S   : string;
  B   : Boolean;
  I   : Integer;
  N   : Integer;
begin
  if (AStartTag = '') or (AEndTag = '') then
    Exit;

  S := View.Text;
  Pos := View.SelStart;
  B := False;
  while not B and (Pos > 1) do
  begin
    N := Length(AStartTag);
    I := N;
    B := S[Pos] = AStartTag[I];
    while B and (Pos > 1) and (I > 1) do
    begin
      Dec(I);
      Dec(Pos);
      B := S[Pos] = AStartTag[I];
    end;
    if not B and (Pos > 1) then
      Dec(Pos);
  end;
  if B then
  begin
    if AIncludeStartTag then
      View.SelStart := Pos
    else
      View.SelStart := Pos + N;
  end;

  if B then
  begin
    Pos := View.SelStart;
    B := False;
    while not B and (Pos <= Length(S)) do
    begin
      N := Length(AEndTag);
      I := 1;
      B := S[Pos] = AEndTag[I];
      while B and (Pos <= Length(S)) and (I < N) do
      begin
        Inc(I);
        Inc(Pos);
        B := S[Pos] = AEndTag[I];
      end;
      if not B and (Pos <= Length(S)) then
        Inc(Pos);
    end;
    if B then
    begin
      if AIncludeEndTag then
        View.SelEnd := Pos + 1
      else
        View.SelEnd := Pos - N + 1;
    end;
  end;
  Result := View.SelectionAvailable;
end;

procedure TEditorCommands.FindNext;
begin
  SearchEngine.FindNext;
end;

procedure TEditorCommands.FindPrevious;
begin
  SearchEngine.FindPrevious;
end;
{$ENDREGION}

end.

