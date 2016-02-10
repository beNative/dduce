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

unit DDuce.Editor.View;

{$REGION'documentation'}
{
Form holding a complete customizable text editor based on the open source
TBCEditor component.
Features:
  - accepts dropped files
  - auto detect file encoding
  - dynamic editor creation
  - synchronized edit
  - highlight selected text
  - code folding
  - file monitor function to watch for external file changes.

TODO:
  - remove bookmark images
  - macrorecorder
  - template editor
  - configurable page setup and printing with preview
  - quickbuttons (like the Delphi version had)
  - URI opener, to open hyperlinks directly from the editor
  - customizable keystroke-function mappings
  - configurable code completion proposal
  - convert to another encoding (partially implemented)
  - find a way to fold particular sections (now only levels are supported)
  - send to mail action

  DEPENDENCIES:
  - BCEditor-
}
{$ENDREGION}

interface
uses
  System.Classes, System.SysUtils, System.Types, System.ImageList,
  Vcl.Controls, Vcl.Forms, Vcl.Graphics, Vcl.Menus, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ImgList,

  BCEditor.Editor, BCEditor.Types, BCEditor.Editor.KeyCommands,

  BCEditor.Highlighter,

  DDuce.Editor.Resources, DDuce.Editor.Highlighters, DDuce.Editor.Interfaces,

  DDuce.Logger;

type
  TEditorView = class(TForm, IEditorView, IEditorSelection)
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);

  strict private
    procedure EditorChangeUpdating(
      ASender    : TObject;
      AUpdating  : Boolean
    );

    procedure EditorSpecialLineColors(
          Sender  : TObject;
          Line    : Integer;
      var Special : Boolean;
      var FG, BG  : TColor
    );
    procedure EditorChange(Sender: TObject);
    procedure EditorClickLink(
      Sender : TObject;
      Button : TMouseButton;
      Shift  : TShiftState;
      X, Y   : Integer
    );

    procedure EditorCaretChanged(
      Sender : TObject;
      X, Y   : Integer
    );

    procedure EditorCommandProcessed(
      Sender       : TObject;
      var ACommand : TBCEditorCommand;
      var AChar    : Char;
      AData        : Pointer
    );
    procedure EditorReplaceText(
      Sender         : TObject;
      const ASearch  : string;
      const AReplace : string;
      ALine          : Integer;
      AColumn        : Integer;
      ADeleteLine    : Boolean;
      var AAction    : TBCEditorReplaceAction
    );

    procedure EditorDropFiles(
      Sender : TObject;
      Pos    : TPoint;
      AFiles : TStrings
    );

    function IsActive: Boolean;
    procedure ApplySettings;

  private
    FUpdate          : Boolean;
    FLineBreakStyle  : string;
    FEditor          : TBCEditor;
    FFindHistory     : TStringList;
    FReplaceHistory  : TStringList;
    FHighlighterItem : THighlighterItem;
    FFileName        : string;
    FFoldLevel       : Integer;
    FIsFile          : Boolean;
    FFontChanged     : Boolean;
    FSelection       : IEditorSelection;
    FOnChange        : TNotifyEvent;

    { search settings }
    FSearchText     : string;
//    FSearchOptions  : TSynSearchOptions;

    {$REGION'property access methods'}
    function GetActions: IEditorActions;
    function GetBlockBegin: TPoint;
    function GetBlockEnd: TPoint;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretX: Integer; virtual;
    function GetCaretXY: TPoint;
    function GetCaretY: Integer; virtual;
    function GetCommands: IEditorCommands;
    function GetCurrentChar: WideChar;
    function GetCurrentWord: string;
    function GetEditor: TBCEditor;
    function GetEditorFont: TFont;
    function GetHighlighterName: string;
    function GetInsertMode: Boolean;
    function GetIsFile: Boolean;
    function GetManager: IEditorManager;
    function GetEncoding: TEncoding;
    function GetEvents: IEditorEvents;
    function GetFileName: string;
    function GetFindHistory: TStrings;
    function GetFoldLevel: Integer;
    function GetForm: TCustomForm;
    function GetHighlighterItem: THighlighterItem;
    function GetLineBreakStyle: string;
    function GetLines: TStrings; virtual;
    function GetLinesInWindow: Integer;
    function GetLineText: string;
    function GetLogicalCaretXY: TPoint;
    function GetModified: Boolean;
    function GetMonitorChanges: Boolean;
    function GetName: string;
    function GetOnChange: TNotifyEvent;
    function GetOnDropFiles: TBCEditorDropFilesEvent;
//    function GetOnStatusChange: TStatusChangeEvent;
    function GetParent: TWinControl;
    function GetPopupMenu: TPopupMenu; reintroduce;
    function GetPreviewText: string;
    function GetReplaceHistory: TStrings;
//    function GetSearchOptions: TSynSearchOptions;
    function GetSearchText: string;
    function GetSelectionAvailable: Boolean;
    function GetSelection: IEditorSelection;
    function GetSelectionMode: TBCEditorSelectionMode;
    function GetSelEnd: Integer;
    function GetSelStart: Integer;
    function GetSelectedText: string;
    function GetSettings: IEditorSettings;
    function GetShowSpecialChars: Boolean;
    function GetText: string;
    function GetTextSize: Integer;
    function GetTopLine: Integer;
    procedure SetBlockBegin(const AValue: TPoint);
    procedure SetBlockEnd(const AValue: TPoint);
    procedure SetCaretX(const Value: Integer); virtual;
    procedure SetCaretXY(const AValue: TPoint);
    procedure SetCaretY(const Value: Integer); virtual;
    procedure SetEditorFont(AValue: TFont);
    procedure SetFileName(const AValue: string);
    procedure SetFoldLevel(const AValue: Integer);
    procedure SetHighlighterItem(const AValue: THighlighterItem);
    procedure SetHighlighterName(AValue: string);
    procedure SetInsertMode(AValue: Boolean);
    procedure SetIsFile(AValue: Boolean);
    procedure SetLineBreakStyle(const AValue: string);
    procedure SetLineText(const AValue: string);
    procedure SetLogicalCaretXY(const AValue: TPoint);
    procedure SetModified(const AValue: Boolean);
    procedure SetMonitorChanges(const AValue: Boolean);
    procedure SetName(AValue: string); reintroduce;
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TBCEditorDropFilesEvent);
//    procedure SetOnStatusChange(const AValue: TStatusChangeEvent);

    procedure SetPopupMenu(AValue: TPopupMenu);
//    procedure SetSearchOptions(AValue: TSynSearchOptions);
    procedure SetSearchText(const Value: string); virtual;
    procedure SetSelectionMode(AValue: TBCEditorSelectionMode);
    procedure SetSelEnd(const AValue: Integer);
    procedure SetSelStart(const AValue: Integer);
    procedure SetSelectedText(const AValue: string);
    procedure SetShowSpecialChars(const AValue: Boolean);
    procedure SetText(const AValue: string);
    procedure SetTopLine(const AValue: Integer);
    {$ENDREGION}

    procedure InitializeEditor(AEditor: TBCEditor);
    procedure EditorSettingsChanged(ASender: TObject);
    function GetHighlighter: TBCEditorHighlighter;
    procedure SetHighlighter(const Value: TBCEditorHighlighter);


  strict protected
  procedure SetParent(Value: TWinControl);

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure BeginUndoBlock;
    procedure EndUndoBlock;

    procedure CopyToClipboard;
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Undo;
    procedure Redo;

    function EditorViewFocused: Boolean;
    function IEditorView.Focused = EditorViewFocused;

    procedure AssignHighlighterForFileType(const AFileExt: string);

    procedure ClearHighlightSearch;
//    procedure SetHighlightSearch(
//      const ASearch  : string;
//            AOptions : TSynSearchOptions
//    );
    procedure SearchAndSelectLine(
            ALineIndex : Integer;
      const ALine      : string
    );
    procedure SearchAndSelectText(const AText: string);
    procedure SelectWord;
    procedure FindNextWordOccurrence(ADirectionForward: Boolean);

    procedure Clear;
    procedure SelectAll;

    procedure DoChange; dynamic;

  protected
    // TCustomForm overrides
    procedure Activate; override;
    procedure UpdateActions; override;
    procedure DoClose(var CloseAction: TCloseAction); override;

  public
    // constructors and destructors
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    // public overridden methods
    function CloseQuery: Boolean; override;

    // public methods
    function GetWordAtPosition(const APosition: TPoint): string;
    function GetWordFromCaret(const ACaretPos: TPoint): string;
//    function GetHighlighterAttriAtRowCol(
//          APosition : TPoint;
//      out AToken    : string;
//      out AAttri    : TSynHighlighterAttributes
//    ): Boolean;
    procedure Load(const AStorageName: string = '');
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure Save(const AStorageName: string = '');

    // public properties
    { Column and line of the start of the selected block. }
    property BlockBegin: TPoint
      read GetBlockBegin write SetBlockBegin;

    { Column and line of the end of the selected block. }
    property BlockEnd: TPoint
      read GetBlockEnd write SetBlockEnd;

    { Current position of the caret on the screen. Expanded TABs make this
      position different from LogicalCaretXY. }
    property CaretXY: TPoint
      read GetCaretXY write SetCaretXY;

    { Current position of the caret in the data buffer. }
    property LogicalCaretXY: TPoint
      read GetLogicalCaretXY write SetLogicalCaretXY;

  published // for the moment published only for easy debugging
    { current X-coordinate of the caret on the screen. }
    property CaretX: Integer
      read GetCaretX write SetCaretX;

    { current Y-coordinate of the caret on the screen. }
    property CaretY: Integer
      read GetCaretY write SetCaretY;

    property CurrentChar: WideChar
      read GetCurrentChar;

    property CurrentWord: string
      read GetCurrentWord;

    property TextSize: Integer
      read GetTextSize;

    property CanPaste: Boolean
      read GetCanPaste;

    property CanRedo: Boolean
      read GetCanRedo;

    property CanUndo: Boolean
      read GetCanUndo;

    property FoldLevel: Integer
      read GetFoldLevel write SetFoldLevel;

    property SelStart: Integer
      read GetSelStart write SetSelStart;

    property SelEnd: Integer
      read GetSelEnd write SetSelEnd;

    property InsertMode: Boolean
      read GetInsertMode write SetInsertMode;

    property SelectionMode: TBCEditorSelectionMode
      read GetSelectionMode write SetSelectionMode;

    property ShowSpecialChars: Boolean
      read GetShowSpecialChars write SetShowSpecialChars;

    property Modified: Boolean
      read GetModified write SetModified;

    { Component name }
    property Name: string
      read GetName write SetName;

    property IsFile: Boolean
      read GetIsFile write SetIsFile;

    { Amount of visible lines (not including folds). }
    property LinesInWindow: Integer
      read GetLinesInWindow;

    property Editor: TBCEditor
      read GetEditor;

    property Form: TCustomForm
      read GetForm;

    property LineText: string
      read GetLineText write SetLineText;

    property SelectionAvailable: Boolean
      read GetSelectionAvailable;

    property Manager: IEditorManager
      read GetManager;

    { Reference to the IEditorActions singleton that manages one or more
      IEditView instances. }
    property Actions: IEditorActions
      read GetActions;

    property Commands: IEditorCommands
      read GetCommands;

    { A set of common events to dispatch to the application. }
    property Events: IEditorEvents
      read GetEvents;

    { Global settings shared by all EditorView instances }
    property Settings: IEditorSettings
      read GetSettings;

    { TODO: assign this to active search engine }
    property FindHistory: TStrings
      read GetFindHistory;

    { TODO: assign this to active search engine }
    property ReplaceHistory: TStrings
      read GetReplaceHistory;

    property Highlighter: TBCEditorHighlighter
      read GetHighlighter write SetHighlighter;

    property HighlighterName: string
      read GetHighlighterName write SetHighlighterName;

    property HighlighterItem: THighlighterItem
      read GetHighlighterItem write SetHighlighterItem;

    property Selection: IEditorSelection
      read GetSelection implements IEditorSelection;

    { Shortcut to the text contained in the editor. }
    property Lines: TStrings
      read GetLines;

    property MonitorChanges: Boolean
      read GetMonitorChanges write SetMonitorChanges;

    property Text: string
      read GetText write SetText;

    property TopLine: Integer
      read GetTopLine write SetTopLine;

    property SearchText: string
      read GetSearchText write SetSearchText;

    { These options are used for highlighting the active word. }
//    property SearchOptions: TSynSearchOptions
//      read GetSearchOptions write SetSearchOptions;

    { Currently selected text in the editor. }
    property SelectedText: string
      read GetSelectedText write SetSelectedText;

    property FileName: string
      read GetFileName write SetFileName;

    property Parent: TWinControl
      read GetParent write SetParent;

    property Encoding: TEncoding
      read GetEncoding;

    property EditorFont: TFont
      read GetEditorFont write SetEditorFont;

    property LineBreakStyle: string
      read GetLineBreakStyle write SetLineBreakStyle;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

//    property OnStatusChange: TStatusChangeEvent
//      read GetOnStatusChange write SetOnStatusChange;

    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;
  end;

implementation

{$R *.dfm}

uses
  System.TypInfo,
  Vcl.GraphUtil,

  DDuce.Editor.Utils;

{$REGION'construction and destruction'}
procedure TEditorView.AfterConstruction;
begin
  inherited AfterConstruction;
  FEditor             := TBCEditor.Create(Self);

//  FSynSelection       := TSynEditSelection.Create(E.ViewedTextBuffer, True);
//  FSynSelection.Caret := E.Caret;

//  FSelection := TEditorSelection.Create(Self);

  FFindHistory            := TStringList.Create;
  FFindHistory.Sorted     := True;
  FFindHistory.Duplicates := dupIgnore;

  FReplaceHistory            := TStringList.Create;
  FReplaceHistory.Sorted     := True;
  FReplaceHistory.Duplicates := dupIgnore;

  FIsFile         := True;
//  FEncoding       := EncodingUTF8;
//  FLineBreakStyle := ALineBreakStyles[Lines.TextLineBreakStyle];

  InitializeEditor(FEditor);
//  FDirectoryWatch          := TDirectoryWatch.Create;
//  FDirectoryWatch.OnNotify := DirectoryWatchNotify;
  // TEST
  MonitorChanges := True;
  //Settings.AddEditorSettingsChangedHandler(EditorSettingsChanged);
  ApplySettings;
end;

procedure TEditorView.BeforeDestruction;
begin
//  FHighlighter := nil;
//  FSelection   := nil;
//  if Assigned(Settings) then
//    Settings.RemoveEditorSettingsChangedHandler(EditorSettingsChanged);
//  DisableAutoSizing;
//  FreeAndNil(FSynSelection);
  FreeAndNil(FReplaceHistory);
  FreeAndNil(FFindHistory);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION'event handlers'}
procedure TEditorView.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
begin
//  if Assigned(OnDropFiles) then
//    OnDropFiles(Self, FileNames);
  Events.DoChange;
end;

procedure TEditorView.EditorChangeUpdating(ASender: TObject;
  AUpdating: Boolean);
begin
//  Logger.Send(
//    'EditorChangeUpdating(AUpdating = %s)',
//    [BoolToStr(AUpdating, 'True', 'False')]
//  );
//  if FFontChanged then
//  begin
//    Settings.EditorFont.Assign(EditorFont);
//    Settings.Apply;
//    FFontChanged := False;
//  end;
end;

{ Makes actionlist shortcuts work on the form }

//procedure TEditorView.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
//begin
//  Handled := Actions.ActionList.IsShortCut(Msg);
//end;

{$IFDEF Windows}
{ Event triggered when MonitorChanges is True }

procedure TEditorView.DirectoryWatchNotify(const Sender: TObject;
  const AAction: TWatchAction; const FileName: string);
begin
  if SameText(FileName, ExtractFileName(Self.FileName)) and (AAction = waModified) then
  begin
    Load(Self.FileName);
    if CanFocus then
    begin
      Editor.CaretY := Editor.Lines.Count;
      Editor.EnsureCursorPosVisible;
    end;
  end;
end;
{$ENDIF}

procedure TEditorView.EditorClickLink(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Logger.Send('EditorClickLink');
  Commands.OpenFileAtCursor;
end;

procedure TEditorView.EditorCommandProcessed(Sender: TObject;
  var ACommand: TBCEditorCommand; var AChar: Char; AData: Pointer);
begin
  Logger.Enter('TEditorView.EditorCommandProcessed');

  Logger.Leave('TEditorView.EditorCommandProcessed');
end;

procedure TEditorView.EditorDropFiles(Sender: TObject; Pos: TPoint;
  AFiles: TStrings);
begin
  Editor.LoadFromFile(AFiles[0]);
  Editor.Highlighter.LoadFromFile('Object Pascal.json');
end;

procedure TEditorView.EditorReplaceText(Sender: TObject; const ASearch,
  AReplace: string; ALine, AColumn: Integer; ADeleteLine: Boolean;
  var AAction: TBCEditorReplaceAction);
begin
  Logger.Enter('TEditorView.EditorReplaceText');
  Logger.Leave('TEditorView.EditorReplaceText');
end;

procedure TEditorView.EditorCaretChanged(Sender: TObject; X, Y: Integer);
begin
  Logger.Enter('TEditorView.EditorCaretChanged');
  Logger.Leave('TEditorView.EditorCaretChanged');
end;

procedure TEditorView.EditorChange(Sender: TObject);
begin
  Logger.Enter('TEditorView.EditorChange');
  Logger.Send('EditorChange');
  DoChange;
  Events.DoChange;
  Logger.Leave('TEditorView.EditorChange');
end;

//procedure TEditorView.EditorStatusChange(Sender: TObject;
//  Changes: TSynStatusChanges);
//begin
//  if not (csDestroying in ComponentState) then
//  begin
//    Logger.Send(
//      'EditorStatusChange(Changes = %s)',
//      [SetToString(TypeInfo(TSynStatusChanges), Changes)]
//    );
//
//    // we use this event to ensure that the view is activated because the OnEnter
//    // event is not triggered when the form is undocked!
//
//    // but side-effects when it is docked !
//    //if not IsActive then
//		  //Activate;
//
//    if Assigned(FOnStatusChange) then
//      FOnStatusChange(Self, Changes);
//    Events.DoStatusChange(Changes);
//    if (scCaretX in Changes) or (scCaretY in Changes) then
//    begin
//      Events.DoCaretPositionChange;
//    end;
//    if scModified in Changes then
//    begin
//      Events.DoModified;
//    end;
//  end;
//  //Logger.Send('FirstLineBytePos', SynSelection.FirstLineBytePos);
//  //Logger.Send('EndLineBytePos', SynSelection.EndLineBytePos);
//  //Logger.Send('StartLinePos', SynSelection.StartLinePos);
//  //Logger.Send('EndLinePos', SynSelection.EndLinePos);
//  //Logger.Send('StartBytePos', SynSelection.StartBytePos);
//  //Logger.Send('EndBytePos', SynSelection.EndBytePos);
//  //Logger.Send('LastLineBytePos', SynSelection.LastLineBytePos);
//
//end;

//procedure TEditorView.EditorProcessCommand(Sender: TObject;
//  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer);
//begin
//  // TODO: dispatch events to manager (cfr. bookmarks)
//  Logger.Send(
//    'EditorProcessCommand(Command = %s; AChar = %s; Data)',
//    [EditorCommandToCodeString(Command), AChar]
//  );
//end;
{$ENDREGION}

{$REGION'event dispatch methods'}
procedure TEditorView.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TEditorView.DoClose(var CloseAction: TCloseAction);
begin
  CloseAction :=  caFree;
  inherited DoClose(CloseAction);
end;
{$ENDREGION}

{$REGION'property access methods'}
function TEditorView.GetSelectedText: string;
begin
  Result := Editor.SelectedText;
end;

procedure TEditorView.SetSelectedText(const AValue: string);
begin
  Editor.BeginUndoBlock;
  Editor.SelectedText := AValue;
  Editor.EndUndoBlock;
end;

function TEditorView.GetFoldLevel: Integer;
begin
  Result := FFoldLevel;
end;

procedure TEditorView.SetFoldLevel(const AValue: Integer);
begin
  if AValue <> FoldLevel then
  begin
    FFoldLevel := AValue;
    //Editor.FoldAll(FFoldLevel, True);
    Events.DoModified;
  end;
end;

function TEditorView.GetText: string;
begin
  Result := Lines.Text;
end;

procedure TEditorView.SetText(const AValue: string);
begin
  if Lines.Text <> AValue then
  begin
    Lines.Text := AValue;
  end;
end;

function TEditorView.GetCaretX: Integer;
begin
  Result := Editor.TextCaretPosition.Char;
end;

procedure TEditorView.SetCaretX(const Value: Integer);
var
  P : TBCEditorTextPosition;
begin
  if Value <> CaretX then
  begin
    P.Char := Value;
    P.Line := CaretY;
    Editor.SetCaretAndSelection(P, P, P);
  end;
end;

function TEditorView.GetCaretY: Integer;
begin
  Result := Editor.TextCaretPosition.Line;
end;

procedure TEditorView.SetCaretY(const Value: Integer);
var
  P : TBCEditorTextPosition;
begin
  if Value <> CaretY then
  begin
    P.Char := CaretX;
    P.Line := Value;
    Editor.SetCaretAndSelection(P, P, P);
  end;
end;

function TEditorView.GetEditorFont: TFont;
begin
  Result := Editor.Font;
end;

function TEditorView.GetHighlighter: TBCEditorHighlighter;
begin
//
end;

function TEditorView.GetHighlighterName: string;
begin
  Result := Editor.Highlighter.Name;
end;

procedure TEditorView.SetHighlighter(const Value: TBCEditorHighlighter);
begin
//
end;

procedure TEditorView.SetHighlighterName(AValue: string);
begin
  if AValue <> HighlighterName then
  begin
    // HighlighterItem is always assigned
    //HighlighterItem := Manager.Highlighters.ItemsByName[AValue];
  end;
  //if no Highlighters defined, we use the number0 Highlighters
//  if trim(AValue)='' then
//     if Manager.Highlighters.Count>0 then
//         HighlighterItem := Manager.Highlighters.Items[0];
end;

function TEditorView.GetInsertMode: Boolean;
begin
  Result := Editor.InsertMode;
end;

procedure TEditorView.SetInsertMode(AValue: Boolean);
begin
  Editor.InsertMode := AValue;
end;

function TEditorView.GetIsFile: Boolean;
begin
  Result := FIsFile;
end;

procedure TEditorView.SetIsFile(AValue: Boolean);
begin
  FIsFile := AValue;
end;

procedure TEditorView.SetEditorFont(AValue: TFont);
begin
  if not Editor.Font.Equals(AValue) then
  begin
    Editor.Font.Assign(AValue);
  end;
end;

function TEditorView.GetLines: TStrings;
begin
  Result := Editor.Lines;
end;

function TEditorView.GetSearchText: string;
begin
  Result := Editor.SearchString;
end;

procedure TEditorView.SetSearchText(const Value: string);
begin
  if Value <> SearchText then
  begin
    Editor.SearchString := Value;
  end;
end;

function TEditorView.GetTopLine: Integer;
begin
  Result := Editor.TopLine;
end;

procedure TEditorView.SetTopLine(const AValue: Integer);
begin
  Editor.TopLine := AValue;
end;

function TEditorView.GetFileName: string;
begin
  Result := FEditor.FileName;
end;

procedure TEditorView.SetFileName(const AValue: string);
begin
  if AValue <> FileName then
  begin
    FFileName := AValue;
    //FEditor.FileName := AValue;
    //Caption := ExtractFileName(AValue);
  end;
end;

function TEditorView.GetEncoding: TEncoding;
begin
  Result := Editor.Encoding;
end;

function TEditorView.GetLineBreakStyle: string;
begin
  Result := FLineBreakStyle;
end;

procedure TEditorView.SetLineBreakStyle(const AValue: string);
begin
  if AValue <> LineBreakStyle then
  begin
    FLineBreakStyle := AValue;
    Modified        := True;
  end;
end;

function TEditorView.GetLineText: string;
begin
  //Result := Editor.Get
//  Result := Editor. LineText;
end;

procedure TEditorView.SetLineText(const AValue: string);
begin
//  Editor.LineText := AValue;
end;

function TEditorView.GetMonitorChanges: Boolean;
begin
//  Result := FDirectoryWatch.Running;

  Result := False;

end;

procedure TEditorView.SetMonitorChanges(const AValue: Boolean);
begin
  if AValue <> MonitorChanges then
  begin
//    if AValue then
//    begin
//      if FileExists(FileName) then
//      begin
//        FDirectoryWatch.Directory := ExtractFileDir(FileName);
//        FDirectoryWatch.Start;
//      end;
//    end
//    else
//      FDirectoryWatch.Stop;
  end;
end;

function TEditorView.GetName: string;
begin
  Result := inherited Name;
end;

procedure TEditorView.SetName(AValue: string);
begin
  inherited Name := AValue;
end;

function TEditorView.GetOnChange: TNotifyEvent;
begin
  Result := FOnChange;
end;

procedure TEditorView.SetOnChange(const AValue: TNotifyEvent);
begin
  FOnChange := AValue;
end;

function TEditorView.GetOnDropFiles: TBCEditorDropFilesEvent;
begin
  Result := Editor.OnDropFiles;
end;

procedure TEditorView.SetOnDropFiles(const AValue: TBCEditorDropFilesEvent);
begin
  Editor.OnDropFiles := AValue;
end;

function TEditorView.GetParent: TWinControl;
begin
  Result := inherited Parent;
end;

procedure TEditorView.SetParent(Value: TWinControl);
begin
  inherited Parent := Value;
  if Assigned(Parent) then
    Visible := True;
end;

function TEditorView.GetPopupMenu: TPopupMenu;
begin
  Result := PopupMenu;
end;

procedure TEditorView.SetPopupMenu(AValue: TPopupMenu);
begin
  inherited PopupMenu := AValue;
end;

function TEditorView.GetLogicalCaretXY: TPoint;
begin
//  Result := Editor.LogicalCaretXY;
end;

procedure TEditorView.SetLogicalCaretXY(const AValue: TPoint);
begin
//  Editor.LogicalCaretXY := AValue;
end;

function TEditorView.GetModified: Boolean;
begin
  Result := Editor.Modified;
end;

procedure TEditorView.SetModified(const AValue: Boolean);
begin
  if AValue <> Modified then
  begin
    Editor.Modified := AValue;
  end;
end;

//function TEditorView.GetSearchOptions: TSynSearchOptions;
//begin
//  Result := FSearchOptions;
//end;
//
//procedure TEditorView.SetSearchOptions(AValue: TSynSearchOptions);
//begin
//  FSearchOptions := AValue;
//end;

function TEditorView.GetSelEnd: Integer;
begin
  //Result := Editor.SelectionEndPosition SelEnd;
end;

procedure TEditorView.SetSelEnd(const AValue: Integer);
begin
  //Editor.SelEnd := AValue;
end;

function TEditorView.GetSelStart: Integer;
begin
  //Result := Editor.SelStart;
end;

procedure TEditorView.SetSelStart(const AValue: Integer);
begin
  //Editor.SelStart := AValue;
end;

function TEditorView.GetSettings: IEditorSettings;
begin
  Result := Owner as IEditorSettings;
end;

function TEditorView.GetActions: IEditorActions;
begin
  Result := Owner as IEditorActions;
end;

function TEditorView.GetCaretXY: TPoint;
begin
  Result.X := Editor.TextCaretPosition.Char;
  Result.Y := Editor.TextCaretPosition.Line;
end;

procedure TEditorView.SetCaretXY(const AValue: TPoint);
begin
  //Editor.CaretXY := AValue;
end;

function TEditorView.GetFindHistory: TStrings;
begin
  Result := FFindHistory;
end;

function TEditorView.GetHighlighterItem: THighlighterItem;
begin
  Result := FHighlighterItem;
end;

procedure TEditorView.SetHighlighterItem(const AValue: THighlighterItem);
begin
  if HighlighterItem <> AValue then
  begin
    FHighlighterItem := AValue;
    if Assigned(AValue) then
    begin
      //AValue.Reload;
      Settings.HighlighterType := FHighlighterItem.Highlighter;
      //Highlighter := AValue.SynHighlighter;
      Actions.UpdateHighLighterActions;
    end;
    Events.DoHighlighterChange;
  end;
end;

function TEditorView.GetShowSpecialChars: Boolean;
begin
  Result := Editor.SpecialChars.Visible;
end;

procedure TEditorView.SetShowSpecialChars(const AValue: Boolean);
begin
  Editor.SpecialChars.Visible := AValue;
end;

function TEditorView.GetBlockBegin: TPoint;
begin
  Result.X := Editor.SelectionBeginPosition.Char;
  Result.Y := Editor.SelectionBeginPosition.Line;
end;

procedure TEditorView.SetBlockBegin(const AValue: TPoint);
var
  P : TBCEditorTextPosition;
begin
  P.Char := AValue.X;
  P.Line := AValue.Y;
  Editor.SelectionBeginPosition := P;
end;

function TEditorView.GetBlockEnd: TPoint;
begin
  Result.X := Editor.SelectionEndPosition.Char;
  Result.Y := Editor.SelectionEndPosition.Line;
end;

procedure TEditorView.SetBlockEnd(const AValue: TPoint);
var
  P : TBCEditorTextPosition;
begin
  P.Char := AValue.X;
  P.Line := AValue.Y;
  Editor.SelectionEndPosition:= P;
end;

function TEditorView.GetTextSize: Integer;
begin
  //Result := Length(Text);
  Result := Editor.GetTextLen;
end;

function TEditorView.GetEvents: IEditorEvents;
begin
  Result := Owner as IEditorEvents;
end;

function TEditorView.GetLinesInWindow: Integer;
begin
  Result := Editor.VisibleLines;
end;

function TEditorView.GetReplaceHistory: TStrings;
begin
  Result := FReplaceHistory;
end;

function TEditorView.GetPreviewText: string;
begin
//  if SelAvail then
//    Result := SelText
//  else
//    Result := LineText;
end;

function TEditorView.GetSelectionAvailable: Boolean;
begin
  Result := Editor.SelectionAvailable;
end;

function TEditorView.GetSelection: IEditorSelection;
begin
  Result := FSelection;
end;

function TEditorView.GetSelectionMode: TBCEditorSelectionMode;
begin
  Result := Editor.Selection.Mode;
end;

procedure TEditorView.SetSelectionMode(AValue: TBCEditorSelectionMode);
begin
  Editor.Selection.Mode := AValue;
end;

function TEditorView.GetCanPaste: Boolean;
begin
  Result := Editor.CanPaste;
end;

function TEditorView.GetCanRedo: Boolean;
begin
  Result := Editor.CanRedo;
end;

function TEditorView.GetCanUndo: Boolean;
begin
  Result := Editor.CanUndo;
end;

function TEditorView.GetForm: TCustomForm;
begin
  Result := Self;
end;

function TEditorView.GetCurrentWord: string;
begin
  Result := Editor.WordAtCursor;
end;

function TEditorView.GetCommands: IEditorCommands;
begin
  Result := Owner as IEditorCommands;
end;

function TEditorView.GetCurrentChar: WideChar;
begin
  //Result := Editor.TextToDisplayPosition()

//  if SelStart < Length(Text) then
//    Result := Text[SelStart]
//  else
//    Result := #0;
end;

function TEditorView.GetEditor: TBCEditor;
begin
  Result := FEditor;
end;

function TEditorView.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;
{$ENDREGION}

{$REGION'private methods'}
procedure TEditorView.AssignHighlighterForFileType(const AFileExt: string);
begin
//  HighlighterItem := Manager.Highlighters.FindHighlighterForFileType(AFileExt);
//  if not Assigned(HighlighterItem) then
//  begin
//    if Settings.AutoGuessHighlighterType then
//    begin
//      Commands.GuessHighlighterType;
//    end;
//    if not Assigned(HighlighterItem) then
//      HighlighterName := HL_TXT;
//  end
end;

function TEditorView.IsActive: Boolean;
begin
  Result := Manager.ActiveView = (Self as IEditorView);
end;

procedure TEditorView.ApplySettings;
begin
  //Editor.LineSpacing.Rule.
  Editor.Tabs.WantTabs := Settings.EditorOptions.WantTabs;
  Editor.Tabs.Width    := Settings.EditorOptions.TabWidth;

  if Settings.EditorOptions.TabsToSpaces then
    Editor.Tabs.Options := Editor.Tabs.Options + [toTabsToSpaces]
  else
    Editor.Tabs.Options := Editor.Tabs.Options - [toTabsToSpaces];

  if Settings.EditorOptions.TabIndent then
    Editor.Tabs.Options := Editor.Tabs.Options + [toSelectedBlockIndent]
  else
    Editor.Tabs.Options := Editor.Tabs.Options - [toSelectedBlockIndent];

  if Settings.EditorOptions.AutoIndent then
    Editor.Options := Editor.Options + [eoAutoIndent]
  else
    Editor.Options := Editor.Options - [eoAutoIndent];

  if Settings.EditorOptions.DragDropEditing then
    Editor.Options := Editor.Options + [eoDragDropEditing]
  else
    Editor.Options := Editor.Options - [eoDragDropEditing];

  if Settings.EditorOptions.TrimTrailingSpaces then
    Editor.Options := Editor.Options + [eoTrimTrailingSpaces]
  else
    Editor.Options := Editor.Options - [eoTrimTrailingSpaces];

  Editor.RightMargin.Visible := Settings.EditorOptions.ShowRightEdge;



//  if Settings.EditorOptions.BlockTabIndent then




  //




//  EditorFont                   := Settings.EditorFont;
//  ShowSpecialChars             := Settings.EditorOptions.ShowSpecialCharacters;
//  Editor.ExtraLineSpacing      := Settings.EditorOptions.ExtraLineSpacing;
//  Editor.ExtraCharSpacing      := Settings.EditorOptions.ExtraCharSpacing;
//  Editor.BracketHighlightStyle := Settings.EditorOptions.BracketHighlightStyle;
//  Editor.BlockTabIndent        := Settings.EditorOptions.BlockTabIndent;
//  Editor.BlockIndent           := Settings.EditorOptions.BlockIndent;
//  Editor.RightEdge             := Settings.EditorOptions.RightEdge;


//
//  if Settings.EditorOptions.AutoIndentOnPaste then
//    Editor.Options := Editor.Options + [eoAutoIndentOnPaste]
//  else
//    Editor.Options := Editor.Options - [eoAutoIndentOnPaste];
//
//  if Settings.EditorOptions.SmartTabs then
//    Editor.Options := Editor.Options + [eoSmartTabs]
//  else
//    Editor.Options := Editor.Options - [eoSmartTabs];
//
//  if Settings.EditorOptions.EnhanceHomeKey then
//    Editor.Options := Editor.Options + [eoEnhanceHomeKey]
//  else
//    Editor.Options := Editor.Options - [eoEnhanceHomeKey];
//
//

//
//  if Settings.EditorOptions.BracketHighlight then
//    Editor.Options := Editor.Options + [eoBracketHighlight]
//  else
//    Editor.Options := Editor.Options - [eoBracketHighlight];
//

//
//  if Settings.EditorOptions.EnhanceEndKey then
//    Editor.Options2 := Editor.Options2 + [eoEnhanceEndKey]
//  else
//    Editor.Options2 := Editor.Options2 - [eoEnhanceEndKey];
//
//  if Settings.EditorOptions.CaretSkipsSelection then
//    Editor.Options2 := Editor.Options2 + [eoCaretSkipsSelection]
//  else
//    Editor.Options2 := Editor.Options2 - [eoCaretSkipsSelection];
//
//  if Settings.EditorOptions.CaretSkipsTab then
//    Editor.Options2 := Editor.Options2 + [eoCaretSkipTab]
//  else
//    Editor.Options2 := Editor.Options2 - [eoCaretSkipTab];
//
//  if Settings.EditorOptions.AlwaysVisibleCaret then
//    Editor.Options2 := Editor.Options2 + [eoAlwaysVisibleCaret]
//  else
//    Editor.Options2 := Editor.Options2 - [eoAlwaysVisibleCaret];
//
//  if Settings.EditorOptions.FoldedCopyPaste then
//    Editor.Options2 := Editor.Options2 + [eoFoldedCopyPaste]
//  else
//    Editor.Options2 := Editor.Options2 - [eoFoldedCopyPaste];
//
//  if Settings.EditorOptions.PersistentBlock then
//    Editor.Options2 := Editor.Options2 + [eoPersistentBlock]
//  else
//    Editor.Options2 := Editor.Options2 - [eoPersistentBlock];
//
//  if Settings.EditorOptions.OverwriteBlock then
//    Editor.Options2 := Editor.Options2 + [eoOverwriteBlock]
//  else
//    Editor.Options2 := Editor.Options2 - [eoOverwriteBlock];
//
//  if Settings.EditorOptions.AutoHideCursor then
//    Editor.Options2 := Editor.Options2 + [eoAutoHideCursor]
//  else
//    Editor.Options2 := Editor.Options2 - [eoAutoHideCursor];
//
//  Editor.MouseLinkColor     := Settings.Colors.MouseLinkColor;
//  Editor.BracketMatchColor  := Settings.Colors.BracketMatchColor;
//  Editor.LineHighlightColor := Settings.Colors.LineHighlightColor;
//  Editor.FoldedCodeColor    := Settings.Colors.FoldedCodeColor;
//  Editor.HighlightAllColor  := Settings.Colors.HighlightAllColor;
//  Editor.SelectedColor      := Settings.Colors.SelectedColor;
//  Editor.IncrementColor     := Settings.Colors.IncrementColor;
//  Editor.RightEdgeColor     := Settings.Colors.RightEdgeColor;
//
//  Editor.Refresh; // will repaint using the actual highlighter settings

  // alternative block selection color?
  //Editor.UseIncrementalColor := False;
  //Editor.IncrementColor.Background := clLtGray;
  //Editor.IncrementColor.Foreground := clNone;

  // highlight all search matches after search operation
  //Editor.HighlightAllColor.Background := $0064B1FF;  // light orange
  //Editor.HighlightAllColor.FrameColor := $0064B1FF;
  ////Editor.HighlightAllColor.FrameColor := $004683FF;  // dark orange
  //Editor.HighlightAllColor.FrameStyle := slsSolid;
end;

procedure TEditorView.InitializeEditor(AEditor: TBCEditor);
var
  N: Integer;
begin
  AEditor.Parent := Self;
  AEditor.Align := alClient;
  AEditor.Font.Assign(Settings.EditorFont);

  AEditor.BorderStyle := bsNone;
  AEditor.DoubleBuffered := True;
  AEditor.Options := [
    eoAutoIndent,
    eoDragDropEditing,
    eoDropFiles,
    eoTrimTrailingSpaces
  ];

  AEditor.OnChange           := EditorChange;
  AEditor.OnReplaceText      := EditorReplaceText;
  AEditor.OnCaretChanged     := EditorCaretChanged;
  AEditor.OnCommandProcessed := EditorCommandProcessed;
  AEditor.OnDropFiles        := EditorDropFiles;

  AEditor.Highlighter.Colors.LoadFromFile('tsColors.json');

  //AEditor.CodeFolding.Options := AEditor.CodeFolding.Options + [cfoFoldMultilineComments];

  AEditor.CodeFolding.Visible := True;
  AEditor.CodeFolding.Options :=  [
    //cfoFoldMultilineComments,
    cfoHighlightFoldingLine,
    //cfoHighlightIndentGuides,
    cfoHighlightMatchingPair,
    cfoShowCollapsedCodeHint,
    //cfoShowCollapsedLine,
    //cfoShowIndentGuides,
    cfoUncollapseByHintClick

  ];

  AEditor.URIOpener := True;
  AEditor.Font.Size := 11;


//  with AEditor.Gutter.LineNumberPart do
//  begin
//    Width := 15;
//    MarkupInfo.Background := clNone;
//    MarkupInfo.Foreground := clGray;
//    MarkupInfo.StyleMask := [fsBold];
//    DigitCount := 2;
//    ShowOnlyLineNumbersMultiplesOf := 10;
//    ZeroStart := False;
//    LeadingZeros := False;
//  end;
//  with AEditor.Gutter.ChangesPart do
//  begin
//    Width := 4;
//    ModifiedColor := 59900;
//    SavedColor := clGreen;
//  end;
//  with AEditor.Gutter.CodeFoldPart do
//  begin
//    MarkupInfo.Background := clNone;
//    MarkupInfo.Foreground := clMedGray;
//  end;
//  with AEditor.Gutter.MarksPart do
//  begin
//    Width := 1;
//    Visible := True;
//  end;
//
//  AEditor.Options := [
//    eoAltSetsColumnMode,
//    eoAutoIndent,        // Will indent the caret on new lines with the same amount of leading white space as the preceding line
//    eoAutoIndentOnPaste,
//    eoEnhanceHomeKey,   // home key jumps to line start if nearer, similar to visual studio
//    eoGroupUndo,       // When undoing/redoing actions, handle all continous changes of the same kind in one call instead undoing/redoing each command separately
//    eoHalfPageScroll,   // When scrolling with page-up and page-down commands, only scroll a half page at a time
//    eoSmartTabs,        // When tabbing, the cursor will go to the next non-white space character of the previous line
//    eoTabIndent,        // When active <Tab> and <Shift><Tab> act as block indent, unindent when text is selected
//    eoTabsToSpaces,    // Converts a tab character to a specified number of space characters
//    eoTrimTrailingSpaces,  // Spaces at the end of lines will be trimmed and not saved
//    eoBracketHighlight, // Highlight matching bracket
//    eoShowCtrlMouseLinks,
//    eoScrollPastEol,         // makes column selections easier
//    eoDragDropEditing,
////    eoPersistentCaret,     // don't use! bug in TSynEdit
//    eoShowScrollHint
//  ];
//
//  AEditor.Options2 := [
//    eoEnhanceEndKey,
//    eoFoldedCopyPaste,
//    eoOverwriteBlock
//  ];
//  AEditor.MouseOptions := [
//    emAltSetsColumnMode,
//    emDragDropEditing,
//    emCtrlWheelZoom,
//    emShowCtrlMouseLinks
//  ];
//  AEditor.ScrollBars := ssAutoBoth;
//
//  AEditor.OnChange             := EditorChange;
//  AEditor.OnMouseLink          := EditorMouseLink;
//  AEditor.OnClickLink          := EditorClickLink;
//  AEditor.OnCutCopy            := EditorCutCopy;
//  AEditor.OnPaste              := EditorPaste;
//  AEditor.OnProcessCommand     := EditorProcessCommand;
//  AEditor.OnProcessUserCommand := EditorProcessUserCommand;
//  AEditor.OnGutterClick        := EditorGutterClick;
//  AEditor.OnClearBookmark      := EditorClearBookmark;
//  AEditor.OnSpecialLineMarkup  := EditorSpecialLineMarkup;
//  AEditor.OnChangeUpdating     := EditorChangeUpdating;
//  AEditor.OnCommandProcessed   := EditorCommandProcessed;
//  AEditor.OnReplaceText        := EditorReplaceText;
//  AEditor.RegisterMouseActionExecHandler(EditorMouseActionExec);
//  AEditor.RegisterMouseActionSearchHandler(EditorMouseActionSearch);
  ActiveControl := Editor;
end;

procedure TEditorView.EditorSettingsChanged(ASender: TObject);
begin
  FUpdate := True;
end;
procedure TEditorView.EditorSpecialLineColors(Sender: TObject; Line: Integer;
  var Special: Boolean; var FG, BG: TColor);
begin
  Logger.Enter('TEditorView.EditorSpecialLineColors');

  Logger.Leave('TEditorView.EditorSpecialLineColors');
end;

{$ENDREGION}

{$REGION'protected methods'}
procedure TEditorView.BeginUndoBlock;
begin
  Editor.BeginUndoBlock;
end;

procedure TEditorView.BeginUpdate;
begin
  Editor.BeginUpdate;
end;

procedure TEditorView.EndUndoBlock;
begin
  Editor.EndUndoBlock;
end;

procedure TEditorView.EndUpdate;
begin
  Editor.EndUpdate;
end;

procedure TEditorView.CopyToClipboard;
begin
  Editor.CopyToClipboard;
end;

procedure TEditorView.Cut;
begin
  if Editor.Focused then
  begin
    if not Editor.SelectionAvailable then
      SelectWord;
    Editor.CutToClipboard;
  end
end;

procedure TEditorView.Copy;
begin
  if Editor.Focused then
  begin
    if not Editor.SelectionAvailable then
    begin
      SelectWord;
    end;
    Editor.CopyToClipboard;
  end
end;

procedure TEditorView.Paste;
begin
  if Editor.Focused and CanPaste then;
  begin
    Editor.PasteFromClipboard;
  end;
end;

procedure TEditorView.Undo;
begin
  Editor.DoUndo;
end;

procedure TEditorView.Redo;
begin
  Editor.DoRedo;
end;

{ Make current editor instance the active one in the editor manager. This does
  not automatically make it focused as the current focus can be set to eg. a
  tool window. }

procedure TEditorView.Activate;
begin
  inherited;
  Manager.ActiveView := Self as IEditorView;
end;

function TEditorView.EditorViewFocused: Boolean;
begin
  Result := Focused or Editor.Focused;
end;

procedure TEditorView.SearchAndSelectLine(ALineIndex: Integer; const ALine: string);
begin
  try
    //Editor.SearchReplaceEx(ALine, '', [ssoWholeWord], Point(0, ALineIndex));
  except
    // don't handle exceptions
  end;
end;

procedure TEditorView.SearchAndSelectText(const AText: string);
begin
  try
    //Editor.SearchReplaceEx(AText, '', [], Point(0, 0));
  except
    // don't handle exceptions
  end;
end;

procedure TEditorView.SelectWord;
begin
  Editor.SelectionBeginPosition := Editor.WordStart;
  Editor.SelectionEndPosition   := Editor.WordEnd;
end;

{ Clears all highlighted search matches of the last search operation. }

procedure TEditorView.ClearHighlightSearch;
begin
  //SetHighlightSearch('', []);
end;

procedure TEditorView.Clear;
begin
  Editor.Clear;
end;

procedure TEditorView.SelectAll;
begin
  if Editor.Focused then
    Editor.SelectAll
end;

procedure TEditorView.FindNextWordOccurrence(ADirectionForward: Boolean);
//var
//  StartX, EndX: Integer;
//  Flags: TSynSearchOptions;
//  LogCaret: TPoint;
begin
//  StartX := 0;
//  EndX   := Editor.MaxLeftChar;
//  LogCaret := LogicalCaretXY;
//  Editor.GetWordBoundsAtRowCol(LogCaret, StartX, EndX);
//  if EndX <= StartX then
//    Exit;
//  Flags := [ssoWholeWord];
//  if ADirectionForward then
//  begin
//    LogCaret.X := EndX;
//  end
//  else
//  begin
//    LogCaret.X := StartX;
//    Include(Flags, ssoBackwards);
//  end;
//  LogicalCaretXY := LogCaret;
//  Editor.SearchReplace(Editor.GetWordAtRowCol(LogCaret), '', Flags);
end;

//procedure TEditorView.SetHighlightSearch(const ASearch: string; AOptions: TSynSearchOptions);
//begin
////  try
////    Editor.SetHighlightSearch(ASearch, AOptions);
////  except
////    // TO TEST
////  end;
//end;

procedure TEditorView.UpdateActions;
var
  B: Boolean;
begin
  inherited UpdateActions;
  B := Focused;
  if not B and Assigned(Parent) then
  begin
    if Parent.Focused then
      B := True;
  end;

  if B then
  begin
    Activate;
  end;

  if IsActive then
  begin
//    Editor.Color := clWhite;
//    UpdateSharedViews;
  end
  else
  begin
    if Settings.DimInactiveView then
//      Editor.Color := GetHighLightColor(15329769, 10);
  end;

  if Assigned(Actions) then
    Actions.UpdateActions;

  if FUpdate then
  begin
    ApplySettings;
    FUpdate := False;
  end;
end;

{$ENDREGION}

{$REGION'public methods'}
function TEditorView.CloseQuery: Boolean;
var
  MR: TModalResult;
  S : string;
  V : IEditorView;
begin
  V := nil;
  Result := inherited CloseQuery;
  if Modified then
  begin
    if Manager.ActiveView <> (Self as IEditorView) then
    begin
      V := Manager.ActiveView;
      Activate;
    end;
    S := Format(SAskSaveChanges, [FileName]);
    MR := MessageDlg(S, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if MR = mrYes then
    begin
      Result := Manager.SaveFile;
    end
    else if MR = mrNo then
    begin
      Result := True;
    end
    else
    begin
      // TODO: Why can't we prevent closing by setting Result to False?
      Result := False;
      Abort;
    end;
  end;
  if Assigned(V) then
  begin
    V.Activate;
  end;
end;

{  When IsFile is true this loads the given filenameinto the editor view. When
  IsFile is false, the given storagename is passed to an event which can be
  handled by the owning application to load the content from another resource
  like eg. a database table. }

procedure TEditorView.Load(const AStorageName: string);
var
  S  : string;
  FS : TFileStream;
begin
  Events.DoLoad(AStorageName);
  if IsFile then
  begin
    if (AStorageName <> '') and FileExists(AStorageName) then
      FileName := AStorageName;

    FS := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
    try
      LoadFromStream(FS);
    finally
      FreeAndNil(FS);
    end;
    LineBreakStyle := ALineBreakStyles[GuessLineBreakStyle(Text)];
    S := ExtractFileExt(FileName);
    S := System.Copy(S, 2, Length(S));
    try
      if FileIsText(FileName) then
      begin
        AssignHighlighterForFileType(S);
      end;
    except
      { TODO -oTS : dirty: need to fix this }
      // for an unknown reason an EAbort is raised
    end;
    Modified := False;
  end;
end;

procedure TEditorView.LoadFromStream(AStream: TStream);
begin
  Editor.LoadFromStream(AStream);
end;

procedure TEditorView.SaveToStream(AStream: TStream);
begin
  Editor.SaveToStream(AStream);
end;

procedure TEditorView.Save(const AStorageName: string);
var
  FS : TFileStream;
begin
  Events.DoBeforeSave(AStorageName);
  if IsFile then
  begin
    FS := TFileStream.Create(AStorageName, fmCreate);
    try
      SaveToStream(FS);
    finally
      FreeAndNil(FS);
    end;
  end;
  Events.DoAfterSave(AStorageName);
  Modified := False;
end;

function TEditorView.GetWordAtPosition(const APosition: TPoint): string;
//var
//  CaretPos: TPoint;
begin
//  Result := '';

//  Caretpos := Editor.PhysicalToLogicalPos(APosition);
//  Result := GetWordFromCaret(CaretPos);
end;

function TEditorView.GetWordFromCaret(const ACaretPos: TPoint): string;
begin
//  Result := Editor. GetWordAtRowCol(ACaretPos);
  //Result := Editor.Get
end;

//function TEditorView.GetHighlighterAttriAtRowCol(APosition: TPoint;
//  out AToken: string; out AAttri: TSynHighlighterAttributes): Boolean;
//begin
//  Result := Editor.GetHighlighterAttriAtRowCol(APosition, AToken, AAttri);
//end;
{$ENDREGION}

end.

{$REGION'Keyboard shortcuts'}
(*//F1                      Topic Search
//Ctrl+F1                Topic Search
  ecNextEditor: SetResult(VK_F6,[]);
  ecPrevEditor: SetResult(VK_F6,[ssShift]);
  ecWordLeft:   SetResult(VK_A,[ssCtrlOS],VK_LEFT,[ssCtrlOS]);
  ecPageDown:   SetResult(VK_C,[ssCtrlOS],VK_NEXT,[]);
//Ctrl+D                 Moves the cursor right one column, accounting for the
//autoindent setting
//Ctrl+E                 Moves the cursor up one line
//Ctrl+F                 Moves one word right
//Ctrl+G                 Deletes the character to the right of the cursor
//Ctrl+H                 Deletes the character to the left of the cursor
//Ctrl+I                  Inserts a tab
//Ctrl+L                 Search|Search Again
//Ctrl+N                 Inserts a new line
//Ctrl+P                 Causes next character to be interpreted as an ASCII
//sequence
//Ctrl+R                 Moves up one screen
//Ctrl+S                 Moves the cursor left one column, accounting for the
//autoindent setting
//Ctrl+T                 Deletes a word
//Ctrl+V                 Turns insert mode on/off
//Ctrl+W                Moves down one screen
//Ctrl+X                 Moves the cursor down one line
//Ctrl+Y                 Deletes a line
//Ctrl+Z                 Moves the cursor up one line
//Ctrl+Shift+S          Performs an incremental search

//Block commands:
//---------------
//Ctrl+K+B      Marks the beginning of a block
//Ctrl+K+C      Copies a selected block
//Ctrl+K+H      Hides/shows a selected block
//Ctrl+K+I       Indents a block by the amount specified in the Block Indent
//combo box on the General page of the Editor Options dialog box.
//Ctrl+K+K      Marks the end of a block
//Ctrl+K+L       Marks the current line as a block
//Ctrl+K+N      Changes a block to uppercase
//Ctrl+K+O      Changes a block to lowercase
//Ctrl+K+P      Prints selected block
//Ctrl+K+R      Reads a block from a file
//Ctrl+K+T       Marks a word as a block
//Ctrl+K+U      Outdents a block by the amount specified in the Block Indent
//combo box on the General page of the Editor Options dialog box.
//Ctrl+K+V      Moves a selected block
//Ctrl+K+W      Writes a selected block to a file
//Ctrl+K+Y      Deletes a selected block
//Ctrl+O+C      Turns on column blocking
//Ctrl+O+I       Marks an inclusive block
//Ctrl+O+K      Turns off column blocking
//Ctrl+O+L      Marks a line as a block
//Shift+Alt+arrow Selects column-oriented blocks
//Click+Alt+mousemv Selects column-oriented blocks
//Ctrl+Q+B      Moves to the beginning of a block
//Ctrl+Q+K      Moves to the end of a block

//Miscellaneous commands:
//-----------------------
//Ctrl+K+D      Accesses the menu bar
//Ctrl+K+E       Changes a word to lowercase
//Ctrl+K+F       Changes a word to uppercase
//Ctrl+K+S      File|Save (Default and IDE Classic only)
//Ctrl+Q+A      Search|Replace
//Ctrl+Q+F      Search|Find
//Ctrl+Q+Y      Deletes to the end of a line
//Ctrl+Q+[       Finds the matching delimiter (forward)
//Ctrl+Q+Ctrl+[ Finds the matching delimiter (forward)
//Ctrl+Q+]       Finds the matching delimiter (backward)
//Ctrl+Q+Ctrl+] Finds the matching delimiter (backward)
//Ctrl+O+A      Open file at cursor
//Ctrl+O+B      Browse symbol at cursor (Delphi only)
//Alt+right arrow  For code browsing
//Alt +left arrow For code browsing
//Ctrl+O+G      Search|Go to line number
//Ctrl+O+O      Inserts compiler options and directives
//Ctrl+O+U      Toggles case
//Bookmark commands:
//------------------
//Shortcut       Action
//Ctrl+K+0       Sets bookmark 0
//Ctrl+K+1       Sets bookmark 1
//Ctrl+K+2       Sets bookmark 2
//Ctrl+K+3       Sets bookmark 3
//Ctrl+K+4       Sets bookmark 4
//Ctrl+K+5       Sets bookmark 5
//Ctrl+K+6       Sets bookmark 6
//Ctrl+K+7       Sets bookmark 7
//Ctrl+K+8       Sets bookmark 8
//Ctrl+K+9       Sets bookmark 9
//Ctrl+K+Ctrl+0 Sets bookmark 0
//Ctrl+K+Ctrl+1 Sets bookmark 1
//Ctrl+K+Ctrl+2 Sets bookmark 2
//Ctrl+K+Ctrl+3 Sets bookmark 3
//Ctrl+K+Ctrl+4 Sets bookmark 4
//Ctrl+K+Ctrl+5 Sets bookmark 5
//Ctrl+K+Ctrl+6 Sets bookmark 6
//Ctrl+K+Ctrl+7 Sets bookmark 7
//Ctrl+K+Ctrl+8 Sets bookmark 8
//Ctrl+K+Ctrl+9 Sets bookmark 9
//Ctrl+Q+0       Goes to bookmark 0
//Ctrl+Q+1       Goes to bookmark 1
//Ctrl+Q+2       Goes to bookmark 2
//Ctrl+Q+3       Goes to bookmark 3
//Ctrl+Q+4       Goes to bookmark 4
//Ctrl+Q+5       Goes to bookmark 5
//Ctrl+Q+6       Goes to bookmark 6
//Ctrl+Q+7       Goes to bookmark 7
//Ctrl+Q+8       Goes to bookmark 8
//Ctrl+Q+9       Goes to bookmark 9
//Ctrl+Q+Ctrl+0 Goes to bookmark 0
//Ctrl+Q+Ctrl+1 Goes to bookmark 1
//Ctrl+Q+Ctrl+2 Goes to bookmark 2
//Ctrl+Q+Ctrl+3 Goes to bookmark 3
//Ctrl+Q+Ctrl+4 Goes to bookmark 4
//Ctrl+Q+Ctrl+5 Goes to bookmark 5
//Ctrl+Q+Ctrl+6 Goes to bookmark 6
//Ctrl+Q+Ctrl+7 Goes to bookmark 7
//Ctrl+Q+Ctrl+8 Goes to bookmark 8
//Ctrl+Q+Ctrl+9 Goes to bookmark 9
//Cursor movement:
//----------------
//Ctrl+Q+B      Moves to the beginning of a block
//Ctrl+Q+C      Moves to end of a file
//Ctrl+Q+D      Moves to the end of a line
//Ctrl+Q+E      Moves the cursor to the top of the window
//Ctrl+Q+K      Moves to the end of a block
//Ctrl+Q+P      Moves to previous position
//Ctrl+Q+R      Moves to the beginning of a file
//Ctrl+Q+S      Moves to the beginning of a line
//Ctrl+Q+T      Moves the viewing editor so that the current line is placed at
//the top of the window
//Ctrl+Q+U      Moves the viewing editor so that the current line is placed at
//the bottom of the window, if possible
//Ctrl+Q+X      Moves the cursor to the bottom of the window
//System keys:
//------------

//F1              Displays context-sensitive Help
//F2              File|Save
//F3              File|Open
//F4              Run to Cursor
//F5              Zooms window
//F6              Displays the next page
//F7              Run|Trace Into
//F8              Run|Step Over
//F9              Run|Run
//F11             View|Object Inspector
//F12             View|Toggle Form/Unit
//Alt+0           View|Window List
//Alt+F2          View|CPU
//Alt+F3          File|Close
//Alt+F7          Displays previous error in Message view
//Alt+F8          Displays next error in Message view
//Alt+F11        File|Use Unit (Delphi)
//Alt+F11        File|Include Unit Hdr (C++)
//Alt+F12        Displays the Code editor
//Alt+X           File|Exit
//Alt+right arrow  For code browsing forward
//Alt +left arrow For code browsing backward
//Alt +up arrow  For code browsing Ctrl-click on identifier
//Alt+Page Down Goes to the next tab
//Alt+Page Up   Goes to the previous tab
//Ctrl+F1        Topic Search
//Ctrl+F2        Run|Program Reset
//Ctrl+F3        View|Call Stack
//Ctrl+F6        Open Source/Header file (C++)
//Ctrl+F7        Add Watch at Cursor
//Ctrl+F8        Toggle Breakpoint
//Ctrl+F9        Project|Compile project (Delphi)
//Ctrl+F9        Project|Make project (C++)
//Ctrl+F11       File|Open Project
//Ctrl+F12       View|Units
//Shift+F7       Run|Trace To Next Source Line
//Shift+F11      Project|Add To Project
//Shift+F12      View|Forms
//Ctrl+D         Descends item (replaces Inspector window)
//Ctrl+N         Opens a new Inspector window
//Ctrl+S          Incremental search
//Ctrl+T          Displays the Type Cast dialog
  else
    GetDefaultKeyForCommand(Command,TheKeyA,TheKeyB);
  end;
*)
{$ENDREGION}
