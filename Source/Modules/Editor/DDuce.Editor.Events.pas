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

unit DDuce.Editor.Events;

{ Events dispatched by the IEditorManager and active IEditorView instance.

  TMethodList is used to emulate multicast events hence these events can be
  dispatched to multiple event handlers. This is an implementation of the
  observer pattern where multiple observers can respond to changes in a subject.
}

interface

uses
  System.Classes, System.SysUtils,

  DDuce.Editor.Types, DDuce.Editor.Interfaces;

type

  { TActionExecuteEvents }

//  TActionExecuteEvents = class(TMethodList)
//    procedure CallEvents(
//          Sender   : TObject;
//          AAction  : TBasicAction;
//      var AHandled : Boolean);
//  end;

  { TCaretPositionEvents }

//  TCaretPositionEvents = class(TMethodList)
//    procedure CallEvents(Sender: TObject; X, Y: Integer);
//  end;

  { TEditorEvents }

  TEditorEvents = class(TInterfacedObject, IEditorEvents)
  strict private
    FManager                 : IEditorManager;
//    FChangeEvents            : TMethodList;
//    FModifiedEvents          : TMethodList;
//    FActiveViewChangeEvents  : TMethodList;
//    FHighlighterChangeEvents : TMethodList;
//    FCaretPositionEvents     : TCaretPositionEvents;
//    FActionExecuteEvents     : TActionExecuteEvents;

    FOnAddEditorView       : TAddEditorViewEvent;
    FOnShowEditorToolView  : TEditorToolViewEvent;
    FOnHideEditorToolView  : TEditorToolViewEvent;
//    FOnMacroStateChange    : TMacroStateChangeEvent;
    FOnNew                 : TNewEvent;
    FOnLoad                : TStorageEvent;
    FOnOpen                : TStorageEvent;
    FOnBeforeSave          : TStorageEvent;
    FOnAfterSave           : TStorageEvent;
    FOnSave                : TStorageEvent;
    FOnOpenOtherInstance   : TOpenOtherInstanceEvent;
//    FOnStatusChange        : TStatusChangeEvent;
    FOnStatusMessage       : TStatusMessageEvent;

  strict protected
    function GetOnOpen: TStorageEvent;
    procedure SetOnOpen(AValue: TStorageEvent);
    function GetOnAfterSave: TStorageEvent;
    function GetOnBeforeSave: TStorageEvent;
    function GetView: IEditorView;
    function GetOnAddEditorView: TAddEditorViewEvent;
    function GetOnHideEditorToolView: TEditorToolViewEvent;
    function GetOnNew: TNewEvent;
    function GetOnLoad: TStorageEvent;
    function GetOnOpenOtherInstance: TOpenOtherInstanceEvent;
    function GetOnSave: TStorageEvent;
    function GetOnShowEditorToolView: TEditorToolViewEvent;
//    function GetOnStatusChange: TStatusChangeEvent;
    procedure SetOnAddEditorView(AValue: TAddEditorViewEvent);
    procedure SetOnHideEditorToolView(AValue: TEditorToolViewEvent);
    procedure SetOnNew(const AValue: TNewEvent);
    procedure SetOnLoad(const AValue: TStorageEvent);
    procedure SetOnSave(const AValue: TStorageEvent);
    procedure SetOnOpenOtherInstance(AValue: TOpenOtherInstanceEvent);
    procedure SetOnShowEditorToolView(AValue: TEditorToolViewEvent);
//    procedure SetOnStatusChange(const AValue: TStatusChangeEvent);
    procedure SetOnAfterSave(AValue: TStorageEvent);
    procedure SetOnBeforeSave(AValue: TStorageEvent);

    { will get called by owner to trigger the events }
    procedure DoChange; virtual;
    procedure DoModified; virtual;
    procedure DoHighlighterChange; virtual;
    procedure DoActiveViewChange; virtual;
    procedure DoAddEditorView(AEditorView: IEditorView); virtual;
    procedure DoShowToolView(AToolView: IEditorToolView); virtual;
    procedure DoHideToolView(AToolView: IEditorToolView); virtual;
    procedure DoCaretPositionChange; virtual;
    procedure DoActionExecute(AAction: TBasicAction; var AHandled: Boolean);
    procedure DoOpenOtherInstance(const AParams: array of string); virtual;
    procedure DoStatusMessage(AText: string); virtual;
//    procedure DoStatusChange(AChanges: TSynStatusChanges); virtual;
    procedure DoOpen(const AName: string);
    procedure DoBeforeSave(const AName: string);
    procedure DoAfterSave(const AName: string);
    procedure DoLoad(const AName: string);
    procedure DoNew(
      const AName : string = '';
      const AText        : string = ''
    );

    procedure AddOnChangeHandler(AEvent: TNotifyEvent);
    procedure AddOnModifiedHandler(AEvent: TNotifyEvent);
    procedure AddOnHighlighterChangeHandler(AEvent: TNotifyEvent);
    procedure AddOnActiveViewChangeHandler(AEvent: TNotifyEvent);
    procedure AddOnCaretPositionEvent(AEvent: TCaretPositionEvent);
    procedure AddOnActionExecuteEvent(AEvent: TActionExecuteEvent);

    procedure RemoveOnChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveOnModifiedHandler(AEvent: TNotifyEvent);
    procedure RemoveOnHighlighterChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveOnActiveViewChangeHandler(AEvent: TNotifyEvent);
    procedure RemoveOnCaretPositionEvent(AEvent: TCaretPositionEvent);
    procedure RemoveOnActionExecuteEvent(AEvent: TActionExecuteEvent);

    property OnAddEditorView: TAddEditorViewEvent
      read GetOnAddEditorView write SetOnAddEditorView;

    property OnShowEditorToolView: TEditorToolViewEvent
      read GetOnShowEditorToolView write SetOnShowEditorToolView;

    property OnHideEditorToolView: TEditorToolViewEvent
      read GetOnHideEditorToolView write SetOnHideEditorToolView;

//    property OnStatusChange: TStatusChangeEvent
//      read GetOnStatusChange write SetOnStatusChange;

    { Called when content is loaded into the editor's buffer. }
    property OnLoad: TStorageEvent
      read GetOnLoad write SetOnLoad;

    { Called when the 'New' action is executed by user }
    property OnNew: TNewEvent
      read GetOnNew write SetOnNew;

    { Called when the editor's content is about to be saved. }
    property OnSave: TStorageEvent
      read GetOnSave write SetOnSave;

    { Called when the 'Open file' action is executed by user. }
    property OnOpen: TStorageEvent
      read GetOnOpen write SetOnOpen;

    property OnOpenOtherInstance: TOpenOtherInstanceEvent
      read GetOnOpenOtherInstance write SetOnOpenOtherInstance;

    property View: IEditorView
      read GetView;

  public
    constructor Create(AManager: IEditorManager);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{ TActionExecuteEvents }

//procedure TActionExecuteEvents.CallEvents(Sender: TObject;
//  AAction: TBasicAction; var AHandled: Boolean);
//var
//  I: Integer;
//begin
//  I := Count;
//  while NextDownIndex(I) do
//    TActionExecuteEvent(Items[I])(Sender, AAction, AHandled);
//end;

{$region 'TCaretPositionEvents' /fold}
{$region 'public methods' /fold}
//procedure TCaretPositionEvents.CallEvents(Sender: TObject; X, Y: Integer);
//var
//  I: Integer;
//begin
//  I := Count;
//  while NextDownIndex(I) do
//    TCaretPositionEvent(Items[I])(Sender, X, Y);
//end;
{$endregion}
{$endregion}

{$region 'TEditorEvents' /fold}
{$region 'construction and destruction' /fold}
constructor TEditorEvents.Create(AManager: IEditorManager);
begin
  inherited Create;
  FManager := AManager;
end;

procedure TEditorEvents.AfterConstruction;
begin
  inherited AfterConstruction;
//  FChangeEvents            := TMethodList.Create;
//  FModifiedEvents          := TMethodList.Create;
//  FActiveViewChangeEvents  := TMethodList.Create;
//  FHighlighterChangeEvents := TMethodList.Create;
//  FCaretPositionEvents     := TCaretPositionEvents.Create;
//  FActionExecuteEvents     := TActionExecuteEvents.Create;
end;

procedure TEditorEvents.BeforeDestruction;
begin
  FManager := nil;
//  FChangeEvents.Free;
//  FModifiedEvents.Free;
//  FActiveViewChangeEvents.Free;
//  FCaretPositionEvents.Free;
//  FActionExecuteEvents.Free;
//  FHighlighterChangeEvents.Free;
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'property access mehods' /fold}
function TEditorEvents.GetOnOpen: TStorageEvent;
begin
  Result := FOnOpen;
end;

procedure TEditorEvents.SetOnOpen(AValue: TStorageEvent);
begin
  FOnOpen := AValue;
end;

function TEditorEvents.GetOnAfterSave: TStorageEvent;
begin
  Result := FOnAfterSave;
end;

function TEditorEvents.GetOnBeforeSave: TStorageEvent;
begin
  Result := FOnBeforeSave;
end;

function TEditorEvents.GetView: IEditorView;
begin
  Result := FManager as IEditorView;
end;

function TEditorEvents.GetOnAddEditorView: TAddEditorViewEvent;
begin
  Result := FOnAddEditorView;
end;

function TEditorEvents.GetOnHideEditorToolView: TEditorToolViewEvent;
begin
  Result := FOnHideEditorToolView;
end;

function TEditorEvents.GetOnNew: TNewEvent;
begin
  Result := FOnNew;
end;

function TEditorEvents.GetOnOpenOtherInstance: TOpenOtherInstanceEvent;
begin
  Result := FOnOpenOtherInstance;
end;

function TEditorEvents.GetOnSave: TStorageEvent;
begin
  Result := FOnSave;
end;

procedure TEditorEvents.SetOnSave(const AValue: TStorageEvent);
begin
  FOnSave :=  AValue;
end;

function TEditorEvents.GetOnLoad: TStorageEvent;
begin
  Result := FOnLoad;
end;

procedure TEditorEvents.SetOnLoad(const AValue: TStorageEvent);
begin
  FOnLoad := AValue;
end;

function TEditorEvents.GetOnShowEditorToolView: TEditorToolViewEvent;
begin
  Result := FOnShowEditorToolView;
end;

//function TEditorEvents.GetOnStatusChange: TStatusChangeEvent;
//begin
//  Result := FOnStatusChange;
//end;

procedure TEditorEvents.SetOnAddEditorView(AValue: TAddEditorViewEvent);
begin
  FOnAddEditorView := AValue;
end;

procedure TEditorEvents.SetOnHideEditorToolView(AValue: TEditorToolViewEvent);
begin
  FOnHideEditorToolView := AValue;
end;

procedure TEditorEvents.SetOnNew(const AValue: TNewEvent);
begin
  FOnNew := AValue;
end;

procedure TEditorEvents.SetOnOpenOtherInstance(AValue: TOpenOtherInstanceEvent);
begin
  FOnOpenOtherInstance := AValue;
end;

procedure TEditorEvents.SetOnShowEditorToolView(AValue: TEditorToolViewEvent);
begin
  FOnShowEditorToolView := AValue;
end;

//procedure TEditorEvents.SetOnStatusChange(const AValue: TStatusChangeEvent);
//begin
//  FOnStatusChange := AValue;
//end;

procedure TEditorEvents.SetOnAfterSave(AValue: TStorageEvent);
begin
  FOnAfterSave := AValue;
end;

procedure TEditorEvents.SetOnBeforeSave(AValue: TStorageEvent);
begin
  FOnBeforeSave := AValue;
end;

{$endregion}

{$region 'event dispatch methods' /fold}
procedure TEditorEvents.DoChange;
begin
  //FChangeEvents.CallNotifyEvents(Self);
end;

procedure TEditorEvents.DoModified;
begin
  //FModifiedEvents.CallNotifyEvents(Self);
end;

procedure TEditorEvents.DoHighlighterChange;
begin
  //FHighlighterChangeEvents.CallNotifyEvents(Self);
end;

procedure TEditorEvents.DoActiveViewChange;
begin
  //FActiveViewChangeEvents.CallNotifyEvents(Self);
end;

procedure TEditorEvents.DoAddEditorView(AEditorView: IEditorView);
begin
  if Assigned(FOnAddEditorView) then
    FOnAddEditorView(Self, AEditorView);
end;

procedure TEditorEvents.DoShowToolView(AToolView: IEditorToolView);
begin
  if Assigned(FOnShowEditorToolView) then
    FOnShowEditorToolView(Self, AToolView);
end;

procedure TEditorEvents.DoHideToolView(AToolView: IEditorToolView);
begin
  if Assigned(FOnHideEditorToolView) then
    FOnHideEditorToolView(Self, AToolView);
end;

procedure TEditorEvents.DoCaretPositionChange;
begin
  //FCaretPositionEvents.CallEvents(Self, View.CaretX, View.CaretY);
end;

procedure TEditorEvents.DoActionExecute(AAction: TBasicAction;
  var AHandled: Boolean);
begin
  //FActionExecuteEvents.CallEvents(Self, AAction, AHandled);
end;

procedure TEditorEvents.DoOpenOtherInstance(const AParams: array of string);
begin
  if Assigned(FOnOpenOtherInstance) then
    FOnOpenOtherInstance(Self, AParams);
end;

procedure TEditorEvents.DoStatusMessage(AText: string);
begin
  if Assigned(FOnStatusMessage) then
    FOnStatusMessage(Self, AText);
end;

//procedure TEditorEvents.DoStatusChange(AChanges: TSynStatusChanges);
//begin
//  if Assigned(FOnStatusChange) then
//    FOnStatusChange(Self, AChanges);
//end;

procedure TEditorEvents.DoOpen(const AName: string);
var
  S : string;
begin
  S  := AName;
  if Assigned(FOnOpen) then
    FOnOpen(Self, S);
end;

procedure TEditorEvents.DoBeforeSave(const AName: string);
var
  S: string;
begin
  if Assigned(FOnBeforeSave) then
  begin
    S := View.FileName;
    FOnBeforeSave(Self, S);
    if View.IsFile then
      View.FileName := S;
  end;
end;

procedure TEditorEvents.DoAfterSave(const AName: string);
var
  S: string;
begin
  if Assigned(FOnAfterSave) then
  begin
    S := View.FileName;
    FOnAfterSave(Self, S);
    if View.IsFile then
      View.FileName := S;
  end;
end;

{ Called by an editor view to dispatch an event when the editor is about to
  load a file or other content that corresponds to the given AName.
  Note that AName is not necessarily a filename but can eg. be a name that
  corresponds to a database resource to load the text content from. }

procedure TEditorEvents.DoLoad(const AName: string);
var
  S : string;
begin
  S  := AName;
  if Assigned(FOnLoad) then
    FOnLoad(Self, S);
end;

{ Called by the manager instance to dispatch an event when actNew is executed. }

procedure TEditorEvents.DoNew(const AName: string; const AText: string);
var
  S : string;
begin
  S  := AName;
  if Assigned(FOnNew) then
    FOnNew(Self, S, AText);
end;
{$endregion}

{$region 'protected methods' /fold}
procedure TEditorEvents.AddOnChangeHandler(AEvent: TNotifyEvent);
begin
  //FChangeEvents.Add(TMethod(AEvent));
end;

procedure TEditorEvents.AddOnModifiedHandler(AEvent: TNotifyEvent);
begin
  //FModifiedEvents.Add(TMethod(AEvent));
end;

procedure TEditorEvents.AddOnHighlighterChangeHandler(AEvent: TNotifyEvent);
begin
  //FHighlighterChangeEvents.Add(TMethod(AEvent));
end;

procedure TEditorEvents.AddOnActiveViewChangeHandler(AEvent: TNotifyEvent);
begin
  //FActiveViewChangeEvents.Add(TMethod(AEvent));
end;

procedure TEditorEvents.AddOnCaretPositionEvent(AEvent: TCaretPositionEvent);
begin
  //FCaretPositionEvents.Add(TMethod(AEvent));
end;

procedure TEditorEvents.AddOnActionExecuteEvent(AEvent: TActionExecuteEvent);
begin
  //FActionExecuteEvents.Add(TMethod(AEvent));
end;

procedure TEditorEvents.RemoveOnChangeHandler(AEvent: TNotifyEvent);
begin
  //FChangeEvents.Remove(TMethod(AEvent));
end;

procedure TEditorEvents.RemoveOnModifiedHandler(AEvent: TNotifyEvent);
begin
  //FModifiedEvents.Remove(TMethod(AEvent));
end;

procedure TEditorEvents.RemoveOnHighlighterChangeHandler(AEvent: TNotifyEvent);
begin
 // FHighlighterChangeEvents.Remove(TMethod(AEvent));
end;

procedure TEditorEvents.RemoveOnActiveViewChangeHandler(AEvent: TNotifyEvent);
begin
  //FActiveViewChangeEvents.Remove(TMethod(AEvent));
end;

procedure TEditorEvents.RemoveOnCaretPositionEvent(AEvent: TCaretPositionEvent);
begin
//FCaretPositionEvents.Remove(TMethod(AEvent));
end;

procedure TEditorEvents.RemoveOnActionExecuteEvent(AEvent: TActionExecuteEvent);
begin
  //FActionExecuteEvents.Remove(TMethod(AEvent));
end;
{$endregion}
{$endregion}

end.

