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

{
  The Original Code is part of the LuiPack library for Freepascal/Lazarus.
  The Initial Developer of the Original Code (multilog.pas) is Luiz Américo
  Pereira Câmara (pascalive@bol.com.br). Portions created by the Initial
  Developer are Copyright (C) 2006. All Rights Reserved. You may obtain a copy
  of the original code at http://code.google.com/p/luipack/
}

unit DDuce.Logger;

{$I DDuce.inc}

{
  LogChannels are implemented as follows:
    TCustomLogChannel
      TIPCChannel - TWinIPCClient
      TFileChannel
      TNamedPipesChannel ?
      TSocketChannel ?
}

interface

uses
  Types, Classes, SysUtils, Windows, Rtti, Generics.Collections;

type
  TLogMessageType = (
    lmtInfo        = 0,
    lmtError       = 1,
    lmtWarning     = 2,
    lmtValue       = 3,
    lmtEnterMethod = 4,
    lmtExitMethod  = 5,
    lmtConditional = 6,
    lmtCheckpoint  = 7,
    lmtStrings     = 8,
    lmtCallStack   = 9,
    lmtObject      = 10,
    lmtException   = 11,
    lmtBitmap      = 12,
    lmtHeapInfo    = 13,
    lmtMemory      = 14,
    lmtCustomData  = 15,
    lmtWatch       = 20,
    lmtCounter     = 21,
    lmtClear       = 100
  );

  ILogger = interface;

  TLogMessage = record
    MsgType : TLogMessageType;
    MsgTime : TDateTime;
    MsgText : string;
    Data    : TStream;
  end;

  TCustomDataCallbackMethod = function(
        ASender : ILogger;
        AData   : TValue;
    var ADoSend : Boolean
  ): string of object;

  TCustomDataCallbackFunction = function(
        ASender : ILogger;
        AData   : TValue;
    var ADoSend : Boolean
  ): string;

  { TWinIPCClient }

  { IPC using WM_COPYDATA messages. }

  TWinIPCClient = class (TComponent)
  strict private
    FActive         : Boolean;
    FServerID       : string;
    FServerInstance : string;
    FWindowName     : string;
    FHWND           : HWnd;

    procedure SetActive(const AValue: Boolean);
    procedure SetServerID(const AValue: string);
    procedure SetServerInstance(const AValue: string);

    procedure UpdateWindowName;

  public
    procedure Connect;
    procedure Disconnect;
    function  ServerRunning: Boolean;
    procedure SendStream(AStream: TStream);

    property Active: Boolean
      read FActive write SetActive;

    property ServerID: string
      read FServerID write SetServerID;

    property ServerInstance: string
      read FServerInstance write SetServerInstance;
  end;

  { TCustomLogChannel }

  TCustomLogChannel = class abstract
  private
    FActive: Boolean;

  public
    procedure Clear; virtual; abstract;
    procedure Write(const AMsg: TLogMessage); virtual; abstract;
    procedure Open; virtual; abstract;

    property Active: Boolean
      read FActive write FActive;
  end;

  { TIPCChannel }

  TIPCChannel = class(TCustomLogChannel)
  private
    FClient       : TWinIPCClient;
    FBuffer       : TMemoryStream;
    FClearMessage : TLogMessage;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Clear; override;
    procedure Write(const AMsg: TLogMessage); override;
  end;

  { TFileChannel }

  TFileChannel = class(TCustomLogChannel)
  strict private
    FRelativeIdent : Integer;
    FBaseIdent     : Integer;
    FShowHeader    : Boolean;
    FShowTime      : Boolean;
    FShowPrefix    : Boolean;
    FShowStrings   : Boolean;
    FStreamWriter  : TStreamWriter;

    procedure SetShowTime(const AValue: Boolean);

    function Space(ACount: Integer): string;
    procedure UpdateIdentation;
    procedure WriteStrings(AStream: TStream);
    procedure WriteComponent(AStream: TStream);

  public
    constructor Create(const AFileName: string);
    procedure BeforeDestruction; override;

    procedure Clear; override;
    procedure Write(const AMsg: TLogMessage); override;
    procedure Open; override;

    property ShowHeader: Boolean
      read FShowHeader write FShowHeader;

    property ShowPrefix: Boolean
      read FShowPrefix write FShowPrefix;

    property ShowTime: Boolean
      read FShowTime write SetShowTime;
  end;

  TChannelList = TObjectList<TCustomLogChannel>;

  ILogger = interface(IInterface)
  ['{28E9BADE-6B42-4399-8867-1CA115576E40}']
    function GetChannels: TChannelList;

    procedure Send(const AName: string; AArgs: array of const); overload;
    procedure Send(const AName: string; const AValue: string = ''); overload;
    procedure Send(const AName: string; AValue: Integer); overload;
    procedure Send(const AName: string; AValue: Cardinal); overload;
    procedure Send(const AName: string; AValue: Double); overload;
    procedure Send(const AName: string; AValue: Int64); overload;
    procedure Send(const AName: string; AValue: Boolean); overload;
    procedure Send(const AName: string; const AValue: TRect); overload;
    procedure Send(const AName: string; const AValue: TPoint); overload;
    procedure Send(const AName: string; AValue: TStrings); overload;
    procedure Send(const AName: string; AValue: TObject); overload;
    procedure Send(const AName: string; AValue: TDateTime); overload;
    procedure Send(const AName: string; AValue: TDate); overload;
    procedure Send(const AName: string; AValue: TTime); overload;
    procedure Send(const AName: string; AValue: TValue); overload;

    procedure IncCounter(const AName: string);
    procedure DecCounter(const AName: string);
    procedure ResetCounter(const AName: string);
    function GetCounter(const AName: string): Integer;

    procedure EnterMethod(const AName: string); overload;
    procedure EnterMethod(ASender: TObject; const AName: string); overload;
    procedure ExitMethod(const AName: string); overload;
    procedure ExitMethod(ASender: TObject; const AName: string); overload;

    procedure Watch(const AName: string; const AValue: string); overload;
    procedure Watch(const AName: string; AValue: Integer); overload;
    procedure Watch(const AName: string; AValue: Cardinal); overload;
    procedure Watch(const AName: string; AValue: Double); overload;
    procedure Watch(const AName: string; AValue: Boolean); overload;

    property Channels: TChannelList
      read GetChannels;
  end;

  { TLogger }

  TLogger = class(TInterfacedObject, ILogger)
  private
    FMaxStackCount : Integer;
    FChannels      : TChannelList;
    FLogStack      : TStrings;
    FCheckList     : TStringList;
    FCounterList   : TStringList;
    FOnCustomData  : TCustomDataCallbackMethod;

    procedure SetMaxStackCount(const AValue: Integer);
    function GetChannels: TChannelList;

    // Helper functions
    function RectToStr(const ARect: TRect): string;
    function PointToStr(const APoint: TPoint): string;

  strict protected
    procedure InternalSend(
            AMsgType : TLogMessageType;
      const AText    : string = ''
    );
    procedure InternalSendStream(
            AMsgType : TLogMessageType;
      const AText    : string;
            AStream  : TStream
    );
    procedure InternalSendBuffer(
            AMsgType : TLogMessageType;
      const AText    : string;
        var ABuffer;
            ACount   : LongWord
    );

    function CalledBy(const AName: string): Boolean;

    procedure Clear;

    // Send functions
    procedure Send(const AName: string; AArgs: array of const); overload;
    procedure Send(const AName: string; const AValue: string = ''); overload;
    procedure Send(const AName: string; AValue: Integer); overload;
    procedure Send(const AName: string; AValue: Cardinal); overload;
    procedure Send(const AName: string; AValue: Double); overload;
    procedure Send(const AName: string; AValue: Int64); overload;
    procedure Send(const AName: string; AValue: Boolean); overload;
    procedure Send(const AName: string; const AValue: TRect); overload;
    procedure Send(const AName: string; const AValue: TPoint); overload;
    procedure Send(const AName: string; AValue: TStrings); overload;
    procedure Send(const AName: string; AValue: TObject); overload;
    procedure Send(const AName: string; AValue: TDateTime); overload;
    procedure Send(const AName: string; AValue: TDate); overload;
    procedure Send(const AName: string; AValue: TTime); overload;
    procedure Send(const AName: string; AValue: TValue); overload;

    procedure SendPointer(const AName: string; APointer: Pointer);
    procedure SendException(const AName: string; AException: Exception);
    procedure SendMemory(const AName: string; AAddress: Pointer; ASize: LongWord);

    procedure SendIf(
      const AText       : string;
            AExpression : Boolean;
            AIsTrue     : Boolean = True
    );

    procedure SendWarning(const AText: string);
    procedure SendError(const AText: string);

    { Uses the OnCustomData event as callback. }
    procedure SendCustomData(
      const AName : string;
      const AData : TValue
    ); overload;
    procedure SendCustomData(
      const AName : string;
      const AData : TValue;
            AFunc : TCustomDataCallbackMethod
    ); overload;
    procedure SendCustomData(
      const AName : string;
      const AData : TValue;
            AFunc : TCustomDataCallbackFunction
    ); overload;

    procedure AddCheckPoint(const AName: string = '');
    procedure ResetCheckPoint(const AName: string = '');

    procedure IncCounter(const AName: string);
    procedure DecCounter(const AName: string);
    procedure ResetCounter(const AName: string);
    function GetCounter(const AName: string): Integer;

    procedure EnterMethod(const AName: string); overload;
    procedure EnterMethod(ASender: TObject; const AName: string); overload;
    procedure ExitMethod(const AName: string); overload;
    procedure ExitMethod(ASender: TObject; const AName: string); overload;

    procedure Watch(const AName: string; const AValue: string); overload;
    procedure Watch(const AName: string; AValue: Integer); overload;
    procedure Watch(const AName: string; AValue: Cardinal); overload;
    procedure Watch(const AName: string; AValue: Double); overload;
    procedure Watch(const AName: string; AValue: Boolean); overload;

    property Channels: TChannelList
      read GetChannels;

    property LogStack: TStrings
      read FLogStack;

    property MaxStackCount: Integer
      read FMaxStackCount write SetMaxStackCount;

    property OnCustomData: TCustomDataCallbackMethod
      read FOnCustomData write FOnCustomData;

  public
    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;
  end;

var
  Logger: ILogger;

//*****************************************************************************

implementation

uses
  Forms, Messages, TypInfo, StrUtils;

resourcestring
  SErrServerNotActive = 'Server with ID %s is not active.';

const
  DefaultCheckName = 'CheckPoint';
  {TODO -oTS -cGeneral : Rename this}
  MsgWndClassName : PChar = 'FPCMsgWindowCls';

  LogPrefixes: array [lmtInfo..lmtCounter] of string = (
    'INFO',
    'ERROR',
    'WARNING',
    'VALUE',
    '>>ENTER METHOD',
    '<<EXIT METHOD',
    'CONDITIONAL',
    'CHECKPOINT',
    'STRINGS',
    'CALL STACK',
    'OBJECT',
    'EXCEPTION',
    'BITMAP',
    'HEAP INFO',
    'MEMORY',
    '',
    '',
    '',
    '',
    '',
    'WATCH',
    'COUNTER'
  );

//=============================================================================

{$REGION 'TLogger'}
{$REGION 'construction and destruction'}
procedure TLogger.AfterConstruction;
begin
  inherited;
  FChannels := TChannelList.Create;
  FMaxStackCount := 20;
  FLogStack := TStringList.Create;
  FCheckList := TStringList.Create;
  with FCheckList do
  begin
    CaseSensitive := False;
    Sorted := True;
  end;
  FCounterList := TStringList.Create;
  with FCounterList do
  begin
    CaseSensitive := False;
    Sorted := True;
  end;
end;

procedure TLogger.BeforeDestruction;
begin
  FChannels.Free;
  FLogStack.Free;
  FCheckList.Free;
  FCounterList.Free;
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TLogger.SetMaxStackCount(const AValue: Integer);
begin
  if AValue < 256 then
    FMaxStackCount := AValue
  else
    FMaxStackCount := 256;
end;
{$ENDREGION}

{$REGION 'private methods'}
function TLogger.RectToStr(const ARect: TRect): string;
begin
  Result := Format('(Left: %d; Top: %d; Right: %d; Bottom: %d)',
    [ARect.Left, ARect.Top, ARect.Right, ARect.Bottom]);
end;

function TLogger.PointToStr(const APoint: TPoint): string;
begin
  Result := Format('(X: %d; Y: %d)', [APoint.X, APoint.Y]);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TLogger.InternalSendStream(AMsgType: TLogMessageType; const AText: string;
  AStream: TStream);
var
  LM : TLogMessage;
  C  : TCustomLogChannel;
begin
  with LM do
  begin
    MsgType := AMsgType;
    MsgTime := Now;
    MsgText := AText;
    Data    := AStream;
  end;
  for C in Channels do
    if C.Active then
      C.Write(LM);
  AStream.Free;
end;

procedure TLogger.InternalSend(AMsgType: TLogMessageType; const AText: string);
begin
  InternalSendStream(AMsgType, AText, nil);
end;

procedure TLogger.InternalSendBuffer(AMsgType: TLogMessageType; const AText: string;
  var ABuffer; ACount: LongWord);
var
  Stream: TStream;
begin
  if ACount > 0 then
  begin
    Stream := TMemoryStream.Create;
    Stream.Write(ABuffer, ACount);
  end
  else
    Stream := nil;
  // SendStream free Stream
  InternalSendStream(AMsgType, AText, Stream);
end;

function TLogger.CalledBy(const AName: string): Boolean;
begin
  Result := FLogStack.IndexOf(UpperCase(AName)) <> -1;
end;

procedure TLogger.Clear;
var
  C: TCustomLogChannel;
begin
  for C in Channels do
    if C.Active then
       C.Clear;
end;

procedure TLogger.Send(const AName: string; AArgs: array of const);
begin
  InternalSend(lmtInfo, Format(AName, AArgs));
end;

procedure TLogger.Send(const AName: string; const AValue: string);
begin
  InternalSend(lmtValue, AName + ' = ' + AValue);
end;

procedure TLogger.Send(const AName: string; AValue: Integer);
begin
  InternalSend(lmtValue, AName + ' = ' + IntToStr(AValue));
end;

procedure TLogger.Send(const AName: string; AValue: Cardinal);
begin
  InternalSend(lmtValue, AName + ' = ' + IntToStr(AValue));
end;

procedure TLogger.Send(const AName: string; AValue: Double);
begin
  InternalSend(lmtValue, AName + ' = ' + FloatToStr(AValue));
end;

procedure TLogger.Send(const AName: string; AValue: Int64);
begin
  InternalSend(lmtValue, AName + ' = ' + IntToStr(AValue));
end;

procedure TLogger.Send(const AName: string; AValue: Boolean);
begin
  InternalSend(lmtValue, AName + ' = ' + BoolToStr(AValue, True));
end;

procedure TLogger.Send(const AName: string; const AValue: TRect);
begin
  InternalSend(lmtValue, AName + ' = ' + RectToStr(AValue));
end;

procedure TLogger.Send(const AName: string; const AValue: TPoint);
begin
  InternalSend(lmtValue, AName + ' = ' + PointToStr(AValue));
end;

procedure TLogger.Send(const AName: string; AValue: TStrings);
var
  S : string;
begin
  if Assigned(AValue) then
    S := AValue.Text
  else
    S := '';
  InternalSendBuffer(lmtStrings, AName, S[1], Length(S));
end;

procedure TLogger.Send(const AName: string; AValue: TObject);
var
  S      : string;
  Stream : TStream;
begin
  Stream := nil;
  S := AName + ' (';
  if AValue <> nil then
  begin
    if AValue is TComponent then
    begin
      S := S + ('"' + TComponent(AValue).Name + '"/');
      Stream := TMemoryStream.Create;
      Stream.WriteComponent(TComponent(AValue));
    end;
    S := S + (AValue.ClassName + '/');
  end;
  S := S + ('$' + IntToHex(Integer(AValue),
      SizeOf(Integer) * 2) + ')');
  InternalSendStream(lmtObject, S, Stream);
end;

procedure TLogger.Send(const AName: string; AValue: TDateTime);
begin
  Send(AName, DateTimeToStr(AValue));
end;

procedure TLogger.Send(const AName: string; AValue: TDate);
begin
  Send(AName, DateToStr(AValue));
end;

procedure TLogger.Send(const AName: string; AValue: TTime);
begin
  Send(AName, TimeToStr(AValue));
end;

procedure TLogger.Send(const AName: string; AValue: TValue);
begin
  case AValue.Kind of
    tkInteger: Send(AName, AValue.AsInteger);
    tkEnumeration:
    begin
      if AValue.TypeInfo = TypeInfo(Boolean) then
      begin
        Send(AName, AValue.AsBoolean);
      end;
    end;
//    tkFloat:
//    begin
//      if AValue.TypeInfo.Name = 'TDate' then
//      begin
//        Send(AName, AValue.AsType<TDate>);
//      end
//      else
//      if AValue.TypeInfo.Name = 'TDateTime' then
//      begin
//        Send(AName, AValue.AsType<TDateTime>);
//      end
//      else
//      if AValue.TypeInfo = TypeInfo(TTime) then
//      begin
//        Send(AName, AValue.AsType<TTime>);
//      end
//      else
//      begin
//        Send(AName, AValue.AsExtended);
//      end;
//    end;
    tkClass: Send(AName, AValue.AsObject);
    //tkVariant: Send(AName, AValue.AsVariant);
    tkInt64: Send(AName, AValue.AsInt64);
  else
    Send(AName, AValue.ToString);
  end;
end;

procedure TLogger.SendPointer(const AName: string; APointer: Pointer);
begin
  InternalSend(lmtValue, AName + ' = ' + IntToHex(Integer(APointer), 8));
end;

procedure TLogger.SendException(const AName: string; AException: Exception);
var
  S: string;
begin
  if AException <> nil then
    S := AException.ClassName + ' - ' + AException.Message + #13#10;
  InternalSendBuffer(lmtException, AName, S[1], Length(S));
end;

procedure TLogger.SendMemory(const AName: string; AAddress: Pointer;
  ASize: LongWord);
begin
  InternalSendBuffer(lmtMemory, AName, AAddress^, ASize);
end;

procedure TLogger.SendIf(const AText: string; AExpression, AIsTrue: Boolean);
begin
  if AExpression <> AIsTrue then
    Exit;
  InternalSend(lmtConditional, AText);
end;

procedure TLogger.SendWarning(const AText: string);
begin
  InternalSend(lmtWarning, AText);
end;

procedure TLogger.SendError(const AText: string);
begin
  InternalSend(lmtError, AText);
end;

procedure TLogger.SendCustomData(const AName: string; const AData: TValue);
begin
  SendCustomData(AName, AData, FOnCustomData);
end;

procedure TLogger.SendCustomData(const AName: string; const AData: TValue;
  AFunc: TCustomDataCallbackMethod);
var
  B : Boolean;
  S : string;
begin
  if not Assigned(AFunc) then
    Exit;
  B := True;
  S := AFunc(Self, AData, B);
  if B then
    InternalSendBuffer(lmtCustomData, AName, S[1], Length(S));
end;

procedure TLogger.SendCustomData(const AName: string; const AData: TValue;
  AFunc: TCustomDataCallbackFunction);
var
  B : Boolean;
  S : string;
begin
  if not Assigned(AFunc) then
    Exit;
  B := True;
  S := AFunc(Self, AData, B);
  if B then
    InternalSendBuffer(lmtCustomData, AName, S[1], Length(S));
end;

procedure TLogger.AddCheckPoint(const AName: string);
var
  I : Integer;
  J : Integer;
begin
  I := FCheckList.IndexOf(AName);
  if I <> -1 then
  begin
    // Add a custom CheckList
    J := Integer(FCheckList.Objects[I]) + 1;
    FCheckList.Objects[I] := TObject(J);
  end
  else
  begin
    FCheckList.AddObject(AName, TObject(0));
    J := 0;
  end;
  InternalSend(lmtCheckpoint, AName + ' #' + IntToStr(J));
end;

procedure TLogger.IncCounter(const AName: string);
var
  I : Integer;
  J : Integer;
begin
  I := FCounterList.IndexOf(AName);
  if I <> -1 then
  begin
    J := Integer(FCounterList.Objects[I]) + 1;
    FCounterList.Objects[I] := TObject(J);
  end
  else
  begin
    FCounterList.AddObject(AName, TObject(1));
    J := 1;
  end;
  InternalSend(lmtCounter, AName + '=' + IntToStr(J));
end;

procedure TLogger.DecCounter(const AName: string);
var
  I : Integer;
  J : Integer;
begin
  I := FCounterList.IndexOf(AName);
  if I <> -1 then
  begin
    J := Integer(FCounterList.Objects[I]) - 1;
    FCounterList.Objects[I] := TObject(J);
  end
  else
  begin
    FCounterList.AddObject(AName, TObject(-1));
    J := -1;
  end;
  InternalSend(lmtCounter, AName + '=' + IntToStr(J));
end;

procedure TLogger.ResetCounter(const AName: string);
var
  I : Integer;
begin
  I := FCounterList.IndexOf(AName);
  if I <> -1 then
  begin
    FCounterList.Objects[I] := TObject(0);
    InternalSend(lmtCounter, FCounterList[I] + '=0');
  end;
end;

function TLogger.GetChannels: TChannelList;
begin
  Result := FChannels;
end;

function TLogger.GetCounter(const AName: string): Integer;
var
  I: Integer;
begin
  I := FCounterList.IndexOf(AName);
  if I <> -1 then
    Result := Integer(FCounterList.Objects[I])
  else
    Result := 0;
end;

procedure TLogger.ResetCheckPoint(const AName: string);
var
  I: Integer;
  S: string;
begin
  if AName = '' then
    S := DefaultCheckName
  else
    S := AName;
  I := FCheckList.IndexOf(AName);
  if I <> -1 then
  begin
    FCheckList.Objects[I] := TObject(0);
    InternalSend(lmtCheckpoint, S + ' #0');
  end;
end;

procedure TLogger.EnterMethod(const AName: string);
begin
  EnterMethod(nil, AName);
end;

procedure TLogger.EnterMethod(ASender: TObject; const AName: string);
begin
  FLogStack.Insert(0, UpperCase(AName));
  if ASender <> nil then
  begin
    if ASender is TComponent then
      InternalSend(lmtEnterMethod, TComponent(ASender).Name + '.' + AName)
    else
      InternalSend(lmtEnterMethod, ASender.ClassName + '.' + AName);
  end
  else
    InternalSend(lmtEnterMethod, AName);
end;

procedure TLogger.ExitMethod(const AName: string);
begin
  ExitMethod(nil, AName);
end;

procedure TLogger.ExitMethod(ASender: TObject; const AName: string);
var
  I: Integer;
begin
  // ensure that ExitMethod will be called allways if there's a unpaired Entermethod
  // even if Classes is not Active
  if FLogStack.Count = 0 then
    Exit;
  // todo: see if is necessary to do Uppercase (set case sensitive to false?)
  I := FLogStack.IndexOf(UpperCase(AName));
  if I <> -1 then
    FLogStack.Delete(I)
  else
    Exit;
  if ASender <> nil then
  begin
    if ASender is TComponent then
      InternalSend(lmtExitMethod, TComponent(ASender).Name + '.' + AName)
    else
      InternalSend(lmtExitMethod, ASender.ClassName + '.' + AName);
  end
  else
    InternalSend(lmtExitMethod, AName);
end;

procedure TLogger.Watch(const AName, AValue: string);
begin
  InternalSend(lmtWatch, AName + '=' + AValue);
end;

procedure TLogger.Watch(const AName: string; AValue: Integer);
begin
  InternalSend(lmtWatch, AName + '=' + IntToStr(AValue));
end;

procedure TLogger.Watch(const AName: string; AValue: Cardinal);
begin
  InternalSend(lmtWatch, AName + '=' + IntToStr(AValue));
end;

procedure TLogger.Watch(const AName: string; AValue: Double);
begin
  InternalSend(lmtWatch, AName + '=' + FloatToStr(AValue));
end;

procedure TLogger.Watch(const AName: string; AValue: Boolean);
begin
  InternalSend(lmtWatch, AName + '=' + BoolToStr(AValue));
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TWinIPCClient'}
{$REGION 'property access methods'}
procedure TWinIPCClient.SetActive(const AValue: Boolean);
begin
  if AValue <> Active then
  begin
    FActive := AValue;
    if FActive then
      Connect
    else
      Disconnect;
  end;
end;

procedure TWinIPCClient.SetServerID(const AValue: string);
begin
  FServerID := AValue;
  UpdateWindowName;
end;

procedure TWinIPCClient.SetServerInstance(const AValue: string);
begin
  FWindowName := AValue;
  UpdateWindowName;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TWinIPCClient.UpdateWindowName;
begin
  if FServerInstance <> '' then
    FWindowName := FServerID + '_' + FServerInstance
  else
    FWindowName := FServerID;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TWinIPCClient.Connect;
begin
  FHWND := FindWindow(MsgWndClassName, PChar(FWindowName));
  if FHWND = 0 then
    raise Exception.Create(Format(SErrServerNotActive, [FServerID]));
end;

procedure TWinIPCClient.Disconnect;
begin
  FHWND := 0;
end;

function TWinIPCClient.ServerRunning: Boolean;
begin
  Result := FindWindow(MsgWndClassName, PChar(FWindowName)) <> 0;
end;

procedure TWinIPCClient.SendStream(AStream: TStream);
var
  CDS     : TCopyDataStruct;
  Data    : TMemorySTream;
  FMemstr : TMemorySTream;
begin
  if AStream is TMemoryStream then
  begin
    Data := TMemoryStream(AStream);
    FMemStr := nil
  end
  else
  begin
    FMemStr := TMemoryStream.Create;
    Data := FMemstr;
  end;
  try
    if Assigned(FMemStr) then
    begin
      FMemStr.CopyFrom(AStream,0);
      FMemStr.Seek(0,soFromBeginning);
    end;
    CDS.lpData := Data.Memory;
    CDS.cbData := Data.Size;
    Windows.SendMessage(FHWnd, WM_COPYDATA, 0, Integer(@CDS));
  finally
    FreeAndNil(FMemStr);
  end;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TIPCChannel'}
{$REGION 'construction and destruction'}
procedure TIPCChannel.AfterConstruction;
begin
  inherited;
  with FClearMessage do
  begin
    MsgType := lmtClear;
    MsgText:='';
    MsgTime:=Now;
    Data:=nil;
  end;
  FBuffer := TMemoryStream.Create;
  FClient := TWinIPCClient.Create(nil);
  with FClient do
  begin
    ServerID := 'ipc_log_server';
    // todo: Start server only when channel is active
    if ServerRunning then
    begin
      Active := True;
    end
    else
      Active := False;
  end;
end;

procedure TIPCChannel.BeforeDestruction;
begin
  FClient.Free;
  FBuffer.Free;
  inherited;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TIPCChannel.Clear;
begin
  Write(FClearMessage);
end;

procedure TIPCChannel.Write(const AMsg: TLogMessage);
const
  ZeroBuf: Integer = 0;
var
  TextSize : Integer;
  DataSize : Integer;
begin
  with FBuffer do
  begin
    TextSize := Length(AMsg.MsgText);
    Seek(0, soFromBeginning);
    WriteBuffer(AMsg.MsgType, SizeOf(Integer));
    WriteBuffer(AMsg.MsgTime, SizeOf(TDateTime));
    WriteBuffer(TextSize, SizeOf(Integer));
    WriteBuffer(AMsg.MsgText[1], TextSize);
    if AMsg.Data <> nil then
    begin
      DataSize := AMsg.Data.Size;
      // WriteLn('[IPCChannel] Size Of Stream: ',DataSize);
      WriteBuffer(DataSize, SizeOf(Integer));
      AMsg.Data.Position := 0;
      CopyFrom(AMsg.Data, DataSize);
      // WriteLn('DataCopied: ',CopyFrom(AMsg.Data,DataSize));
    end
    else
      WriteBuffer(ZeroBuf, SizeOf(Integer)); // necessary?
  end;
  FClient.SendStream(FBuffer);
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TFileChannel'}
{$REGION 'construction and destruction'}
constructor TFileChannel.Create(const AFileName: String);
begin
  FShowPrefix   := True;
  FShowTime     := True;
  FShowStrings  := True;
  Active        := True;
  FStreamWriter := TStreamWriter.Create(AFileName, True);
  inherited Create;
end;

procedure TFileChannel.BeforeDestruction;
begin
  FStreamWriter.Free;
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TFileChannel.SetShowTime(const AValue: Boolean);
begin
  FShowTime := AValue;
  UpdateIdentation;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TFileChannel.UpdateIdentation;
var
  S : string;
begin
  S := '';
  if FShowTime then
    S := FormatDateTime('hh:nn:ss:zzz', Time);
  FBaseIdent := Length(S) + 3;
end;

function TFileChannel.Space(ACount: Integer): string;
begin
  Result := DupeString(' ', ACount);
end;

procedure TFileChannel.WriteStrings(AStream: TStream);
var
  I: Integer;
begin
  if AStream.Size = 0 then
    Exit;
  with TStringList.Create do
    try
      AStream.Position := 0;
      LoadFromStream(AStream);
      for I := 0 to Count - 1 do
        FStreamWriter.WriteLine(Space(FRelativeIdent + FBaseIdent) + Strings[I]);
    finally
      Free;
    end;
end;

procedure TFileChannel.WriteComponent(AStream: TStream);
begin
  AStream.Seek(0, soFromBeginning);
  ObjectBinaryToText(AStream, FStreamWriter.BaseStream);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TFileChannel.Clear;
begin
  FStreamWriter.BaseStream.Position := 0;
end;

procedure TFileChannel.Write(const AMsg: TLogMessage);
begin
  // Exit method identation must be set before
  if (AMsg.MsgType = lmtExitMethod) and (FRelativeIdent >= 2) then
    Dec(FRelativeIdent, 2);
  if FShowTime then
    FStreamWriter.Write(FormatDateTime('hh:nn:ss:zzz', AMsg.MsgTime) + ' ');
  FStreamWriter.Write(Space(FRelativeIdent));
  if FShowPrefix then
    FStreamWriter.Write(LogPrefixes[AMsg.MsgType] + ': ');
  FStreamWriter.WriteLine(AMsg.MsgText);
  if FShowStrings and (AMsg.Data <> nil) then
  begin
    case AMsg.MsgType of
      lmtStrings, lmtCallStack, lmtHeapInfo, lmtException:
         WriteStrings(AMsg.Data);
      lmtObject:
        WriteComponent(AMsg.Data);
    end;
  end;
  // Update enter method identation
  if AMsg.MsgType = lmtEnterMethod then
    Inc(FRelativeIdent, 2);
end;

procedure TFileChannel.Open;
begin
  if FShowHeader then
    FStreamWriter.WriteLine('=== Log Session Started at ' + DateTimeToStr(Now)
        + ' by ' + Application.name + ' ===');
  UpdateIdentation;
end;
{$ENDREGION}
{$ENDREGION}

initialization
  Logger := TLogger.Create;

end.
