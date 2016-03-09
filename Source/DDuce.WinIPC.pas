{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.WinIPC;

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils;

type
  TMessageType = Integer;

  TWinIPCServer = class(TComponent)
  private
    FMsgWindowClass : TWndClass;
    FGlobal         : Boolean;
    FOnMessage      : TNotifyEvent;
    FMsgType        : TMessageType;
    FMsgData        : TStream;
    FWindowName     : string;
    FBusy           : Boolean;
    FActive         : Boolean;
    FServerID       : string;
    FDataPushed     : Boolean;
    FHWND           : HWND;

    procedure SetActive(const AValue : Boolean);
    procedure SetServerID(const AValue : string);
    function GetInstanceID : string;
    procedure SetGlobal(const AValue : Boolean);
    function GetBusy: Boolean;

    procedure CheckInactive;
    procedure CheckActive;
    procedure DoMessage;

  protected
    procedure ReadMessage;
    function AllocateHWnd(const AWindowName : string) : HWND;
    procedure ReadMsgData(var Msg : TMsg);
    procedure StartServer;
    procedure StopServer;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Active : Boolean
      read FActive write SetActive;

    property Busy: Boolean
      read GetBusy;

    property ServerID : string
      read FServerID write SetServerID;

    property MsgType : TMessageType
      read FMsgType write FMsgType;

    property MsgData : TStream
      read FMsgData;

    property InstanceID : string
      read GetInstanceID;

    property Global : Boolean
      read FGlobal write SetGlobal;

    property OnMessage : TNotifyEvent
      read FOnMessage write FOnMessage;
  end;

  { TWinIPCClient }

  { IPC using WM_COPYDATA messages. }
type
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

implementation

uses
  WinApi.Messages,
  Vcl.Forms;

const
// old name maintained for backwards compatibility
  MSG_WND_CLASSNAME : PChar = 'FPCMsgWindowCls';

resourcestring
  SServerNotActive = 'Server with ID %s is not active.';
  SActive          = 'This operation is illegal when the server is active.';
  SInActive        = 'This operation is illegal when the server is inactive.';
  SFailedToRegisterWindowClass = 'Failed to register message window class';
  SFailedToCreateWindow        = 'Failed to create message window %s';

{$REGION 'TWinIPCServer'}
{$REGION 'non-interfaced routines'}
function MsgWndProc(HWindow: HWND; AMessage, WParam, LParam: LongInt): LongInt;
  stdcall;
var
  WIS : TWinIPCServer;
  Msg : TMsg;
begin
  Result := 0;
  if AMessage = WM_COPYDATA then
  begin
    WIS := TWinIPCServer(GetWindowLongPtr(HWindow, GWL_USERDATA));
    if Assigned(WIS) then
    begin
      Msg.Message := AMessage;
      Msg.WParam  := WParam;
      Msg.LParam  := LParam;
      WIS.ReadMsgData(Msg);
      WIS.FDataPushed := True;
      if Assigned(WIS.OnMessage) then
        WIS.ReadMessage;
      Result := 1;
    end
  end
  else
    Result := DefWindowProc(HWindow, AMessage, WParam, LParam);
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TWinIPCServer.AfterConstruction;
begin
  inherited AfterConstruction;
  FGlobal     := False;
  FActive     := False;
  FBusy       := False;
  FMsgData    := TStringStream.Create('');
//  FWindowName := ServerID + '_' + InstanceID;
end;

procedure TWinIPCServer.BeforeDestruction;
begin
  Active := False;
  FreeAndNil(FMsgData);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TWinIPCServer.SetActive(const AValue : Boolean);
begin
  if Active <> AValue then
  begin
    if csLoading in ComponentState then
      FActive := AValue
    else if AValue then
      StartServer
    else
      StopServer;
  end;
end;

procedure TWinIPCServer.SetServerID(const AValue : string);
begin
  if ServerID <> AValue then
  begin
    CheckInactive;
    FServerID := AValue
  end;
end;

procedure TWinIPCServer.SetGlobal(const AValue : Boolean);
begin
  if FGlobal <> AValue then
  begin
    CheckInactive;
    FGlobal := AValue;
  end;
end;

function TWinIPCServer.GetBusy: Boolean;
begin
  Result := FBusy;
end;

function TWinIPCServer.GetInstanceID : string;
begin
  Result := IntToStr(HInstance);
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TWinIPCServer.DoMessage;
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TWinIPCServer.CheckInactive;
begin
  if not (csLoading in ComponentState) and Active then
    raise Exception.Create(SActive);
end;

procedure TWinIPCServer.CheckActive;
begin
  if not (csLoading in ComponentState) and not Active then
    raise Exception.Create(SInActive);
end;

function TWinIPCServer.AllocateHWnd(const AWindowName: string): HWND;
var
  WC : TWndClass;
begin
  Pointer(FMsgWindowClass.lpfnWndProc) := @MsgWndProc;
  FMsgWindowClass.hInstance            := HInstance;
  FMsgWindowClass.lpszClassName        := MSG_WND_CLASSNAME;
  if not GetClassInfo(hInstance, MSG_WND_CLASSNAME, WC)
    and (Winapi.Windows.RegisterClass(FMsgWindowClass) = 0) then
      raise Exception.Create(SFailedToRegisterWindowClass);
  Result := CreateWindowEx(
    WS_EX_TOOLWINDOW,
    MSG_WND_CLASSNAME,
    PChar(AWindowName),
    WS_POPUP,
    0,
    0,
    0,
    0,
    0,
    0,
    HInstance,
    nil
  );
  if Result <> 0 then
    SetWindowLongPtr(Result, GWL_USERDATA, NativeInt(Self))
  else
    raise Exception.CreateFmt(SFailedToCreateWindow, [AWindowName])
end;

procedure TWinIPCServer.StartServer;
begin
  if FServerID = '' then
    FServerID := Application.Name;
  FWindowName := ServerID;
//  if not Global then
//    FWindowName := FWindowName + '_' + IntToStr(HInstance);
  FHWND         := AllocateHWnd(FWindowName);
  FActive := True;
end;

procedure TWinIPCServer.StopServer;
begin
  DestroyWindow(FHWND);
  FHWND := 0;
  FActive := False;
end;

procedure TWinIPCServer.ReadMessage;
var
  Msg : TMsg;
begin
  CheckActive;
  FBusy := True;
  try
    if FDataPushed then
      FDataPushed := False
    else if Winapi.Windows.PeekMessage(Msg, FHWND, 0, 0, PM_REMOVE)
      and (Msg.Message = WM_COPYDATA) then
    begin
      ReadMsgData(Msg);
    end;
    DoMessage;
  finally
    FBusy := False;
  end;
end;

procedure TWinIPCServer.ReadMsgData(var Msg: TMsg);
var
  CDS : PCopyDataStruct;
begin
  CDS := PCopyDataStruct(Msg.LParam);
  FMsgType      := CDS^.dwData;
  FMsgData.Size := 0;
  FMsgData.Seek(0, soFrombeginning);
  FMsgData.WriteBuffer(CDS^.lpData^, CDS^.cbData);
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
  if AValue <> ServerID then
  begin
    FServerID := AValue;
    UpdateWindowName;
  end;
end;

procedure TWinIPCClient.SetServerInstance(const AValue: string);
begin
  //if AValue <> ServerInstance then
  begin
    FWindowName := AValue;
    UpdateWindowName;
  end;
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
  FHWND := FindWindow(MSG_WND_CLASSNAME, PChar(FWindowName));
  if FHWND = 0 then
    raise Exception.Create(Format(SServerNotActive, [FServerID]));
end;

procedure TWinIPCClient.Disconnect;
begin
  FHWND := 0;
end;

function TWinIPCClient.ServerRunning: Boolean;
begin
  Result := FindWindow(MSG_WND_CLASSNAME, PChar(FWindowName)) <> 0;
end;

procedure TWinIPCClient.SendStream(AStream: TStream);
var
  CDS  : TCopyDataStruct;
  Data : TMemoryStream;
  MS   : TMemoryStream;
begin
  if AStream is TMemoryStream then
  begin
    Data := TMemoryStream(AStream);
    MS   := nil
  end
  else
  begin
    MS := TMemoryStream.Create;
    Data := MS;
  end;
  try
    if Assigned(MS) then
    begin
      MS.CopyFrom(AStream, 0);
      MS.Seek(0, soFromBeginning);
    end;
    Data.Seek(0, soFromBeginning);
    CDS.lpData := Data.Memory;
    CDS.cbData := Data.Size;
//  if FHWND = 0 then
//    FHWND := FindWindow(MSG_WND_CLASSNAME, PChar('ipc_log_server'));
    Winapi.Windows.SendMessage(FHWnd, WM_COPYDATA, 0, Integer(@CDS));
  finally
    FreeAndNil(MS);
  end;
end;
{$ENDREGION}
{$ENDREGION}

end.
