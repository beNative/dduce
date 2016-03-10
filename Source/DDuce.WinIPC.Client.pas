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

unit DDuce.WinIPC.Client;

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils;

  { IPC using WM_COPYDATA messages. }
type
  TWinIPCClient = class
  strict private
    FActive         : Boolean;
    FServerHandle   : THandle;

    procedure SetActive(const AValue: Boolean);

  public
    procedure Connect;
    procedure Disconnect;
    function  ServerRunning: Boolean;
    procedure SendStream(AStream: TStream);

    property Active: Boolean
      read FActive write SetActive;
  end;

implementation

uses
  WinApi.Messages,
  Vcl.Forms;

const
// old name maintained for backwards compatibility
  MSG_WND_CLASSNAME : PChar = 'FPCMsgWindowCls';
  SERVER_WINDOWNAME : PChar = 'ipc_log_server';

resourcestring
  SServerNotActive = 'Server with ID %s is not active.';

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
{$ENDREGION}

{$REGION 'public methods'}
procedure TWinIPCClient.Connect;
begin
  FServerHandle := FindWindow(MSG_WND_CLASSNAME, SERVER_WINDOWNAME);
end;

procedure TWinIPCClient.Disconnect;
begin
  FServerHandle := 0;
end;

function TWinIPCClient.ServerRunning: Boolean;
begin
  Result := FindWindow(MSG_WND_CLASSNAME, SERVER_WINDOWNAME) <> 0;
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
    try
      Data := MS;
      MS.CopyFrom(AStream, 0);
      MS.Seek(0, soFromBeginning);
    finally
      FreeAndNil(MS);
    end;
  end;
  CDS.lpData := Data.Memory;
  CDS.cbData := Data.Size;
  Winapi.Windows.SendMessage(
    FServerHandle,
    WM_COPYDATA,
    0,
    Integer(@CDS)
  );
end;
{$ENDREGION}

end.
