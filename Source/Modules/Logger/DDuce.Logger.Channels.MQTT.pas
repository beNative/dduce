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

{$I DDuce.inc}

unit DDuce.Logger.Channels.MQTT;

interface

{ Channel holding a MQTT publisher socket where one or more Logviewers can
  subscribe to.

  The channel connects to a MQTT broker (like Mosquitto)
}

uses
  System.Classes, System.SysUtils,

  MQTT,

  DDuce.Logger.Interfaces, DDuce.Logger.Channels.Base;

type
  TMQTTChannel = class(TCustomLogChannel, ILogChannel, IMQTTChannel)
  private
    FBuffer    : TStringStream;
    FMQTT      : TMQTT;
    //FPublisher : IZMQPair;
    FPort      : Integer;
    //FEndPoint  : string;
    //FBound     : Boolean;

  protected
    function GetPort: Integer; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    constructor Create(
      const ABrokerHost : string = '';
      APort             : Integer = 1833;
      AActive           : Boolean = True
    ); reintroduce; overload; virtual;

    function Connect: Boolean; override;
    function Disconnect: Boolean; override;

    function Write(const AMsg: TLogMessage): Boolean; override;

  end;

implementation

uses
  Spring, Spring.Helpers, Spring.SystemUtils;

{$REGION 'construction and destruction'}
procedure TMQTTChannel.AfterConstruction;
begin
  inherited AfterConstruction;
  if Enabled then
  begin
    Connect;
  end;
end;

procedure TMQTTChannel.BeforeDestruction;
begin
  FMQTT.Disconnect;
  FMQTT.Free;
  inherited BeforeDestruction;
end;

constructor TMQTTChannel.Create(const ABrokerHost: string; APort: Integer;
  AActive: Boolean);
begin
  inherited Create(AActive);
  FPort := APort;
  FMQTT := TMQTT.Create(UTF8String(ABrokerHost), APort);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TMQTTChannel.GetPort: Integer;
begin
  Result := FPort;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TMQTTChannel.Connect: Boolean;
begin
  Result := FMQTT.Connect;
end;

function TMQTTChannel.Disconnect: Boolean;
begin
  Result := FMQTT.Disconnect;
end;

function TMQTTChannel.Write(const AMsg: TLogMessage): Boolean;
const
  ZeroBuf: Integer = 0;
var
  TextSize : Integer;
  DataSize : Integer;
begin
  if Enabled then
  begin
    if not Connected and AutoConnect then
      Connect;
    if Connected then
    begin
      TextSize := Length(AMsg.Text);
      FBuffer.Seek(0, soFromBeginning);
      FBuffer.WriteBuffer(AMsg.MsgType);
      FBuffer.WriteBuffer(AMsg.LogLevel);
      FBuffer.WriteBuffer(AMsg.Reserved1);
      FBuffer.WriteBuffer(AMsg.Reserved2);
      FBuffer.WriteBuffer(AMsg.TimeStamp);
      FBuffer.WriteBuffer(TextSize);
      if TextSize > 0 then
      begin
        FBuffer.WriteBuffer(AMsg.Text[1], TextSize);
      end;
      if AMsg.Data <> nil then
      begin
        DataSize := AMsg.Data.Size;
        FBuffer.WriteBuffer(DataSize);
        AMsg.Data.Position := 0;
        FBuffer.CopyFrom(AMsg.Data, DataSize);
      end
      else
        FBuffer.WriteBuffer(ZeroBuf);
      Result := FMQTT.Publish('Test', UTF8Encode(FBuffer.DataString));
    end
    else
    begin
      Result := False;
    end;
  end
  else
  begin
    Result := False;
  end;
end;
{$ENDREGION}

end.
