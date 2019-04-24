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

unit Mosquitto;

interface

{
  cint = Integer

}
uses
  Mosquitto.API;

type
  TMosquitto = class(TInterfacedObject)
  private
     FMosq : Pmosquitto;

  public

    class constructor Create;
    class destructor Destroy;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;



    function Connect(
      const AHost : string;
      APort       : Integer = 1883;
      AKeepAlive  : Integer = 2
    ): Boolean;


  end;


implementation

uses
  System.SysUtils, Vcl.Dialogs;

{ TMosquitto }

{$REGION 'construction and destruction'}
class constructor TMosquitto.Create;
begin
//  if mosquitto_lib_init <> MOSQ_ERR_SUCCESS then
//  begin
//    raise Exception.Create('Mosquitto could not initialize!');
//  end;
end;

class destructor TMosquitto.Destroy;
begin
  //mosquitto_lib_cleanup;
end;

procedure TMosquitto.AfterConstruction;
begin
  inherited AfterConstruction;
  mosquitto_lib_init;
  FMosq := mosquitto_new(nil, True, Self);

end;

procedure TMosquitto.BeforeDestruction;
begin
  mosquitto_destroy(FMosq);
  mosquitto_lib_cleanup;
  inherited BeforeDestruction;
end;
{$ENDREGION}



function TMosquitto.Connect(const AHost: string; APort,
  AKeepAlive: Integer): Boolean;
var
  LErrorCode: Integer;
begin
  //mosquitto
  LErrorCode := mosquitto_connect(FMosq, 'localhost', APort, AKeepAlive);
  if LErrorCode <> MOSQ_ERR_SUCCESS then
  begin
    raise Exception.Create(PAnsiChar(mosquitto_strerror(LErrorCode)));
  end;
  Result := LErrorCode = MOSQ_ERR_SUCCESS;
end;


end.
