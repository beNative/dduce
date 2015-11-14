{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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

{ IPC appender for the Spring4D logging system. }

unit DDuce.Logging.Appenders.WinIPC;

interface

uses
  Spring.Logging.Appenders.Base, Spring.Logging;

type
  TWinIPCAppender = class(TLogAppenderBase, ILogAppender)

  protected
    procedure DoSend(const entry: TLogEntry); override;
  end;

implementation

{ TWinIPCAppender }

procedure TWinIPCAppender.DoSend(const entry: TLogEntry);
begin
  inherited;

end;

end.
