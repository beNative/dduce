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

{  }

unit DDuce.Logging.Appenders.LogTree;

interface

uses
  Spring.Logging.Appenders.Base, Spring.Logging,

  DDuce.Components.LogTree;

type
  TLogTreeAppender = class(TLogAppenderBase, ILogAppender)
  private
    FLogTree : TLogTree;

  protected

    procedure DoSend(const entry: TLogEntry); override;

  public
    constructor Create(ALogTree: TLogTree);
  end;

implementation

uses
  Spring;

{ TLogTreeAppender }

constructor TLogTreeAppender.Create(ALogTree: TLogTree);
begin
  Guard.CheckNotNull(ALogTree, 'ALogTree');
  FLogTree := ALogTree;
end;

procedure TLogTreeAppender.DoSend(const entry: TLogEntry);
begin
  //FLogTree.



end;

end.
