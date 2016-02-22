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

unit DDuce.Editor.Search.Engine.Settings;

interface

uses
  System.Classes,
  Vcl.Forms, Vcl.Controls,

  BCEditor.Editor.Search, BCEditor.Types;

type
  TSearchEngineSettings = class(TComponent)
  private
    FOptions        : TBCEditorSearchOptions;
    FSearchAllViews : Boolean;

  public
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

  published
    property Options : TBCEditorSearchOptions
      read FOptions write FOptions;

    property SearchAllViews: Boolean
      read FSearchAllViews write FSearchAllViews default False;



  end;

implementation

{$REGION 'public methods'}
procedure TSearchEngineSettings.AssignTo(Dest: TPersistent);
var
  SES: TSearchEngineSettings;
begin
  if Dest is TSearchEngineSettings then
  begin
    SES := TSearchEngineSettings(Dest);
    SES.Options        := Options;
    SES.SearchAllViews := SearchAllViews;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TSearchEngineSettings.Assign(Source: TPersistent);
var
  SES: TSearchEngineSettings;
begin
  if Source is TSearchEngineSettings then
  begin
    SES := TSearchEngineSettings(Source);
    Options        := SES.Options;
    SearchAllViews := SES.SearchAllViews;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

initialization
  RegisterClass(TSearchEngineSettings);

end.


