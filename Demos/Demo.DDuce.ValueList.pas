{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Demo.DDuce.ValueList;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.TypInfo,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  zObjInspector,

  DDuce.DynamicRecord, DDuce.Components.ValueList;

type
  TfrmValueListDemo = class(TForm)
    {$REGION 'designer controls'}
    pnlLeft     : TPanel;
    pnlRight    : TPanel;
    splVertical : TSplitter;
    {$ENDREGION}

  private
    FValueList       : TValueList;
    FObjectInspector : TzObjectInspector;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Logger, DDuce.Logger.Channels.WinIPC, DDuce.Factories.zObjInspector;

{$REGION 'construction and destruction'}
procedure TfrmValueListDemo.AfterConstruction;
begin
  inherited AfterConstruction;
  Logger.Channels.Add(TWinIPCChannel.Create);
  FValueList        := TValueList.Create(Self);
  FValueList.Parent := pnlRight;
  FValueList.Align  := alClient;
  FValueList.Data   := DynamicRecord.Create(Application);
  FObjectInspector  := TzObjectInspectorFactory.Create(
    Self,
    pnlLeft,
    FValueList
  );
  FObjectInspector.ObjectVisibility := mvPublished;
end;
{$ENDREGION}

end.
