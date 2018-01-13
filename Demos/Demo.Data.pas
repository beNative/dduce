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

unit Demo.Data;

interface

uses
  System.Classes, System.ImageList,
  Vcl.ImgList, Vcl.Controls,
  Data.DB, Datasnap.DBClient;

type
  TdmData = class(TDataModule)
    imlMain : TImageList;

  private
    function GetImageList: TImageList;

  public
    property ImageList: TImageList
      read GetImageList;
  end;

function Data: TdmData;

implementation

{$R *.dfm}

uses
  Vcl.Forms;

var
  FData: TdmData;

{$REGION 'interfaced routines'}
function Data: TdmData;
begin
  if not Assigned(FData) then
    FData := TdmData.Create(Application);
  Result := FData;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TdmData.GetImageList: TImageList;
begin
  Result := imlMain;
end;
{$ENDREGION}

end.



