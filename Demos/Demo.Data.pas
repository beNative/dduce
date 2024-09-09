{
  Copyright (C) 2013-2024 Tim Sinaeve tim.sinaeve@gmail.com

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
  Vcl.ImgList, Vcl.Controls, Vcl.BaseImageCollection, Vcl.ImageCollection,
  Vcl.VirtualImageList,

  SVGIconImageCollection;

type
  TdmData = class(TDataModule)
    imlMain : TVirtualImageList;
    imcMain : TSVGIconImageCollection;

  private
    function GetImageList: TVirtualImageList;

  public
    property ImageList: TVirtualImageList
      read GetImageList;
  end;

var
  dmData : TdmData;

function Data: TdmData;

implementation

{$R *.dfm}

uses
  Vcl.Forms;


{
  Don't forget also the importance of TVirtualImageList.PreserveItems when you
  have a large ImageCollection with many linked Actions. Without setting this
  property to "True", everytime you add or remove an icon in the collection,
  you have to check and change the ImageIndex of all the Actions.
}

{$REGION 'interfaced routines'}
function Data: TdmData;
begin
  Result := dmData;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TdmData.GetImageList: TVirtualImageList;
begin
  Result := imlMain;
end;
{$ENDREGION}

end.



