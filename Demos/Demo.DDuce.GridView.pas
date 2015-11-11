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

{$I ..\Source\DDuce.inc}

unit Demo.DDuce.GridView;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.ActnList, Vcl.ImgList,

  Spring, Spring.Collections,

  DDuce.Components.GridView, DDuce.Components.PropertyInspector,

  Demo.Contact;

type
  TfrmGridView = class(TForm)
    imlMain     : TImageList;
    aclMain     : TActionList;
    pnlLeft     : TPanel;
    splVertical : TSplitter;
    pnlRight    : TPanel;

  private
    FGridView          : TGridView;
    FPropertyInspector : TPropertyInspector;
    FList              : IList<TContact>;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  Demo.Factories, DDuce.RandomData;

{$REGION 'construction and destruction'}
procedure TfrmGridView.AfterConstruction;
begin
  inherited AfterConstruction;
  FGridView := TDemoFactories.CreateGridView(Self, pnlRight);
  FPropertyInspector :=
    TDemoFactories.CreatePropertyInspector(Self, pnlLeft, FGridView);
  FList := TDemoFactories.CreateContactList(1000);
end;
{$ENDREGION}

end.
