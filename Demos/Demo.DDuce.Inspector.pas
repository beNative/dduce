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

unit Demo.DDuce.Inspector;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList,
  Vcl.ImgList, System.Actions, Vcl.ActnList, Vcl.ExtCtrls,

{$IFDEF SPRING}
  Spring, Spring.Collections,
{$ENDIF}

  DDuce.Components.Inspector, DDuce.Components.PropertyInspector,

  Demo.Contact;

type
  TfrmInspector = class(TForm)
    pnlLeft     : TPanel;
    pnlRight    : TPanel;
    splVertical : TSplitter;
    aclMain     : TActionList;
    imlMain     : TImageList;

  private
    FInspector         : TInspector;
    FPropertyInspector : TPropertyInspector;
    FList              : IList<TContact>;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  Demo.Factories;

{ TfrmInspector }

procedure TfrmInspector.AfterConstruction;
begin
  inherited AfterConstruction;
  FInspector := TDemoFactories.CreateInspector(Self, pnlRight);
  FPropertyInspector :=
    TDemoFactories.CreatePropertyInspector(Self, pnlLeft, FInspector);
  FList := TDemoFactories.CreateContactList(1000);
end;

end.
