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

unit Demo.DDuce.VirtualDBGrid;

{$I ..\Source\DDuce.inc}

{ Form demonstrating the TVirtualDBGrid component connected to a generic
  TListDataSet. }

interface

uses
  System.Actions, System.Classes,
  Vcl.ActnList, Vcl.ImgList, Vcl.Controls, Vcl.DBCtrls, Vcl.ExtCtrls, Vcl.Forms,
  Vcl.ComCtrls,
  Data.DB,

{$IFDEF SPRING}
  Spring, Spring.Collections,
{$ENDIF}

  DDuce.Components.VirtualDBGrid, DDuce.Components.ListDataSet,
  DDuce.Components.PropertyInspector,

  Demo.Contact, System.ImageList;

type
  TfrmVirtualDBGrid = class(TForm)
    splHorizontal       : TSplitter;
    sbrMain             : TStatusBar;
    pnlTop              : TPanel;
    pnlBottom           : TPanel;
    pnlLeft             : TPanel;
    splVertical         : TSplitter;
    pnlRight            : TPanel;
    imlMain             : TImageList;
    aclMain             : TActionList;
    actInspectComponent : TAction;
    dscMain             : TDataSource;
    navMain             : TDBNavigator;

  private
    FVDBG      : TVirtualDBGrid;
    FList      : IList<TContact>;
    FDataSet   : TListDataset<TContact>;
    FInspector : TPropertyInspector;

  public
    procedure AfterConstruction; override;

  end;

implementation

uses
  Demo.Data, Demo.Factories;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmVirtualDBGrid.AfterConstruction;
begin
  inherited;
  FList           := TDemoFactories.CreateContactList(1000);
  FDataSet        := TListDataset<TContact>.Create(Self, FList);
  dscMain.DataSet := FDataSet;
  FVDBG           := TDemoFactories.CreateVirtualDBGrid(Self, pnlLeft, dscMain);
  FInspector      := TDemoFactories.CreateInspector(Self, pnlRight, FVDBG);
  FDataSet.Active := True;
  FVDBG.Header.AutoFitColumns;
end;
{$ENDREGION}

end.
