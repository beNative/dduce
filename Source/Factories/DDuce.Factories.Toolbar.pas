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


unit DDuce.Factories.ToolBar;

interface

uses
  System.Classes,
  Vcl.ComCtrls, Vcl.Controls, Vcl.Menus;

type
  TToolBarFactory = class sealed

    class function CreateToolButton(
       AParent    : TToolBar;
       AAction    : TBasicAction;
       APopupMenu : TPopupMenu = nil
    ): TToolButton; overload;

    class function CreateToolBar(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TToolBar;
  end;

implementation

uses
  Spring;

class function TToolBarFactory.CreateToolBar(AOwner: TComponent;
  AParent: TWinControl): TToolBar;
var
  TB : TToolbar;
begin
  Guard.CheckNotNull(AOwner, 'AOwner');
  Guard.CheckNotNull(AParent, 'AParent');
  TB := TToolBar.Create(AOwner);
  TB.Parent := AParent;
  //TB.Images := FActions.ActionList.Images;
  Result := TB;
end;

class function TToolBarFactory.CreateToolButton(AParent: TToolBar;
  AAction: TBasicAction; APopupMenu: TPopupMenu): TToolButton;
var
  TB: TToolButton;
begin
  TB := TToolButton.Create(AParent);
  TB.Parent := AParent;
  if not Assigned(AAction) then
  begin
    TB.Style := tbsDivider;
  end
  else
  begin
    if Assigned(APopupMenu) then
    begin
      TB.Style        := tbsDropDown;
      TB.DropdownMenu := APopupMenu;
    end;
    TB.Action := AAction;
  end;
  Result := TB;
end;

end.
