{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit DDuce.Editor.Factories.Manager;

interface

uses
  System.Classes, System.SysUtils,

  DDuce.Editor.Interfaces;

type
{ TEditorManagerFactory }

  TEditorManagerFactory = class(TInterfacedObject, IEditorManagerFactory)
    function CreateInstance(
            AOwner            : TComponent = nil;
            APersistSettings  : Boolean = False;
      const ASettingsFileName : string = ''
    ): IEditorManager; overload;

    function CreateInstance(
      AOwner    : TComponent;
      ASettings : IEditorSettings
    ): IEditorManager; overload;
  end;

implementation

uses
  Vcl.Forms,

  DDuce.Editor.Manager;

{ TEditorManagerFactory }

function TEditorManagerFactory.CreateInstance(AOwner: TComponent;
  APersistSettings: Boolean; const ASettingsFileName: string): IEditorManager;
var
  O : TComponent;
  S : string;
begin
  if not Assigned(AOwner) then
    O := Application
  else
    O := AOwner;
  Result := TdmEditorManager.Create(O, nil);
  Result.PersistSettings := APersistSettings;
  if APersistSettings then
  begin
//    if ASettingsFileName = '' then
//      S := ApplicationName + '.xml'
//    else
//      S := ASettingsFileName;
    Result.Settings.FileName := S;
  end;
end;

function TEditorManagerFactory.CreateInstance(AOwner: TComponent;
  ASettings: IEditorSettings): IEditorManager;
begin
  Result := TdmEditorManager.Create(AOwner, ASettings);
end;

end.

