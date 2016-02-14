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

unit DDuce.Editor.ActionList.Templates;

interface

uses
  System.Classes, System.SysUtils, System.Actions, System.Rtti,
  Vcl.ActnList,

  BCEditor.Editor.KeyCommands,

  Spring.Collections,

  DSharp.Windows.ColumnDefinitions,
  DSharp.Core.DataTemplates, DSharp.Windows.ControlTemplates,
  DSharp.Windows.ColumnDefinitions.ControlTemplate;

{$REGION 'TActionListTemplate'}
type
  TActionListTemplate = class(TColumnDefinitionsControlTemplate)
    function GetValue(
      const Item        : TObject;
      const ColumnIndex : Integer
    ) : TValue; override;
  end;
{$ENDREGION}


{$REGION 'TKeyStrokeTemplate'}
type
  TKeyCommandTemplate = class(TColumnDefinitionsControlTemplate)
    function GetValue(
      const Item        : TObject;
      const ColumnIndex : Integer
    ) : TValue; override;
  end;
{$ENDREGION}

resourcestring
  SName       = 'Name';
  SCategory   = 'Category';
  SCaption    = 'Caption';
  SShortcut   = 'Shortcut';
  SShortcut2  = 'Shortcut2';
  SHint       = 'Hint';
  SVisible    = 'Visible';
  SEnabled    = 'Enabled';
  SCommand    = 'Command';
  SButton     = 'Button';
  SShift      = 'Shift';
  SShiftMask  = 'ShiftMask';
  SMoveCaret  = 'MoveCaret';

implementation

uses
  Vcl.Menus;

{$REGION 'TActionListTemplate'}
function TActionListTemplate.GetValue(const Item: TObject;
  const ColumnIndex: Integer): TValue;
var
  CA: TContainedAction;
  CD: TColumnDefinition;
begin
  CA := Item as TContainedAction;
  CD := TColumnDefinition(ColumnDefinitions[ColumnIndex]);
  if CD.Caption = SShortcut then
    Result := ShortCutToText(CA.Shortcut)
  else
    Result := inherited GetValue(Item, ColumnIndex);
end;
{$ENDREGION}

{$REGION 'TKeyCommandTemplate'}
function TKeyCommandTemplate.GetValue(const Item: TObject;
  const ColumnIndex: Integer): TValue;
var
  KC: TBCEditorKeyCommand;
  CD: TColumnDefinition;
  S : string;
begin
  KC := Item as TBCEditorKeyCommand;
  CD := TColumnDefinition(ColumnDefinitions[ColumnIndex]);
  if CD.Caption = SCommand then
  begin
    if not EditorCommandToIdent(KC.Command, S) then
      S := IntToStr(KC.Command);
    Result := S;
  end
  else if CD.Caption = SShortcut then
  begin
     Result := ShortCutToText(KC.ShortCut);
  end
  else if CD.Caption = SShortcut2 then
  begin
    Result := ShortCutToText(KC.SecondaryShortCut);
  end
  else
    Result := inherited GetValue(Item, ColumnIndex);
end;
{$ENDREGION}

end.
