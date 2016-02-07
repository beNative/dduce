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

unit DDuce.Editor.SortStrings.Settings;

interface

uses
  System.Classes, System.SysUtils,

  DDuce.Editor.Types;

const
  DEFAULT_WIDTH = 360;

type
  TSortStringsSettings = class(TComponent)
  strict private
    FCaseSensitive : Boolean;
    FIgnoreSpaces  : Boolean;
    FSortDirection : TSortDirection;
    FSortScope     : TSortScope;
    FWidth         : Integer;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

  published
    property SortDirection: TSortDirection
      read FSortDirection write FSortDirection default sdAscending;

    property SortScope: TSortScope
      read FSortScope write FSortScope default ssLines;

    property CaseSensitive: Boolean
      read FCaseSensitive write FCaseSensitive default False;

    property IgnoreSpaces: Boolean
      read FIgnoreSpaces write FIgnoreSpaces default False;

    property Width: Integer
      read FWidth write FWidth default DEFAULT_WIDTH;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TSortStringsSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FWidth := DEFAULT_WIDTH;
end;

procedure TSortStringsSettings.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TSortStringsSettings.AssignTo(Dest: TPersistent);
var
  S: TSortStringsSettings;
begin
  if Dest is TSortStringsSettings then
  begin
    S := TSortStringsSettings(Dest);
    S.Width := Width;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TSortStringsSettings.Assign(Source: TPersistent);
var
  S : TSortStringsSettings;
begin
  if Source is TSortStringsSettings then
  begin
    S := TSortStringsSettings(Source);
    Width := S.Width;
    SortDirection :=  S.SortDirection;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

initialization
  RegisterClass(TSortStringsSettings);

end.

