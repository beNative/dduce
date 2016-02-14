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

unit DDuce.Editor.AlignLines.Settings;

interface

uses
  System.Classes,
  Vcl.Forms, Vcl.Controls,

  DDuce.Editor.Types;

const
  DEFAULT_WIDTH = 360;

type
  TAlignLinesSettings = class(TComponent)
  strict private
    FAlignInParagraphs    : Boolean;
    FAlignToToken         : TAlignToToken;
    FKeepSpaceAfterToken  : Boolean;
    FKeepSpaceBeforeToken : Boolean;
    FRemoveWhiteSpace     : Boolean;
    FSortAfterAlign       : Boolean;
    FSortDirection        : TSortDirection;
    FTokens               : TStringList;
    FWidth                : Integer;

    function GetTokens: TStrings;
    procedure SetTokens(AValue: TStrings);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

  published
    property AlignInParagraphs: Boolean
      read FAlignInParagraphs write FAlignInParagraphs default False;

    property AlignToToken: TAlignToToken
      read FAlignToToken write FAlignToToken default atLeftMost;

    property SortAfterAlign: Boolean
      read FSortAfterAlign write FSortAfterAlign default False;

    property SortDirection: TSortDirection
      read FSortDirection write FSortDirection default sdAscending;

    property RemoveWhiteSpace: Boolean
      read FRemoveWhiteSpace write FRemoveWhiteSpace default False;

    property KeepSpaceBeforeToken: Boolean
      read FKeepSpaceBeforeToken write FKeepSpaceBeforeToken default False;

    property KeepSpaceAfterToken: Boolean
      read FKeepSpaceAfterToken write FKeepSpaceAfterToken default False;

    property Tokens: TStrings
      read GetTokens write SetTokens;

    property Width: Integer
      read FWidth write FWidth default DEFAULT_WIDTH;
  end;

implementation

{$REGION 'construction and destruction' /fold}
procedure TAlignLinesSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FSortDirection := sdAscending;
  FAlignToToken  := atLeftMost;
  FWidth         := DEFAULT_WIDTH;
  FTokens            := TStringList.Create;
  FTokens.Duplicates := dupIgnore;
  FTokens.Sorted     := True;
end;

procedure TAlignLinesSettings.BeforeDestruction;
begin
  FTokens.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods' /fold}
function TAlignLinesSettings.GetTokens: TStrings;
begin
  Result := FTokens;
end;

procedure TAlignLinesSettings.SetTokens(AValue: TStrings);
begin
  FTokens.Assign(AValue);
end;
{$ENDREGION}

{$REGION 'public methods' /fold}
procedure TAlignLinesSettings.AssignTo(Dest: TPersistent);
var
  ALS: TAlignLinesSettings;
begin
  if Dest is TAlignLinesSettings then
  begin
    ALS := TAlignLinesSettings(Dest);
    ALS.KeepSpaceAfterToken  := KeepSpaceAfterToken;
    ALS.KeepSpaceBeforeToken := KeepSpaceBeforeToken;
    ALS.AlignInParagraphs    := AlignInParagraphs;
    ALS.RemoveWhiteSpace     := RemoveWhiteSpace;
    ALS.AlignToToken         := AlignToToken;
    ALS.Tokens               := Tokens;
    ALS.Width                := Width;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TAlignLinesSettings.Assign(Source: TPersistent);
var
  ALS: TAlignLinesSettings;
begin
  if Source is TAlignLinesSettings then
  begin
    ALS := TAlignLinesSettings(Source);
    KeepSpaceAfterToken  := ALS.KeepSpaceAfterToken;
    KeepSpaceBeforeToken := ALS.KeepSpaceBeforeToken;
    AlignInParagraphs    := ALS.AlignInParagraphs;
    AlignToToken         := ALS.AlignToToken;
    RemoveWhiteSpace     := ALS.RemoveWhiteSpace;
    Tokens               := ALS.Tokens;
    Width                := ALS.Width;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

initialization
  RegisterClass(TAlignLinesSettings);

end.

