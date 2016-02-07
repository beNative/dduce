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

unit DDuce.Editor.Search.Data;

{ Data structures used to display the search results hierarchically. }

{ TSearchResultGroup -> TSearchResultLine -> TSearchResult }

interface

uses
  System.Classes, System.SysUtils, System.Contnrs, System.Types,

  Spring.Collections;

type
  TSearchResult = class(TPersistent)
  private
    FIndex      : Integer;
    FLine       : Integer;
    FColumn     : Integer;
    FBlockBegin : TPoint;
    FBlockEnd   : TPoint;
    FFileName   : string;
    FMatch      : string;
    FShowMatch  : Boolean;
    FStartPos   : Integer;
    FEndPos     : Integer;
    FViewName   : string;

    function GetText: string;

  public
    property BlockBegin : TPoint
      read FBlockBegin write FBlockBegin;

    property BlockEnd : TPoint
      read FBlockEnd write FBlockEnd;

    property ViewName: string
      read FViewName write FViewName;

    property FileName : string
      read FFileName write FFileName;

    property Index: Integer
      read FIndex write FIndex;

    property Line : Integer
      read FLine write FLine;

    property Column : Integer
      read FColumn write FColumn;

    property StartPos: Integer
      read FStartPos write FStartPos;

    property EndPos: Integer
      read FEndPos write FEndPos;

    property Match: string
      read FMatch write FMatch;

    property ShowMatch: Boolean
      read FShowMatch write FShowMatch;

  published
    property Text: string
      read GetText;

  end;

  { TSearchResultLine }

  TSearchResultLine = class(TPersistent)
  private
    FLine : Integer;
    FList : IObjectList;
    function GetText: string;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property List: IObjectList
      read FList;

    property Line : Integer
      read FLine write FLine;

  published
    property Text: string
      read GetText;

  end;

  { TSearchResultGroup }

  TSearchResultGroup = class(TPersistent)
  private
    FLines    : IObjectList;
    FFileName : string;
    FViewName : string;

    function GetText: string;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Lines: IObjectList
      read FLines;

    property ViewName: string
      read FViewName write FViewName;

    property FileName : string
      read FFileName write FFileName;

  published
    property Text: string
      read GetText;

  end;

implementation

resourcestring
  SPosition = 'Position: (%d, %d)';
  SMatch    = ': ''%s''';
  SLine     = 'Line: %d';
  SGroup    = '%s (%d matching lines)';

{$REGION 'TSearchResult'}
function TSearchResult.GetText: string;
begin
  Result := Format(SPosition, [Column, Line]);
  if ShowMatch then
  begin
    Result := Result + Format(SMatch, [Match]);
  end;
end;
{$ENDREGION}

{$REGION 'TSearchResultLine'}
procedure TSearchResultLine.AfterConstruction;
begin
  inherited AfterConstruction;
  FList := TCollections.CreateObjectList<TSearchResult>(True) as IObjectList;
end;

procedure TSearchResultLine.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;

function TSearchResultLine.GetText: string;
begin
  Result := Format(SLine, [Line]);
end;
{$ENDREGION}

{$REGION 'TSearchResultGroup'}
function TSearchResultGroup.GetText: string;
begin
  Result := Format(SGroup, [FFileName, FLines.Count]);
end;

procedure TSearchResultGroup.AfterConstruction;
begin
  inherited AfterConstruction;
  FLines := TCollections.CreateObjectList<TSearchResultLine>(True) as IObjectList;
end;

procedure TSearchResultGroup.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;
{$ENDREGION}

end.

