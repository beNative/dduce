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

unit DDuce.Editor.CodeTags;

interface

uses
  System.Classes, System.SysUtils;

type
  TCodeTagItem = class(TComponent)
  strict private
    FEndTag   : string;
    FStartTag : string;

    procedure SetEndTag(AValue: string);
    procedure SetStartTag(AValue: string);

  public
    procedure Assign(Source: TPersistent); override;

  published
    property StartTag: string
      read FStartTag write SetStartTag;

    property EndTag: string
      read FEndTag write SetEndTag;
  end;

  { TCodeTags }

  TCodeTags = class(TComponent)
  private
    function GetItem(Index: Integer): TCodeTagItem;
    procedure SetItem(Index: Integer; AValue: TCodeTagItem);
  public
    function Add: TCodeTagItem;

    property Items[Index: Integer]: TCodeTagItem
      read GetItem write SetItem; default;
  end;

implementation

function TCodeTags.GetItem(Index: Integer): TCodeTagItem;
begin
  Result := Components[Index] as TCodeTagItem;
end;

procedure TCodeTags.SetItem(Index: Integer; AValue: TCodeTagItem);
begin
  Components[Index].Assign(AValue);
end;

function TCodeTags.Add: TCodeTagItem;
begin
  Result := TCodeTagItem.Create(Self);
end;

{$REGION 'property access mehods'}
procedure TCodeTagItem.SetStartTag(AValue: string);
begin
  if FStartTag = AValue then
    Exit;
  FStartTag := AValue;
end;

procedure TCodeTagItem.Assign(Source: TPersistent);
var
  CTI : TCodeTagItem;
begin
 if (Source <> Self) and (Source is TCodeTagItem) then
 begin
   CTI := TCodeTagItem(Source);
   StartTag := CTI.StartTag;
   EndTag := CTI.EndTag;
 end
 else
   inherited Assign(Source);
end;

procedure TCodeTagItem.SetEndTag(AValue: string);
begin
  if FEndTag = AValue then
    Exit;
  FEndTag := AValue;
end;
{$ENDREGION}

initialization
  RegisterClass(TCodeTags);
  RegisterClass(TCodeTagItem);

end.

