unit DDuce.Components.XMLTree.NodeAttributes;

{$I ..\DDuce.inc}

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Graphics,

  DSharp.Core.Collections;

type
  TNodeType = (
    ntUnknown,   // Error
    ntRoot,     // DocumentElement, NODE_DOCUMENT, NODE_PROCESSING_INSTRUCTION
    ntComment,  // NODE_COMMENT
    ntText,     // NODE_TEXT, NODE_CDATA_SECTION
    ntAttribute,// NODE_ATTRIBUTE
    ntElement,  // NODE_ELEMENT without ChildNodes
    ntNode      // NODE_ELEMENT with ChildNodes
  );

type
  TNodeAttributesItem = class(TCollectionItem)
  private
    FBackGroundColor : TColor;
    FFont            : TFont;
    FName            : string;
    FNodeType        : TNodeType;

    procedure SetBackGroundColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetNodeType(AValue: TNodeType);
  protected
    function GetDisplayName: string; override;

  public
    constructor Create(ACollection: System.Classes.TCollection); override;
    procedure BeforeDestruction; override;

  published
    property Name: string
      read FName write FName;

    property NodeType: TNodeType
      read FNodeType write SetNodeType;

    property BackGroundColor: TColor
      read FBackGroundColor write SetBackGroundColor;

    property Font: TFont
      read FFont write SetFont;
  end;

type
  TNodeAttributes = class(TOwnedCollection<TNodeAttributesItem>)
  private
    function GetItemByType(Index: TNodeType): TNodeAttributesItem;

  public
    property ItemByType[Index: TNodeType]: TNodeAttributesItem
      read GetItemByType;
  end;

implementation

{$region 'TNodeAttributesItem' /fold}
{$region 'construction and destruction' /fold}
constructor TNodeAttributesItem.Create(ACollection: System.Classes.TCollection);
begin
  inherited Create(ACollection);
  FFont := TFont.Create;
  FFont.Size := 8;
end;

function TNodeAttributesItem.GetDisplayName: string;
begin
  Result := FName;
end;

procedure TNodeAttributesItem.BeforeDestruction;
begin
  FFont.Free;
  inherited BeforeDestruction;
end;
{$endregion}

{$region 'property access mehods' /fold}
procedure TNodeAttributesItem.SetBackGroundColor(AValue: TColor);
begin
  if AValue <> BackGroundColor then
  begin
    FBackGroundColor := AValue;
    Changed(False);
  end;
end;

procedure TNodeAttributesItem.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
  Changed(False);
end;

procedure TNodeAttributesItem.SetNodeType(AValue: TNodeType);
begin
  if AValue <> NodeType then
  begin;
    FNodeType := AValue;
    Changed(False);
  end;
end;
{$endregion}
{$endregion}

{$region 'TNodeAttributes' /fold}
{$region 'property access mehods' /fold}
function TNodeAttributes.GetItemByType(Index: TNodeType): TNodeAttributesItem;
var
  I : Integer;
  B : Boolean;
begin
  I := 0;
  B := False;
  Result := nil;
  while (I < Count) and not B do
  begin
    if Items[I].NodeType = Index then
    begin
      Result := Items[I];
      B := True;
    end;
    Inc(I);
  end;
end;
{$endregion}
{$endregion}
end.

