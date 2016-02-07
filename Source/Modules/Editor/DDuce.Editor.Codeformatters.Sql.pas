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

unit DDuce.Editor.CodeFormatters.SQL;

interface

uses
  System.Classes, System.SysUtils,

  //ts.Core.SQLParser, ts.Core.SQLScanner, ts.Core.SQLTree,

  DDuce.Editor.CodeFormatters;

type
  TSQLFormatter = class(TInterfacedObject, ICodeFormatter)
  strict private
//    FLineReader : TStreamLineReader;
//    FSQLParser  : TSQLParser;
//    FSQLScanner : TSQLScanner;
    FSQLStream  : TStringStream;

  strict protected
    function Format(const AString: string): string; virtual;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TSQLFormatter.AfterConstruction;
begin
  inherited AfterConstruction;
//  FLineReader := TStreamLineReader.Create(FSQLStream);
//  FSQLScanner := TSQLScanner.Create(FLineReader);
//  FSQLParser  := TSQLParser.Create(FSQLScanner);
end;

procedure TSQLFormatter.BeforeDestruction;
begin
  FSQLStream.Free;
//  FLineReader.Free;
//  FSQLScanner.Free;
//  FSQLParser.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TSQLFormatter.Format(const AString: string): string;
var
  SS : TStringStream;
//  LR : TStreamLineReader;
//  P  : TSQLParser;
//  S  : TSQLScanner;
//  E  : TSQLElement;
begin
//  SS := TStringStream.Create(AString);
//  LR := TStreamLineReader.Create(SS);
//  S  := TSQLScanner.Create(LR);
//  P  := TSQLParser.Create(S);
//  try
//    try
//      E := P.Parse;
//    except
//      e:=NIL;
//      Result:=AString;
//    end;
//   if (Assigned(E)) and (length(aString)>0) then
//      Result := E.GetAsSQL(
//        [
//          //sfoDoubleQuotes,           // Use double quote character for string literals
//          sfoBackslashEscape,        // Backslash escapes in string literals
//          //sfoSingleQuoteIdentifier,  // quote Identifiers using '
//          //sfoDoubleQuoteIdentifier,  // quote Identifiers using "
//          //sfoBackQuoteIdentifier,    // quote Identifiers using `
//          sfoLowercaseKeyword,       // Lowercase SQL keywords
//          sfoOneFieldPerLine,        // One field per line in SELECT, Update, Insert
//          sfoIndentFields,           // Indent fields (indent=2 space characters)
//          sfoOneTablePerLine,        // One table per line in select FROM clause
//          sfoIndentTables,           // Indent tables in FROM clause
//          sfoNoBracketRightJoin,     // In join, Do not put ( ) around right table if it is also a join
//          //sfoBracketLeftJoin,        // In join, put ( ) around left table if it is also a join
//          sfoWhereOnSeparateLine,    // Put WHERE clause on a separate line
//          sfoIndentWhere,            // Indent WHERE clause
//          sfoOneGroupByFieldPerLine, // One field per line in GROUP BY
//          sfoIndentGroupByFields,    // Indent GROUP BY fields (indent=2 space characters)
//          sfoHavingOnSeparateLine,   // Put HAVING clause on a separate line
//          sfoIndentHaving,           // Indent HAVING clause
//          sfoUnionOnSeparateLine,    // Put UNION on separate line
//          sfoOneOrderByFieldPerLine, // One field per line in ORDER BY
//          sfoIndentOrderByFields,    // Indent ORDER BY fields (indent=2 space characters)
//          sfoPlanOnSeparateLine,     // Put HAVING clause on a separate line
//          sfoIndentPlan,             // Indent HAVING clause
//          sfoOneLogicalPerLine,      // in AND or OR clauses, put newline before AND or OR
//          sfoListNoSpaceBeforeComma, // In comma-separated lists, do not put space before ,
//          //sfoListNoSpaceAfterComma,  // In comma-separated lists, do not put space after ,
//          sfoForceAscending,         // In ORDER BY, explicitly write ASC
//          sfoMultilineDeclareFunction, // Separate parts of 'Declare function' with newlines
//          sfoMultilineCreateDatabase,  // Separate parts of create/alter database with newlines
//          sfoMultilineCreateShadow,    // Separate additional filespecs of create/alter shadow with newlines
//          sfoIndentProcedureBlock      // Indent statements inside procedure/trigger statement block
//        ],
//        2
//      );
//  finally
//    SS.Free;
//    LR.Free;
//    P.Free;
//    S.Free;
//    E.Free;
//  end;
end;
{$ENDREGION}

end.

