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

unit DDuce.Editor.Factories.Settings;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Controls,

  Spring.Collections,

  DDuce.Editor.Tools.Settings, DDuce.Editor.Highlighters, DDuce.Editor.Interfaces;

type
  TEditorSettingsFactory = class(TInterfacedObject, IEditorSettingsFactory)
  public
    procedure RegisterToolSettings(ASettings: TEditorToolSettings);
    procedure RegisterHighlighters(AHighlighters: THighlighters);

    function CreateInstance(
            AOwner    : TComponent = nil;
      const AFileName : string = ''
    ): IEditorSettings;
  end;

implementation

uses
  System.IOUtils,
  Vcl.Forms,

  DDuce.Editor.CodeFormatters,
//  DDuce.Editor.CodeFormatters.SQL,
  DDuce.Editor.AlignLines.Settings,
//  DDuce.Editor.CodeFilter.Settings,
//  DDuce.Editor.CodeShaper.Settings,
//  DDuce.Editor.HexEditor.Settings,
//  DDuce.Editor.HTMLView.Settings,
//  DDuce.Editor.MiniMap.Settings,
//  DDuce.Editor.SortStrings.Settings,
  DDuce.Editor.Search.Engine.Settings,
  DDuce.Editor.Settings,
  DDuce.Editor.Resources;

{$REGION 'private methods'}

procedure TEditorSettingsFactory.RegisterToolSettings(
  ASettings: TEditorToolSettings);
begin
  ASettings.RegisterSettings(TAlignLinesSettings, 'AlignLinesSettings');
//  ASettings.RegisterSettings(TCodeFilterSettings, 'CodeFilterSettings');
//  ASettings.RegisterSettings(THTMLViewSettings, 'HTMLViewSettings');
//  ASettings.RegisterSettings(TSortStringsSettings, 'SortStringsSettings');
//  ASettings.RegisterSettings(TMiniMapSettings, 'MiniMapSettings');
  //ASettings.RegisterSettings(THexEditorSettings, 'HexEditorSettings');
  ASettings.RegisterSettings(TSearchEngineSettings, 'SearchSettings');
//  ASettings.RegisterSettings(TCodeShaperSettings, 'CodeShaperSettings');
end;
//
procedure TEditorSettingsFactory.RegisterHighlighters(
  AHighlighters: THighlighters);
var
  S    : string;
  F    : string;
  //HL : IList<string>;

  procedure Reg(
    const ALayoutName     : string = '';
    const AName           : string = '';
    const ADescription    : string = '';
    const AFileExtensions : string = '';
    const ACodeFormatter  : ICodeFormatter = nil
  );
  var
    LPath : string;
    LExt  : string;
  begin
    LPath := '.\Highlighters';
    LExt  := '.json';
    AHighlighters.RegisterHighlighter(
      Format('%s\%s%s', [LPath, ALayoutName, LExt]),
      AName,
      ADescription,
      AFileExtensions,
      ACodeFormatter
    );
  end;

begin



//  HL := TCollections.CreateList<string>;
//  HL.AddRange(TDirectory.GetFiles('.\Highlighters', '*.json'));
//  Reg('ActionScript',
//  Reg('ASP',
//  Reg('Assembler - 68HC11',
//  Reg('AutoIt v3',
//  Reg('AWK',
//  Reg('C',
  Reg('C#',                 HL_CS,   SCSDescription,   FILE_EXTENSIONS_CS);
  Reg('C++',                HL_CPP,  SCPPDescription,  FILE_EXTENSIONS_CPP);
//  Reg('CoffeeScript',
//  Reg('CSS',
//  Reg('D',
  Reg('Delphi Form Module', HL_LFM,  SLFMDescription,  FILE_EXTENSIONS_LFM);
//  Reg('Free Pascal',        HL_PAS,  SPASDescription,  FILE_EXTENSIONS_PAS);
//  Reg('Go',
//  Reg('Groovy',
//  Reg('HTML with Scripts',
  Reg('INI',                HL_INI,  SINIDescription,  FILE_EXTENSIONS_INI);
  //Reg('Inno Setup',
  Reg('Java',               HL_JAVA, SJavaDescription, FILE_EXTENSIONS_JAVA);
  Reg('JavaScript',         HL_JS,   SJSDescription,   FILE_EXTENSIONS_JS);
  Reg('JSON',               HL_JSON, SJSONDescription, FILE_EXTENSIONS_JSON);
//  Reg('LaTex',
//  Reg('Lisp',
  Reg('Lua',                HL_LUA,  SLUADescription,  FILE_EXTENSIONS_LUA);
//  Reg('MATLAB',
  Reg('MS-DOS Batch',       HL_BAT,  SBATDescription,  FILE_EXTENSIONS_BAT);
  Reg('Object Pascal',      HL_PAS,  SPASDescription,  FILE_EXTENSIONS_PAS);
//  Reg('Objective-C',
//  Reg('OCaml',
  Reg('Perl',               HL_PERL, SPERLDescription, FILE_EXTENSIONS_PERL);
  Reg('PHP',                HL_PHP,  SPHPDescription,  FILE_EXTENSIONS_PHP);
//  Reg('PowerShell',
  Reg('Python',             HL_PY,   SPYDescription,   FILE_EXTENSIONS_PY);
  Reg('Ruby',               HL_RUBY, SRUBYDescription, FILE_EXTENSIONS_RUBY);
//  Reg('Rust',
//  Reg('Scala',
//  Reg('SQL - Firebird',
//  Reg('SQL - Oracle',
//  Reg('SQL - PostgreSQL',
//  Reg('SQL - SQLite',
  Reg('SQL - Standard',    HL_SQL,   SSQLDescription, FILE_EXTENSIONS_SQL);
//  Reg('SQL - Sybase',
//  Reg('TclTk',
  Reg('Text',              HL_TXT,   STXTDescription, FILE_EXTENSIONS_TXT);
//  Reg('UnrealScript',
//  Reg('Visual Basic',
  Reg('XML',               HL_XML,   SXMLDescription, FILE_EXTENSIONS_XML);
//  Reg('XSL',

end;
{$ENDREGION}

{$REGION 'public methods'}
function TEditorSettingsFactory.CreateInstance(AOwner: TComponent;
  const AFileName: string): IEditorSettings;
var
  ES : IEditorSettings;
begin
  ES := TEditorSettings.Create(AOwner);
  RegisterToolSettings(ES.ToolSettings);
  RegisterHighlighters(ES.Highlighters);
  if AFileName <> '' then
  begin
    ES.FileName := AFileName;
    ES.Load;
  end;
  Result := ES;
end;
{$ENDREGION}

end.

