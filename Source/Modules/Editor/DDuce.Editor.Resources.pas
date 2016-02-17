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

unit DDuce.Editor.Resources;

{ Shared resources. }

interface

uses
  System.Classes, System.SysUtils, System.ImageList,
  Vcl.Controls, Vcl.ImgList;

const
  HL_LOG  = 'LOG';
  HL_TXT  = 'TXT';
  HL_PAS  = 'PAS';
  HL_CPP  = 'CPP';
  HL_JAVA = 'JAVA';
  HL_SQL  = 'SQL';
  HL_XML  = 'XML';
  HL_LFM  = 'LFM';
  HL_INI  = 'INI';
  HL_BAT  = 'BAT';
  HL_RTF  = 'RTF';
  HL_RES  = 'RES';
  HL_PHP  = 'PHP';
  HL_PERL = 'PERL';
  HL_PY   = 'PY';
  HL_PO   = 'PO';
  HL_HTML = 'HTML';
  HL_CSS  = 'CSS';
  HL_JS   = 'JS';
  HL_JSON = 'JSON';
  HL_CS   = 'CS';
  HL_DIFF = 'DIFF';
  HL_SH   = 'SH';
  HL_TEX  = 'TEX';
  HL_RUBY = 'RUBY';
  HL_LUA  = 'LUA';

  // comma separated lists of supported file extensions (no spaces)
  FILE_EXTENSIONS_TXT  = 'txt,nfo,me';
  FILE_EXTENSIONS_PAS  = 'pas,dpr,pp,lpr,inc,dpk';
  FILE_EXTENSIONS_CPP  = 'cpp,hpp,' +            // C++
                         'c,h' +                 // regular C
                         'ino,pde';              // Arduino sketch
  FILE_EXTENSIONS_JAVA = 'java';                 // Java source file
  FILE_EXTENSIONS_SQL  = 'sql';                  // SQL script source file
  FILE_EXTENSIONS_XML  = 'xml,' +                // General XML file
                         'dtd,' +                // Document Type Definition
                         'xsd,' +                // XML Schema Definition
                         'xslt,' +               // XSL Transform file
                         'hgl,' +                // Highlighter (synunisyn)
                         'lpi,lps,lpk,' +        // Lazarus
                         'fpc,compiled' +        // FPC Make
                         'dproj,groupproj,' +    // Delphi
                         'template,' +           // Java templates
                         'svg,' +                // Scalable Vector Graphics
                         'docx,' +               // Open XML document
                         'docm,' +               // Word Macro-Enabled Document
                         'dotx,' +               // Word Template
                         'dotm,' +               // Word Macro-Enabled Template
                         'xlsx,' +               // Open XML Spreadsheet
                         'xlsm,' +               // Excel Macro-Enabled Workbook
                         'xltx,' +               // Excel Template
                         'xltm,' +               // Excel Macro-Enabled Template
                         'xlam,' +               // Excel Add-In
                         'pptx,' +               // Open XML Presentation
                         'pptm,' +               // PowerPoint Macro-Enabled Presentation
                         'potx,'  +              // PowerPoint Template
                         'potm,'  +              // PowerPoint Macro-Enabled Template
                         'ppam,'  +              // PowerPoint Add-In
                         'ppsx,'  +              // PowerPoint Show
                         'ppsm,' +               // PowerPoint Macro-Enabled Show
                         'vsdx,' +               // Visio Drawing
                         'vsdm,' +               // Visio Macro-Enabled Drawing
                         'vssx,' +               // Visio Stencil
                         'vssm,' +               // Visio Macro-Enabled Stencil
                         'vstx,' +               // Visio Template
                         'vstm,' +               // Visio Macro-Enabled Template
                         'sgml,' +               // Standard Generalized Markup Language
                         'xmmap,' +              // Mindjet MindManager
                         'vcxproj,' +            // Visual C++ Project file
                         'vcxproj.filters,' +    // Visual C++ Project filters
                         'cpppjoj';              // C++ Project file
  FILE_EXTENSIONS_LFM  = 'dfm,' +                // Delphi VCL form
                         'lfm,' +                // Lazarus LCL form
                         'fmx';                  // Delphi Firemonkey form
  FILE_EXTENSIONS_INI  = 'ini,' +
                         'fpd,' +                // FastReport definition
                         'reg,' +                // Windows registry file
                         'lrt,' +
                         'msg,' +
                         'prop,properties,' +
                         'desktop';
  FILE_EXTENSIONS_BAT  = 'bat,cmd';
  FILE_EXTENSIONS_RTF  = 'rtf';
  FILE_EXTENSIONS_RES  = 'res';
  FILE_EXTENSIONS_PHP  = 'php';
  FILE_EXTENSIONS_PERL = 'pl';
  FILE_EXTENSIONS_PY   = 'py';
  FILE_EXTENSIONS_HTML = 'html,htm';
  FILE_EXTENSIONS_PO   = 'po';
  FILE_EXTENSIONS_JS   = 'js';
  FILE_EXTENSIONS_CSS  = 'css';
  FILE_EXTENSIONS_CS   = 'cs';
  FILE_EXTENSIONS_DIFF = 'diff';
  FILE_EXTENSIONS_TEX  = 'tex';
  FILE_EXTENSIONS_SH   = 'sh';
  FILE_EXTENSIONS_RUBY = 'rb,rbw';
  FILE_EXTENSIONS_LUA  = 'lua';
  FILE_EXTENSIONS_JSON = 'json';

resourcestring
  STextNotFound = 'Text not found';
  SSearching    = 'Searching...';

  SINIDescription  = 'Settings file';
  SBATDescription  = 'Windows batch script';
  SPODescription   = 'Gettext translation strings';
  SRTFDescription  = 'Rich Text Format document';
  STXTDescription  = 'Text document';
  SXMLDescription  = 'XML document';
  SHTMLDescription = 'HTML document';
  SPASDescription  = 'Object Pascal source file';
  SSQLDescription  = 'SQL script';
  SCPPDescription  = 'C++ source file';
  SJavaDescription = 'Java source file';
  SLFMDescription  = 'Object Pascal form definition';
  SLOGDescription  = 'Log';
  SRESDescription  = 'Windows resources';
  SPHPDescription  = 'PHP script';
  SPERLDescription = 'Perl script';
  SPYDescription   = 'Python script';
  SCSSDescription  = 'Cascading Style Sheet';
  SJSDescription   = 'JavaScript';
  SCSDescription   = 'C# source file';
  SDIFFDescription = 'Diff';
  STEXDescription  = 'TeX document';
  SSHDescription   = 'Shell script';
  SRUBYDescription = 'Ruby script';
  SLUADescription  = 'Lua script';
  SJSONDescription = 'JSON document';

  SAskSaveChanges = 'File %s is modified. Do you want to save changes?';

  SNewEditorViewFileName   = '<new>';
  SFileMenuCaption         = '&File';
  SSeLectionMenuCaption    = 'Se&lection';
  SSearchMenuCaption       = '&Search';
  SInsertMenuCaption       = '&Insert';
  SViewMenuCaption         = '&View';
  SToolsMenuCaption        = '&Tools';
  SSettingsMenuCaption     = '&Settings';
  SHighlightersMenuCaption = '&Highlighters';
  SHelpMenuCaption         = '&Help';
  SApplicationMenuCaption  = '&Application';

  // CodeFilterDialog
  SOneLineWithMatchFound = '1 line with match found.';
  SLinesWithMatchFound   = '%d lines with match found.';

  // IEditorManager
  SNotImplementedYet     = 'This feature is not implemented yet';

const
  DEFAULT_BLEND_FACTOR = 128;

const
  ALineBreakStyles : array[TTextLineBreakStyle] of string = (
    'LF',
    'CRLF'
  );

  AHighlighterLayouts : array[0..47] of string = (
   'ActionScript',
   'ASP',
   'Assembler - 68HC11',
   'AutoIt v3',
   'AWK',
   'C',
   'C#',
   'C++',
   'CoffeeScript',
   'CSS',
   'D',
   'Delphi Form Module',
   'Free Pascal',
   'Go',
   'Groovy',
   'HTML with Scripts',
   'INI',
   'Inno Setup',
   'Java',
   'JavaScript',
   'JSON',
   'LaTex',
   'Lisp',
   'Lua',
   'MATLAB',
   'MS-DOS Batch',
   'Object Pascal',
   'Objective-C',
   'OCaml',
   'Perl',
   'PHP',
   'PowerShell',
   'Python',
   'Ruby',
   'Rust',
   'Scala',
   'SQL - Firebird',
   'SQL - Oracle',
   'SQL - PostgreSQL',
   'SQL - SQLite',
   'SQL - Standard',
   'SQL - Sybase',
   'TclTk',
   'Text',
   'UnrealScript',
   'Visual Basic',
   'XML',
   'XSL'
  );

type
  TResourcesDataModule = class(TDataModule)
    imlFunctionKeys : TImageList;
  end;

implementation

{$R *.dfm}

end.

