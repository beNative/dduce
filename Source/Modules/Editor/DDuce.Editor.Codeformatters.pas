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

unit DDuce.Editor.CodeFormatters;

interface

uses
  System.Classes, System.SysUtils;

type
  ICodeFormatter = interface
  ['{8E423CF0-8F69-476C-9D09-718645ADE97E}']
    function Format(const AString: string): string;
  end;

  TPascalFormatter = class(TInterfacedObject, ICodeFormatter)
  strict protected
    function Format(const AString: string): string; virtual;
  end;

  TCPPFormatter = class (TInterfacedObject, ICodeFormatter)
  strict protected
    function Format(const AString: string): string; virtual;
  end;

  TJavaFormatter = class (TInterfacedObject, ICodeFormatter)
  strict protected
    function Format(const AString: string): string; virtual;
  end;

  TCSharpFormatter = class (TInterfacedObject, ICodeFormatter)
  strict protected
    function Format(const AString: string): string; virtual;
  end;

  TXMLFormatter = class(TInterfacedObject, ICodeFormatter)
  strict protected
    function Format(const AString: string): string; virtual;
  end;

  THTMLFormatter = class(TInterfacedObject, ICodeFormatter)
  strict protected
    function Format(const AString: string): string; virtual;
  end;

implementation

uses
  Vcl.Forms,

  DDuce.Editor.Utils;

function RunFormatterProcess(const AExeName: string; const AParams: string;
  const AString: string; const ATempFile: string): string;
var
  //Process : TProcess;
  SL      : TStringList;
  S       : string;
  T       : string;
begin
//  S := GetApplicationPath + AExeName;
//  T := GetApplicationPath + ATempFile;
//  if FileExistsUTF8(S) then
//  begin
//    SL := TStringList.Create;
//    try
//      SL.Text := AString;
//      SL.SaveToFile(T);
//      Process := TProcess.Create(nil);
//      try
//        Process.Options := [poNoConsole];
//        Process.CommandLine := SysUtils.Format(S + ' ' + AParams, [T]);
//        Process.Execute;
//        while Process.Running do
//        begin
//          Application.ProcessMessages;
//        end;
//      finally
//        FreeAndNil(Process);
//      end;
//      SL.LoadFromFile(T);
//      Result := SL.Text;
//    finally
//      FreeAndNil(SL);
//    end;
//    if FileExistsUTF8(T) then
//      DeleteFileUTF8(T);
//  end
//  else
//    raise Exception.CreateFmt('%s not found!', [S]);
end;

function TCPPFormatter.Format(const AString: string): string;
begin
  Result := RunFormatterProcess(
    'AStyle.exe',
    '--style=allman --indent=spaces=2 --suffix=none --quiet --mode=c %s',
    AString,
    'Formatter.tmp'
  );
end;

function TJavaFormatter.Format(const AString: string): string;
begin
  Result := RunFormatterProcess(
    'AStyle.exe',
    '--style=java --indent=spaces=2 --suffix=none --quiet --mode=java %s',
    AString,
    'Formatter.tmp'
  );
end;

function TCSharpFormatter.Format(const AString: string): string;
begin
  Result := RunFormatterProcess(
    'AStyle.exe',
    '--style=allman --indent=spaces=2 --suffix=none --quiet --mode=cs %s',
    AString,
    'Formatter.tmp'
  );
end;

{
  Poor Man's T-SQL Formatter - a small free Transact-SQL formatting
  library for .Net 2.0, written in C#. Distributed under AGPL v3.
  Copyright (C) 2011 Tao Klerks
  v1.0.1.23412

  Usage notes:

  SqlFormatter <filename or pattern> <options>

  is  indentString (default: \t)
  st  spacesPerTab (default: 4)
  mw  maxLineWidth (default: 999)
  tc  trailingCommas (default: false)
  sac spaceAfterExpandedComma (default: false)
  ebc expandBetweenConditions (default: true)
  ebe expandBooleanExpressions (default: true)
  ecs expandCaseStatements (default: true)
  ecl expandCommaLists (default: true)
  uk  uppercaseKeywords (default: true)
  sk  standardizeKeywords (default: false)
  e   extensions (default: sql)
  r   recursive (default: false)
  b   backups (default: true)
  b   outputFileOrFolder (default: none; if set, overrides the backup option)
  h ? help

  Disable boolean options with a trailing minus, enable by just specifying them or
   with a trailing plus.

  eg:

  SqlFormatter TestFiles\* /is:"  " /tc /uc-

  or

  SqlFormatter test*.sql /o:resultfile.sql
}

//function TSQLFormatter.Format(const AString: string): string;
//begin
//  Result := RunFormatterProcess(
//    'SQLFormatter.exe',
//    '%s /is:"  " /st:2 /mw:80 /tc /uk-',
//    AString,
//    'Formatter.sql'
//  );
//end;

function TXMLFormatter.Format(const AString: string): string;
begin
  Result := DDuce.Editor.Utils.FormatXML(AString);
end;

function THTMLFormatter.Format(const AString: string): string;
begin
  Result := DDuce.Editor.Utils.FormatXML(AString);
end;

function TPascalFormatter.Format(const AString: string): string;
begin
  Result := RunFormatterProcess(
    'Formatter.exe',
    '-silent -delphi -config Formatter.config %s',
    AString,
    'Formatter.tmp'
  );
end;

end.

