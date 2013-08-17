
{.$DEFINE SPRING}
{$DEFINE DSHARP}

{$I jedi.inc}                // Import JEDI Development environment directives

{$IFNDEF DELPHI2010_UP}
  {$MESSAGE ERROR 'This library requires Delphi 2010 or higher.'}
{$ENDIF}

{$IFNDEF MSWINDOWS}
  {$MESSAGE ERROR 'This library currently only supports Windows.'}
{$ENDIF}

{$WARN UNIT_PLATFORM OFF}    // Turn off UNIT_PLATFORM warning
{$WARN SYMBOL_PLATFORM OFF}  // Turn off SYMBOL_PLATFORM warning
