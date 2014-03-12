program Test.DDuce;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  Test.DynamicRecord in 'Test.DynamicRecord.pas',
  Test.DynamicRecord.Data in 'Test.DynamicRecord.Data.pas',
  Test.DynamicRecord.Generic in 'Test.DynamicRecord.Generic.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
  begin
    GUITestRunner.RunRegisteredTests;
  end;
end.

