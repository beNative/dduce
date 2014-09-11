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
  Test.DDuce.DynamicRecord in 'Test.DDuce.DynamicRecord.pas',
  Test.DDuce.DynamicRecord.Data in 'Test.DDuce.DynamicRecord.Data.pas',
  Test.DDuce.DynamicRecord.Generic in 'Test.DDuce.DynamicRecord.Generic.pas',
  Test.Registration in 'Test.Registration.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  RegisterTests;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
  begin
    GUITestRunner.RunRegisteredTests;
  end;
end.

