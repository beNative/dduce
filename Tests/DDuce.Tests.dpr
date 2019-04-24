program DDuce.Tests;

{$I Test.DDuce.inc}

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}

{$R *.dres}

uses
  FastMM4,
  {$IFDEF LEAKCHECK}
  LeakCheck,
  LeakCheck.Setup.Trace,
  LeakCheck.MapFile,
  LeakCheck.Trace.Map,
  LeakCheck.Trace.WinApi,
  DUnitX.MemoryLeakMonitor.LeakCheck,
  {$ENDIF }
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  Vcl.Forms,
  DUnitX.Loggers.GUI.VCL,
  DUnitX.Windows.Console,
  DUnitX.MemoryLeakMonitor.FastMM4,
  DUnitX.TestFramework,
  System.SysUtils,
  Test.Registration in 'Test.Registration.pas',
  Test.Data in 'Test.Data.pas',
  Test.Utils in 'Test.Utils.pas',
  Test.Resources in 'Test.Resources.pas',
  Test.DDuce.DynamicRecord in 'Test.DDuce.DynamicRecord.pas',
  Test.DDuce.DynamicRecord.Generic in 'Test.DDuce.DynamicRecord.Generic.pas',
  Test.DDuce.Reflect in 'Test.DDuce.Reflect.pas',
  Test.DDuce.Logger in 'Test.DDuce.Logger.pas',
  Test.DDuce.Mosquitto in 'Test.DDuce.Mosquitto.pas';

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := False;
  RegisterTests;
  {$IFDEF TESTINSIGHT}
  RunRegisteredTests;
  Exit;
  {$ENDIF}
  DUnitX.Loggers.GUI.VCL.Run;
end.

