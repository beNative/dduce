{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit Demo.DDuce.Logger;

{ This form demonstrates the methods of the ILogger interface and its support
  for various channels. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions, System.Rtti,
  System.ImageList, System.Bindings.Outputs,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ImgList, Vcl.Bind.DBEngExt,
  Vcl.Bind.Editors, Vcl.Menus,
  Data.Bind.EngExt, Data.Bind.Components,

  DDuce.Logger.Interfaces;

type
  TfrmLogger = class(TForm)
    {$REGION 'designer controls'}
    aclMain                   : TActionList;
    actAddCheckpoint          : TAction;
    actDecCounter             : TAction;
    actEnterMethod1           : TAction;
    actEnterMethod2           : TAction;
    actIncCounter             : TAction;
    actLeaveMethod1           : TAction;
    actLeaveMethod2           : TAction;
    actResetCheckpoint        : TAction;
    actResetCounter           : TAction;
    actSendBitmap             : TAction;
    actSendClear              : TAction;
    actSendComponent          : TAction;
    actSendDataSet            : TAction;
    actSendError              : TAction;
    actSendInfo               : TAction;
    actSendInterface          : TAction;
    actSendMessages           : TAction;
    actSendObject             : TAction;
    actSendODS                : TAction;
    actSendPersistent         : TAction;
    actSendPoint              : TAction;
    actSendRecord             : TAction;
    actSendRect               : TAction;
    actSendScreenshot         : TAction;
    actSendSQL                : TAction;
    actSendStrings            : TAction;
    actSendTestSequence       : TAction;
    actSendText               : TAction;
    actSendWarning            : TAction;
    actZMQBind                : TAction;
    actZMQBindToDefaultPort   : TAction;
    actZMQBindToEphemeralPort : TAction;
    actZMQCloseSocket         : TAction;
    Bind1                     : TMenuItem;
    Bindtodefaultport55551    : TMenuItem;
    Bindtoephemeralport1      : TMenuItem;
    btnAddCheckpoint          : TButton;
    btnDecCounter             : TButton;
    btnEnterMethod1           : TButton;
    btnEnterMethod2           : TButton;
    btnExitMethod1            : TButton;
    btnExitMethod2            : TButton;
    btnIncCounter             : TButton;
    btnResetCheckpoint        : TButton;
    btnResetCounter           : TButton;
    btnSendBitmap             : TButton;
    btnSendClear              : TButton;
    btnSendComponent          : TButton;
    btnSendDataSet            : TButton;
    btnSendError              : TButton;
    btnSendInfo               : TButton;
    btnSendInterface          : TButton;
    btnSendMessages           : TButton;
    btnSendObject             : TButton;
    btnSendObject1            : TButton;
    btnSendODS                : TButton;
    btnSendPersistent         : TButton;
    btnSendPoint              : TButton;
    btnSendRecord             : TButton;
    btnSendRect               : TButton;
    btnSendSQL                : TButton;
    btnSendStrings            : TButton;
    btnSendText               : TButton;
    btnSendWarning            : TButton;
    btnZMQBind                : TButton;
    btnZMQCloseSocket         : TButton;
    chkActions                : TCheckBox;
    chkEnableCountTimer       : TCheckBox;
    chkLogFileChannel         : TCheckBox;
    chkSendRandomValueTimer   : TCheckBox;
    chkWinIPCChannel          : TCheckBox;
    chkZeroMQChannel          : TCheckBox;
    edtEndPoint               : TLabeledEdit;
    edtLogFile                : TButtonedEdit;
    edtMessageCount           : TLabeledEdit;
    edtMethod1                : TLabeledEdit;
    edtMethod2                : TLabeledEdit;
    grpActions                : TGroupBox;
    grpCheckpoints            : TGroupBox;
    grpCounters               : TGroupBox;
    grpCustom                 : TGroupBox;
    grpLoggerSettings         : TGroupBox;
    grpMethodTracing          : TGroupBox;
    grpNotificationMessages   : TGroupBox;
    grpValues                 : TGroupBox;
    grpWatches                : TGroupBox;
    imlLogger                 : TImageList;
    lblCheckpointDescription  : TLabel;
    lblCounterValue           : TLabel;
    lblIPAddress              : TLabel;
    lblIPCaption              : TLabel;
    lblLogViewer              : TLabel;
    lblPort                   : TLabel;
    lblPortCaption            : TLabel;
    lblPosition               : TLabel;
    lblPositionValue          : TLabel;
    ppmBind                   : TPopupMenu;
    tmrSendCounter            : TTimer;
    tmrSendValue              : TTimer;
    trbMain                   : TTrackBar;
    chkMQTTChannel            : TCheckBox;
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure chkEnableCountTimerClick(Sender: TObject);
    procedure chkLogFileChannelClick(Sender: TObject);
    procedure chkSendRandomValueTimerClick(Sender: TObject);
    procedure chkWinIPCChannelClick(Sender: TObject);
    procedure chkZeroMQChannelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure tmrSendCounterTimer(Sender: TObject);
    procedure tmrSendValueTimer(Sender: TObject);
    procedure trbMainChange(Sender: TObject);
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actAddCheckpointExecute(Sender: TObject);
    procedure actDecCounterExecute(Sender: TObject);
    procedure actEnterMethod1Execute(Sender: TObject);
    procedure actEnterMethod2Execute(Sender: TObject);
    procedure actIncCounterExecute(Sender: TObject);
    procedure actLeaveMethod1Execute(Sender: TObject);
    procedure actLeaveMethod2Execute(Sender: TObject);
    procedure actResetCheckpointExecute(Sender: TObject);
    procedure actResetCounterExecute(Sender: TObject);
    procedure actSendBitmapExecute(Sender: TObject);
    procedure actSendClearExecute(Sender: TObject);
    procedure actSendComponentExecute(Sender: TObject);
    procedure actSendErrorExecute(Sender: TObject);
    procedure actSendInfoExecute(Sender: TObject);
    procedure actSendObjectExecute(Sender: TObject);
    procedure actSendODSExecute(Sender: TObject);
    procedure actSendRecordExecute(Sender: TObject);
    procedure actSendStringsExecute(Sender: TObject);
    procedure actSendTestSequenceExecute(Sender: TObject);
    procedure actSendWarningExecute(Sender: TObject);
    procedure actSendRectExecute(Sender: TObject);
    procedure actSendPointExecute(Sender: TObject);
    procedure actSendInterfaceExecute(Sender: TObject);
    procedure actSendTextExecute(Sender: TObject);
    procedure actSendPersistentExecute(Sender: TObject);
    procedure actSendMessagesExecute(Sender: TObject);
    procedure actSendSQLExecute(Sender: TObject);
    procedure actZMQBindExecute(Sender: TObject);
    procedure actSendDataSetExecute(Sender: TObject);
    procedure aclMainExecute(Action: TBasicAction; var Handled: Boolean);
    procedure actZMQBindToEphemeralPortExecute(Sender: TObject);
    procedure actZMQCloseSocketExecute(Sender: TObject);
    procedure actZMQBindToDefaultPortExecute(Sender: TObject);
    {$ENDREGION}

  private
    FM1Entered      : Boolean;
    FM2Entered      : Boolean;
    FLogFileChannel : ILogFileChannel;
    FWinIPCChannel  : IWinIPCChannel;
    FZeroMQChannel  : IZeroMQChannel;
    FMQTTChannel    : IMQTTChannel;
    FLogger         : ILogger;

    procedure LoadSettings;
    procedure SaveSettings;

    procedure WatchZeroMQChannel;

  protected
    procedure TestProcedure1;
    procedure TestProcedure2;
    procedure UpdateActions; override;

    property Logger: ILogger
      read FLogger;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

uses
  System.Types,
  Data.DB,

  Spring,

  DDuce.Utils.Winapi,
  DDuce.Logger.Factories,
  DDuce.Logger.Channels.WinIPC, DDuce.Logger.Channels.LogFile,
  DDuce.Logger.Channels.ZeroMQ, DDuce.Logger.Channels.MQTT,

  Demo.Data, Demo.Resources, Demo.Settings, Demo.Factories;

{$R *.dfm}

resourcestring
  SEnter   = 'Enter %s';
  SLeave   = 'Leave %s';
  SVersion = 'ZeroMQ version %s';

{$REGION 'non-interfaced routines'}
procedure EnsureZMQLibExists;
const
  LIBZMQ = 'libzmq';
var
  LResStream  : TResourceStream;
  LFileStream : TFileStream;
  LPath       : string;
begin
  LPath := Format('%s\%s.dll', [ExtractFileDir(ParamStr(0)), LIBZMQ]);
  if not FileExists(LPath) then
  begin
    LResStream := TResourceStream.Create(HInstance, LIBZMQ, RT_RCDATA);
    try
      LFileStream := TFileStream.Create(LPath, fmCreate);
      try
        LFileStream.CopyFrom(LResStream, 0);
      finally
        LFileStream.Free;
      end;
    finally
      LResStream.Free;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TfrmLogger.AfterConstruction;
var
  SL : TStringList;
begin
  inherited AfterConstruction;
  aclMain.Images          := imlLogger;
  btnAddCheckpoint.Images := imlLogger;
  btnIncCounter.Images    := imlLogger;
  btnDecCounter.Images    := imlLogger;
  btnEnterMethod1.Images  := imlLogger;
  btnEnterMethod2.Images  := imlLogger;
  btnExitMethod1.Images   := imlLogger;
  btnExitMethod2.Images   := imlLogger;
  btnResetCounter.Images  := imlLogger;
  btnSendError.Images     := imlLogger;
  btnSendObject.Images    := imlLogger;
  btnSendWarning.Images   := imlLogger;
  btnSendInfo.Images      := imlLogger;

  FLogger := TLoggerFactories.CreateLogger;
  LoadSettings;
  FWinIPCChannel  := TWinIPCChannel.Create(False);
  FLogFileChannel := TLogFileChannel.Create(edtLogFile.Text);
  FZeroMQChannel  := TZeroMQChannel.Create(False);
  FMQTTChannel    := TMQTTChannel.Create('192.168.0.219');
//  FMQTTChannel    := TMQTTChannel.Create('localhost');
//  FMQTTChannel.Enabled := True;

  chkZeroMQChannel.Hint := Format(SVersion, [FZeroMQChannel.ZMQVersion]);

  Logger.Channels.Add(FLogFileChannel);
  Logger.Channels.Add(FWinIPCChannel);
  Logger.Channels.Add(FZeroMQChannel);
  Logger.Channels.Add(FMQTTChannel);
  Randomize;
  edtLogFile.Text := FLogFileChannel.FileName;

  SL := TStringList.Create;
  try
    GetIPAddresses(SL);
    if SL.Count > 0 then
      lblIPAddress.Caption := SL[0];
  finally
    SL.Free;
  end;
end;

procedure TfrmLogger.BeforeDestruction;
begin
  SaveSettings;
  FLogFileChannel.Enabled := False;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmLogger.actSendInfoExecute(Sender: TObject);
begin
  Logger.Info('Info message');
end;

procedure TfrmLogger.actSendInterfaceExecute(Sender: TObject);
begin
  Logger.SendInterface('TestInterface', Logger);
end;

procedure TfrmLogger.actSendMessagesExecute(Sender: TObject);
var
  N : Integer;
  I : Integer;
begin
  N := StrToIntDef(edtMessageCount.Text, 0);
  for I := 1 to N do
  begin
    Logger.Send('I', I);
  end;
end;

procedure TfrmLogger.actSendWarningExecute(Sender: TObject);
begin
  Logger.Warn('Warning message');
end;

procedure TfrmLogger.actZMQBindExecute(Sender: TObject);
begin
  FZeroMQChannel.EndPoint := edtEndPoint.Text;
  if FZeroMQChannel.Connect then
  begin
    lblPort.Caption := FZeroMQChannel.Port.ToString;
    edtEndPoint.Text := FZeroMQChannel.EndPoint;
  end;
  WatchZeroMQChannel;
end;

procedure TfrmLogger.actZMQBindToDefaultPortExecute(Sender: TObject);
begin
  FZeroMQChannel.EndPoint := 'tcp://*:5555';
  if FZeroMQChannel.Connect then
  begin
    lblPort.Caption := FZeroMQChannel.Port.ToString;
    edtEndPoint.Text := FZeroMQChannel.EndPoint;
  end;
  WatchZeroMQChannel;
end;

procedure TfrmLogger.actZMQBindToEphemeralPortExecute(Sender: TObject);
begin
  FZeroMQChannel.EndPoint := 'tcp://*:*';
  if FZeroMQChannel.Connect then
  begin
    lblPort.Caption := FZeroMQChannel.Port.ToString;
    edtEndPoint.Text := FZeroMQChannel.EndPoint;
  end;
  WatchZeroMQChannel;
end;

procedure TfrmLogger.actZMQCloseSocketExecute(Sender: TObject);
begin
  FZeroMQChannel.Disconnect;
  WatchZeroMQChannel;
end;

procedure TfrmLogger.actSendBitmapExecute(Sender: TObject);
var
  B : TBitmap;
begin
  B := GetFormImage;
  try
    Logger.SendBitmap('Bitmap', B);
  finally
    B.Free;
  end;
end;

procedure TfrmLogger.actSendClearExecute(Sender: TObject);
begin
  Logger.Clear;
end;

procedure TfrmLogger.actSendComponentExecute(Sender: TObject);
begin
  Logger.SendComponent(Self.Name, Self);
end;

procedure TfrmLogger.actSendDataSetExecute(Sender: TObject);
var
  LDataSet : TDataSet;
begin
  LDataSet := TDemoFactories.CreateContactDataSet(nil, 100);
  try
   Logger.SendDataSet('DataSet', LDataSet);
  finally
    LDataSet.Free;
  end;
end;

procedure TfrmLogger.actSendErrorExecute(Sender: TObject);
begin
  Logger.Error('Error message');
end;

procedure TfrmLogger.actEnterMethod1Execute(Sender: TObject);
begin
  Logger.Enter(edtMethod1.Text);
  FM1Entered := True;
end;

procedure TfrmLogger.actEnterMethod2Execute(Sender: TObject);
begin
  Logger.Enter(edtMethod2.Text);
  FM2Entered := True;
end;

procedure TfrmLogger.actLeaveMethod1Execute(Sender: TObject);
begin
  FM1Entered := False;
  Logger.Leave(edtMethod1.Text);
end;

procedure TfrmLogger.actLeaveMethod2Execute(Sender: TObject);
begin
  FM2Entered := False;
  Logger.Leave(edtMethod2.Text);
end;

procedure TfrmLogger.actSendObjectExecute(Sender: TObject);
var
  F : TFont;
begin
  F := TFont.Create;
  try
    Logger.SendObject('Object', F);
  finally
    F.Free;
  end;
end;

procedure TfrmLogger.actSendODSExecute(Sender: TObject);
begin
  OutputDebugString(LOREM_IPSUM);
end;

procedure TfrmLogger.actSendPersistentExecute(Sender: TObject);
begin
  Logger.SendPersistent('Persistent', Self);
end;

procedure TfrmLogger.actSendPointExecute(Sender: TObject);
begin
  Logger.SendPoint('Point', Point(Left, Top));
end;

procedure TfrmLogger.actSendRecordExecute(Sender: TObject);
begin
  Logger.Send('ClientRect', TValue.From(ClientRect));
end;

procedure TfrmLogger.actSendRectExecute(Sender: TObject);
var
  R : TRect;
begin
  R := BoundsRect;
  Logger.SendRect('BoundsRect', R);
end;

procedure TfrmLogger.actSendSQLExecute(Sender: TObject);
begin
  Logger.SendText('SQL', EXAMPLE_SQL, 'SQL');
end;

procedure TfrmLogger.actSendStringsExecute(Sender: TObject);
var
  SL : TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('This');
    SL.Add('is');
    SL.Add('a');
    SL.Add('test');
    Logger.SendStrings('SL', SL);
  finally
    SL.Free;
  end;
end;

procedure TfrmLogger.actSendTestSequenceExecute(Sender: TObject);
var
  I : Integer;
begin
  for I := 0 to 10 do
  begin
    Logger.Send('I', I);
    Logger.Watch('I', I);
    TestProcedure1;
//    TestProcedure2;
  end;
end;

procedure TfrmLogger.actSendTextExecute(Sender: TObject);
begin
  Logger.SendText(LOREM_IPSUM);
end;

procedure TfrmLogger.aclMainExecute(Action: TBasicAction; var Handled: Boolean);
begin
  if chkActions.Checked then
    Logger.Action(Action);
end;

procedure TfrmLogger.actAddCheckpointExecute(Sender: TObject);
begin
  Logger.AddCheckPoint;
end;

procedure TfrmLogger.actIncCounterExecute(Sender: TObject);
begin
  Logger.IncCounter('Counter');
end;

procedure TfrmLogger.actDecCounterExecute(Sender: TObject);
begin
  Logger.DecCounter('Counter');
end;

procedure TfrmLogger.actResetCheckpointExecute(Sender: TObject);
begin
  Logger.ResetCheckPoint;
end;

procedure TfrmLogger.actResetCounterExecute(Sender: TObject);
begin
  Logger.ResetCounter('Counter');
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmLogger.chkEnableCountTimerClick(Sender: TObject);
begin
  tmrSendCounter.Enabled := chkEnableCountTimer.Checked;
end;

procedure TfrmLogger.chkLogFileChannelClick(Sender: TObject);
begin
  if Assigned(FLogFileChannel) then
  begin
    FLogFileChannel.Enabled := (Sender as TCheckBox).Checked;
    FLogFileChannel.FileName := edtLogFile.Text;
  end;
end;

procedure TfrmLogger.chkSendRandomValueTimerClick(Sender: TObject);
begin
  tmrSendValue.Enabled := chkSendRandomValueTimer.Checked;
end;

procedure TfrmLogger.chkWinIPCChannelClick(Sender: TObject);
begin
  FWinIPCChannel.Enabled := (Sender as TCheckBox).Checked;
end;

procedure TfrmLogger.chkZeroMQChannelClick(Sender: TObject);
begin
  if Assigned(FZeroMQChannel) then
  begin
    FZeroMQChannel.Enabled := (Sender as TCheckBox).Checked;
    UpdateActions;
  end;
end;

procedure TfrmLogger.tmrSendCounterTimer(Sender: TObject);
begin
  Logger.IncCounter('Counter');
  WatchZeroMQChannel;
end;

procedure TfrmLogger.tmrSendValueTimer(Sender: TObject);
begin
  Logger.Send('RandomValue', Random(1000));
  WatchZeroMQChannel;
end;

procedure TfrmLogger.trbMainChange(Sender: TObject);
begin
  Logger.Watch('Trackbar', trbMain.Position);
end;

procedure TfrmLogger.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmLogger.FormShow(Sender: TObject);
begin
  Caption := Format('%s (%d)', [Application.ExeName, GetCurrentProcessId]);
  WatchZeroMQChannel;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmLogger.LoadSettings;
begin
  edtLogFile.Text := Settings.ReadString(UnitName, 'LogFileChannel.FileName');
  chkLogFileChannel.Checked := Settings.ReadBool(UnitName, 'LogFileChannel.Enabled');
  chkWinIPCChannel.Checked := Settings.ReadBool(UnitName, 'WinIPCChannel.Enabled');
  chkZeroMQChannel.Checked := Settings.ReadBool(UnitName, 'ZeroMQChannel.Enabled');
  //chkMQTTChannel.Checked := Settings.ReadBool(UnitName, 'MQTTChannel.Enabled');
  edtMethod1.Text := Settings.ReadString(
    UnitName, 'edtMethod1.Text', 'MyObject.Execute'
  );
  edtMethod2.Text := Settings.ReadString(
    UnitName, 'edtMethod2.Text', 'MyObject.Update'
  );
  edtMessageCount.Text := IntToStr(Settings.ReadInteger(UnitName, 'MessageCount'));
  edtEndPoint.Text := Settings.ReadString(UnitName, 'EndPoint');
  chkActions.Checked := Settings.ReadBool(UnitName, 'LogActions');
end;

procedure TfrmLogger.SaveSettings;
begin
  Settings.WriteBool(UnitName, 'LogFileChannel.Enabled', FLogFileChannel.Enabled);
  Settings.WriteString(UnitName, 'LogFileChannel.FileName', FLogFileChannel.FileName);
  Settings.WriteBool(UnitName, 'WinIPCChannel.Enabled', FWinIPCChannel.Enabled);
  Settings.WriteBool(UnitName, 'ZeroMQChannel.Enabled', FZeroMQChannel.Enabled);
  Settings.WriteBool(UnitName, 'MQTTChannel.Enabled', FMQTTChannel.Enabled);
  Settings.WriteString(UnitName, 'edtMethod1.Text', edtMethod1.Text);
  Settings.WriteString(UnitName, 'edtMethod2.Text', edtMethod2.Text);
  Settings.WriteInteger(UnitName, 'MessageCount', StrToIntDef(edtMessageCount.Text, 0));
  Settings.WriteString(UnitName, 'EndPoint', edtEndPoint.Text);
  Settings.WriteBool(UnitName, 'LogActions', chkActions.Checked);
end;

procedure TfrmLogger.TestProcedure1;
begin
  Logger.Track(Self, 'TestProcedure1');
  Logger.SendRect('Form.ClientRect', ClientRect);
  Logger.Watch('Caption', Caption);
  Logger.SendTime('Now', Now);
  Logger.Info('Information message.');
  Logger.Error('Fatal error occured! Something went wrong over here!');
  Logger.Warn('This message warns you about nothing.');
//  Logger.SendComponent('Form', Self);  // sends DFM with published properties => ERROR!!!
  Logger.SendPersistent('Font', Font); // sends published property values
  Logger.SendObject('Font', Font);     // sends fields and property values
end;

procedure TfrmLogger.TestProcedure2;
begin
  Logger.Track(Self,'TestProcedure2');
  Logger.Warn('Warning message.');
  Logger.SendDateTime('Now', Now);
  Logger.SendShortCut('ShortCut', actSendTestSequence.ShortCut);
  Logger.SendColor('Color', clBlack);
  Logger.SendTime('CurrentTime', Now);
end;

procedure TfrmLogger.WatchZeroMQChannel;
begin
  Logger.Watch('FZeroMQChannel.Enabled', FZeroMQChannel.Enabled);
  Logger.Watch('FZeroMQChannel.Connected', FZeroMQChannel.Connected);
  Logger.Watch('FZeroMQChannel.Port', FZeroMQChannel.Port);
  Logger.Watch('FZeroMQChannel.EndPoint', FZeroMQChannel.EndPoint);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmLogger.UpdateActions;
var
  B : Boolean;
begin
  inherited UpdateActions;
  actEnterMethod1.Caption := Format(SEnter, [edtMethod1.Text]);
  actEnterMethod2.Caption := Format(SEnter, [edtMethod2.Text]);
  actLeaveMethod1.Caption := Format(SLeave, [edtMethod1.Text]);
  actLeaveMethod2.Caption := Format(SLeave, [edtMethod2.Text]);
  actEnterMethod1.Enabled := not FM1Entered;
  actLeaveMethod1.Enabled := FM1Entered;
  actEnterMethod2.Enabled := not FM2Entered;
  actLeaveMethod2.Enabled := FM2Entered;
  chkLogFileChannel.Checked := FLogFileChannel.Enabled;
  chkWinIPCChannel.Checked  := FWinIPCChannel.Enabled;
  lblCounterValue.Caption  := Logger.GetCounter('Counter').ToString;
  lblPositionValue.Caption := trbMain.Position.ToString;
  actSendMessages.Caption  := Format('Send %s messages', [edtMessageCount.Text]);

  B := chkZeroMQChannel.Checked;
  edtEndPoint.Enabled := B;
  actZMQBind.Enabled := B and not FZeroMQChannel.Connected;
  actZMQBindToEphemeralPort.Enabled := B and not FZeroMQChannel.Connected;
  actZMQCloseSocket.Enabled := B and FZeroMQChannel.Connected;
  lblPort.Caption := FZeroMQChannel.Port.ToString;
end;
{$ENDREGION}

initialization
  EnsureZMQLibExists;

end.
