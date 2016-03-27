{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I ..\Source\DDuce.inc}

unit Demo.DDuce.Logger;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ImgList,

  DDuce.Logger.Interfaces;

type
  TfrmLogger = class(TForm)
    {$REGION 'designer controls'}
    aclMain                  : TActionList;
    actAddCheckpoint         : TAction;
    actDecCounter            : TAction;
    actEnterMethod1          : TAction;
    actEnterMethod2          : TAction;
    actIncCounter            : TAction;
    actLeaveMethod1          : TAction;
    actLeaveMethod2          : TAction;
    actResetCheckpoint       : TAction;
    actResetCounter          : TAction;
    actSendClear             : TAction;
    actSendError             : TAction;
    actSendInfo              : TAction;
    actSendObject            : TAction;
    actSendTestSequence      : TAction;
    actSendWarning           : TAction;
    btnAddCheckpoint         : TButton;
    btnDecCounter            : TButton;
    btnEnterMethod1          : TButton;
    btnEnterMethod2          : TButton;
    btnExitMethod1           : TButton;
    btnExitMethod2           : TButton;
    btnIncCounter            : TButton;
    btnResetCheckpoint       : TButton;
    btnResetCounter          : TButton;
    btnSendClear             : TButton;
    btnSendError             : TButton;
    btnSendInfo              : TButton;
    btnSendObject            : TButton;
    btnSendObject1           : TButton;
    btnSendWarning           : TButton;
    chkEnableCountTimer      : TCheckBox;
    chkLogFileChannelActive  : TCheckBox;
    chkWinIPCChannelActive   : TCheckBox;
    chkZeroMQChannelActive   : TCheckBox;
    edtLogMessage            : TLabeledEdit;
    grpCounters              : TGroupBox;
    grpLoggerSettings        : TGroupBox;
    grpMethodTracing         : TGroupBox;
    grpNotificationMessages  : TGroupBox;
    grpWatches               : TGroupBox;
    lblPosition              : TLabel;
    tmrSendCounter           : TTimer;
    trbMain                  : TTrackBar;
    {$ENDREGION}

    procedure trbMainChange(Sender: TObject);
    procedure tmrSendCounterTimer(Sender: TObject);
    procedure chkEnableCountTimerClick(Sender: TObject);

    procedure actSendInfoExecute(Sender: TObject);
    procedure actSendObjectExecute(Sender: TObject);
    procedure actSendWarningExecute(Sender: TObject);
    procedure actEnterMethod1Execute(Sender: TObject);
    procedure actEnterMethod2Execute(Sender: TObject);
    procedure actLeaveMethod1Execute(Sender: TObject);
    procedure actLeaveMethod2Execute(Sender: TObject);
    procedure actSendErrorExecute(Sender: TObject);
    procedure actAddCheckpointExecute(Sender: TObject);
    procedure actIncCounterExecute(Sender: TObject);
    procedure actDecCounterExecute(Sender: TObject);
    procedure actResetCounterExecute(Sender: TObject);
    procedure actSendTestSequenceExecute(Sender: TObject);
    procedure actResetCheckpointExecute(Sender: TObject);
    procedure actSendClearExecute(Sender: TObject);


  private
    FM1Entered      : Boolean;
    FM2Entered      : Boolean;
    FLogFileChannel : ILogChannel;
    FWinIPCChannel  : ILogChannel;
    FZeroMQChannel  : ILogChannel;

    procedure TestProcedure1;
    procedure TestProcedure2;

  protected
    procedure ExecuteTestSequence;
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;
  end;

implementation

uses
  DDuce.Logger, DDuce.Logger.Channels.WinIPC,
  DDuce.Logger.Factories,
  DDuce.Logger.Channels.LogFile, DDuce.Logger.Channels.ZeroMQ,

  Demo.Data;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmLogger.AfterConstruction;
begin
  inherited AfterConstruction;
  aclMain.Images          := Data.ImageList;
  btnAddCheckpoint.Images := Data.ImageList;
  btnIncCounter.Images    := Data.ImageList;
  btnDecCounter.Images    := Data.ImageList;
  btnEnterMethod1.Images  := Data.ImageList;
  btnEnterMethod2.Images  := Data.ImageList;
  btnExitMethod1.Images   := Data.ImageList;
  btnExitMethod2.Images   := Data.ImageList;
  btnResetCounter.Images  := Data.ImageList;
  btnSendError.Images     := Data.ImageList;
  btnSendObject.Images    := Data.ImageList;
  btnSendWarning.Images   := Data.ImageList;
  btnSendInfo.Images      := Data.ImageList;

  FWinIPCChannel  := TWinIPCChannel.Create(False);
  FLogFileChannel := TLogFileChannel.Create;
  FZeroMQChannel  := TZeroMQChannel.Create(False);

  Logger.Channels.Add(FLogFileChannel);
  Logger.Channels.Add(FWinIPCChannel);
  Logger.Channels.Add(FZeroMQChannel);
end;

procedure TfrmLogger.chkEnableCountTimerClick(Sender: TObject);
begin
  tmrSendCounter.Enabled := chkEnableCountTimer.Checked;
end;

procedure TfrmLogger.TestProcedure1;
begin
  Logger.Track(Self, 'TestProcedure1');
  Logger.SendRect('Form.ClientRect', ClientRect);
  Logger.Watch('Caption', Caption);
  Logger.Send('Now', Now);
  Logger.Info('Information message.');
  Logger.Error('OMG, something went wrong over here!');
  Logger.Warn('This message warns you about nothing.');
  Logger.SendComponent('Form', Self);
  Logger.Send('Font', Font);
end;

procedure TfrmLogger.TestProcedure2;
begin
  Logger.Track(Self,'TestProcedure2');
  Logger.Warn('Warning message.');
  Logger.SendDateTime('Now', Now);
  Logger.SendShortCut('ShortCut', actSendTestSequence.ShortCut);
  Logger.SendColor('Color', clBlack);
  Logger.SendTime('Current time over here', Now);
end;

{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmLogger.actSendInfoExecute(Sender: TObject);
begin
  Logger.Info(edtLogMessage.Text);
end;

procedure TfrmLogger.actSendWarningExecute(Sender: TObject);
begin
  Logger.Warn(edtLogMessage.Text);
end;

procedure TfrmLogger.actSendClearExecute(Sender: TObject);
begin
  Logger.Clear;
end;

procedure TfrmLogger.actSendErrorExecute(Sender: TObject);
begin
  Logger.Error(edtLogMessage.Text);
end;

procedure TfrmLogger.actEnterMethod1Execute(Sender: TObject);
begin
  Logger.Enter('Method1');
  FM1Entered := True;
end;

procedure TfrmLogger.actEnterMethod2Execute(Sender: TObject);
begin
  Logger.Enter('Method2');
  FM2Entered := True;
end;

procedure TfrmLogger.actLeaveMethod1Execute(Sender: TObject);
begin
  FM1Entered := False;
  Logger.Leave('Method1');
end;

procedure TfrmLogger.actLeaveMethod2Execute(Sender: TObject);
begin
  FM2Entered := False;
  Logger.Leave('Method2');
end;

procedure TfrmLogger.actSendObjectExecute(Sender: TObject);
var
  SL : TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('Line 1');
    //SL.Add('Line 2');
    Logger.Send('SL', SL);
  finally
    SL.Free;
  end;
end;

procedure TfrmLogger.actSendTestSequenceExecute(Sender: TObject);
begin
  ExecuteTestSequence;
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
procedure TfrmLogger.tmrSendCounterTimer(Sender: TObject);
begin
  Logger.IncCounter('Counter');
end;

procedure TfrmLogger.trbMainChange(Sender: TObject);
begin
  Logger.Watch('Trackbar', trbMain.Position);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmLogger.ExecuteTestSequence;
begin
  Logger.Track(Self, 'ExecuteTestSequence');
  TestProcedure1;
  TestProcedure2;
end;

procedure TfrmLogger.UpdateActions;
begin
  inherited UpdateActions;
  actEnterMethod1.Enabled := not FM1Entered;
  actLeaveMethod1.Enabled := FM1Entered;
  actEnterMethod2.Enabled := not FM2Entered;
  actLeaveMethod2.Enabled := FM2Entered;

  FLogFileChannel.Active := chkLogFileChannelActive.Checked;
  FWinIPCChannel.Active := chkWinIPCChannelActive.Checked;
  FZeroMQChannel.Active := chkZeroMQChannelActive.Checked;
end;
{$ENDREGION}

end.
