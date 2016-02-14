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
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ImgList;

type
  TfrmLogger = class(TForm)
    aclMain                 : TActionList;
    actAddCheckpoint        : TAction;
    actDecCounter           : TAction;
    actEnterMethod1         : TAction;
    actEnterMethod2         : TAction;
    actLeaveMethod1         : TAction;
    actLeaveMethod2         : TAction;
    actIncCounter           : TAction;
    actResetCounter         : TAction;
    actSendError            : TAction;
    actSendInfo             : TAction;
    actSendObject           : TAction;
    actSendWarning          : TAction;
    btnAddCheckpoint        : TButton;
    btnDecCounter           : TButton;
    btnEnterMethod1         : TButton;
    btnEnterMethod2         : TButton;
    btnExitMethod1          : TButton;
    btnExitMethod2          : TButton;
    btnIncCounter           : TButton;
    btnResetCounter         : TButton;
    btnSendError            : TButton;
    btnSendInfo             : TButton;
    btnSendObject           : TButton;
    btnSendWarning          : TButton;
    edtLogMessage           : TLabeledEdit;
    grpCounters             : TGroupBox;
    grpMethodTracing        : TGroupBox;
    grpNotificationMessages : TGroupBox;
    grpWatches              : TGroupBox;
    trbMain                 : TTrackBar;
    lblPosition             : TLabel;

    procedure trbMainChange(Sender: TObject);

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

  private
    FM1Entered : Boolean;
    FM2Entered : Boolean;

  protected

    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;
  end;

implementation

uses
  DDuce.Logger,

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
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmLogger.actSendInfoExecute(Sender: TObject);
begin
  Logger.SendInfo(edtLogMessage.Text);
end;

procedure TfrmLogger.actSendWarningExecute(Sender: TObject);
begin
  Logger.SendWarning(edtLogMessage.Text);
end;

procedure TfrmLogger.actSendErrorExecute(Sender: TObject);
begin
  Logger.SendError(edtLogMessage.Text);
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
  SL.Add('Line 1');
  SL.Add('Line 2');
  Logger.Send('SL', SL);
  SL.Free;
end;

procedure TfrmLogger.actAddCheckpointExecute(Sender: TObject);
begin
  Logger.AddCheckPoint('Checkpont');
end;

procedure TfrmLogger.actIncCounterExecute(Sender: TObject);
begin
  Logger.IncCounter('Counter');
end;

procedure TfrmLogger.actDecCounterExecute(Sender: TObject);
begin
  Logger.DecCounter('Counter');
end;

procedure TfrmLogger.actResetCounterExecute(Sender: TObject);
begin
  Logger.ResetCounter('Counter');
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmLogger.trbMainChange(Sender: TObject);
begin
  Logger.Watch('Trackbar', trbMain.Position);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmLogger.UpdateActions;
begin
  inherited UpdateActions;
  actEnterMethod1.Enabled := not FM1Entered;
  actLeaveMethod1.Enabled := FM1Entered;
  actEnterMethod2.Enabled := not FM2Entered;
  actLeaveMethod2.Enabled := FM2Entered;
end;
{$ENDREGION}

end.
