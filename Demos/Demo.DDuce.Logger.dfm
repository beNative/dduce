object frmLogger: TfrmLogger
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Logger'
  ClientHeight = 275
  ClientWidth = 508
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnSendObject: TButton
    Left = 375
    Top = 8
    Width = 120
    Height = 25
    Action = actSendObject
    TabOrder = 0
  end
  object grpWatches: TGroupBox
    Left = 8
    Top = 198
    Width = 487
    Height = 59
    Caption = 'Watch variables'
    TabOrder = 1
    DesignSize = (
      487
      59)
    object lblPosition: TLabel
      Left = 12
      Top = 30
      Width = 41
      Height = 13
      Caption = 'Position:'
    end
    object trbMain: TTrackBar
      Left = 68
      Top = 25
      Width = 416
      Height = 29
      Anchors = [akLeft, akTop, akRight]
      Max = 100
      PageSize = 10
      PositionToolTip = ptTop
      TabOrder = 0
      OnChange = trbMainChange
    end
  end
  object grpMethodTracing: TGroupBox
    Left = 8
    Top = 103
    Width = 280
    Height = 89
    Caption = 'Method tracing'
    TabOrder = 2
    object btnEnterMethod1: TButton
      Left = 21
      Top = 22
      Width = 122
      Height = 25
      Action = actEnterMethod1
      ImageMargins.Left = 2
      TabOrder = 0
    end
    object btnEnterMethod2: TButton
      Left = 21
      Top = 53
      Width = 122
      Height = 25
      Action = actEnterMethod2
      ImageMargins.Left = 2
      TabOrder = 1
    end
    object btnExitMethod1: TButton
      Left = 149
      Top = 22
      Width = 124
      Height = 25
      Action = actLeaveMethod1
      ImageMargins.Left = 2
      TabOrder = 2
    end
    object btnExitMethod2: TButton
      Left = 149
      Top = 53
      Width = 124
      Height = 25
      Action = actLeaveMethod2
      ImageMargins.Left = 2
      TabOrder = 3
    end
  end
  object grpNotificationMessages: TGroupBox
    Left = 8
    Top = 8
    Width = 361
    Height = 89
    Caption = 'Notification messages'
    TabOrder = 3
    DesignSize = (
      361
      89)
    object edtLogMessage: TLabeledEdit
      Left = 56
      Top = 26
      Width = 297
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 46
      EditLabel.Height = 13
      EditLabel.Caption = 'Message:'
      LabelPosition = lpLeft
      TabOrder = 0
      Text = 'This message will be sent to the Logger'
    end
    object btnSendInfo: TButton
      Left = 3
      Top = 53
      Width = 114
      Height = 25
      Action = actSendInfo
      ImageMargins.Left = 2
      TabOrder = 1
    end
    object btnSendWarning: TButton
      Left = 123
      Top = 53
      Width = 114
      Height = 25
      Action = actSendWarning
      ImageMargins.Left = 2
      TabOrder = 2
    end
    object btnSendError: TButton
      Left = 243
      Top = 53
      Width = 114
      Height = 25
      Action = actSendError
      ImageMargins.Left = 2
      TabOrder = 3
    end
  end
  object btnAddCheckpoint: TButton
    Left = 375
    Top = 39
    Width = 120
    Height = 25
    Action = actAddCheckpoint
    ImageMargins.Left = 3
    TabOrder = 4
  end
  object grpCounters: TGroupBox
    Left = 294
    Top = 103
    Width = 204
    Height = 89
    Caption = 'Counters'
    TabOrder = 5
    object btnIncCounter: TButton
      Left = 11
      Top = 22
      Width = 92
      Height = 25
      Action = actIncCounter
      TabOrder = 0
    end
    object btnResetCounter: TButton
      Left = 11
      Top = 53
      Width = 184
      Height = 25
      Action = actResetCounter
      TabOrder = 1
    end
    object btnDecCounter: TButton
      Left = 110
      Top = 22
      Width = 85
      Height = 25
      Action = actDecCounter
      TabOrder = 2
    end
  end
  object aclMain: TActionList
    Left = 416
    Top = 64
    object actSendInfo: TAction
      Caption = 'Send Info'
      ImageIndex = 486
      OnExecute = actSendInfoExecute
    end
    object actSendObject: TAction
      Caption = 'Send object'
      ImageIndex = 105
      OnExecute = actSendObjectExecute
    end
    object actSendWarning: TAction
      Caption = 'Send Warning'
      ImageIndex = 392
      OnExecute = actSendWarningExecute
    end
    object actEnterMethod1: TAction
      Caption = 'Enter Method1'
      ImageIndex = 52
      OnExecute = actEnterMethod1Execute
    end
    object actLeaveMethod1: TAction
      Caption = 'Leave Method1'
      ImageIndex = 46
      OnExecute = actLeaveMethod1Execute
    end
    object actEnterMethod2: TAction
      Caption = 'Enter Method2'
      ImageIndex = 52
      OnExecute = actEnterMethod2Execute
    end
    object actLeaveMethod2: TAction
      Caption = 'Leave Method2'
      ImageIndex = 46
      OnExecute = actLeaveMethod2Execute
    end
    object actSendError: TAction
      Caption = 'Send Error'
      ImageIndex = 177
      OnExecute = actSendErrorExecute
    end
    object actAddCheckpoint: TAction
      Caption = 'Add Checkpoint'
      ImageIndex = 0
      OnExecute = actAddCheckpointExecute
    end
    object actIncCounter: TAction
      Caption = 'Increment'
      ImageIndex = 1
      OnExecute = actIncCounterExecute
    end
    object actDecCounter: TAction
      Caption = 'Decrement'
      ImageIndex = 334
      OnExecute = actDecCounterExecute
    end
    object actResetCounter: TAction
      Caption = 'Reset'
      ImageIndex = 281
      OnExecute = actResetCounterExecute
    end
  end
end
