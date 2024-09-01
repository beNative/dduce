object frmLogger: TfrmLogger
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Logger'
  ClientHeight = 1076
  ClientWidth = 973
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  PopupMode = pmAuto
  Position = poMainFormCenter
  ShowHint = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 144
  TextHeight = 23
  object grpWatches: TGroupBox
    AlignWithMargins = True
    Left = 5
    Top = 874
    Width = 963
    Height = 75
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Caption = 'Watches'
    TabOrder = 0
    ExplicitWidth = 962
    DesignSize = (
      963
      75)
    object lblPosition: TLabel
      Left = 18
      Top = 41
      Width = 64
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Caption = 'Position:'
    end
    object lblPositionValue: TLabel
      Left = 89
      Top = 20
      Width = 79
      Height = 45
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Alignment = taCenter
      AutoSize = False
      Caption = '60'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -32
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Transparent = False
      Layout = tlCenter
    end
    object trbMain: TTrackBar
      Left = 179
      Top = 23
      Width = 535
      Height = 43
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akLeft, akTop, akRight]
      Max = 100
      PageSize = 10
      Frequency = 5
      Position = 50
      PositionToolTip = ptTop
      ShowSelRange = False
      TabOrder = 0
      ThumbLength = 30
      OnChange = trbMainChange
      ExplicitWidth = 534
    end
    object chkSendRandomValueTimer: TCheckBox
      Left = 715
      Top = 29
      Width = 231
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Send random value timer'
      TabOrder = 1
      OnClick = chkSendRandomValueTimerClick
    end
  end
  object grpMethodTracing: TGroupBox
    AlignWithMargins = True
    Left = 5
    Top = 395
    Width = 963
    Height = 127
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Caption = 'Method tracing'
    TabOrder = 1
    ExplicitWidth = 962
    object btnEnterMethod1: TButton
      Left = 18
      Top = 33
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actEnterMethod1
      ImageMargins.Left = 3
      TabOrder = 0
    end
    object btnEnterMethod2: TButton
      Left = 18
      Top = 77
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actEnterMethod2
      ImageMargins.Left = 3
      TabOrder = 3
    end
    object btnExitMethod1: TButton
      Left = 720
      Top = 30
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actLeaveMethod1
      ImageName = 'Item8'
      ImageMargins.Left = 3
      Images = imlLogger
      TabOrder = 2
    end
    object btnExitMethod2: TButton
      Left = 720
      Top = 77
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actLeaveMethod2
      ImageName = 'Item8'
      ImageMargins.Left = 3
      Images = imlLogger
      TabOrder = 5
    end
    object edtMethod1: TLabeledEdit
      Left = 371
      Top = 36
      Width = 342
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      EditLabel.Width = 121
      EditLabel.Height = 31
      EditLabel.Margins.Left = 5
      EditLabel.Margins.Top = 5
      EditLabel.Margins.Right = 5
      EditLabel.Margins.Bottom = 5
      EditLabel.Caption = 'Method1 name:'
      LabelPosition = lpLeft
      TabOrder = 1
      Text = 'MyObject.Execute'
    end
    object edtMethod2: TLabeledEdit
      Left = 371
      Top = 80
      Width = 342
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      EditLabel.Width = 121
      EditLabel.Height = 31
      EditLabel.Margins.Left = 5
      EditLabel.Margins.Top = 5
      EditLabel.Margins.Right = 5
      EditLabel.Margins.Bottom = 5
      EditLabel.Caption = 'Method2 name:'
      LabelPosition = lpLeft
      TabOrder = 4
      Text = 'MyObject.Update'
    end
  end
  object grpNotificationMessages: TGroupBox
    AlignWithMargins = True
    Left = 5
    Top = 298
    Width = 963
    Height = 87
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Caption = 'Notification messages'
    TabOrder = 2
    ExplicitWidth = 962
    object btnSendInfo: TButton
      Left = 26
      Top = 32
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendInfo
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageName = 'Item4'
      ImageMargins.Left = 3
      Images = imlLogger
      ParentFont = False
      TabOrder = 0
    end
    object btnSendWarning: TButton
      Left = 260
      Top = 32
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendWarning
      ImageName = 'Item2'
      ImageMargins.Left = 3
      Images = imlLogger
      TabOrder = 1
    end
    object btnSendError: TButton
      Left = 495
      Top = 32
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendError
      ImageName = 'Item3'
      ImageMargins.Left = 3
      Images = imlLogger
      TabOrder = 2
    end
    object btnSendClear: TButton
      Left = 729
      Top = 32
      Width = 216
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendClear
      ImageName = 'Item6'
      Images = imlLogger
      TabOrder = 3
    end
  end
  object grpCounters: TGroupBox
    AlignWithMargins = True
    Left = 5
    Top = 532
    Width = 963
    Height = 87
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Caption = 'Counters'
    TabOrder = 3
    ExplicitWidth = 962
    object lblCounterValue: TLabel
      Left = 128
      Top = 27
      Width = 115
      Height = 45
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Alignment = taCenter
      AutoSize = False
      Caption = '0'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -32
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Transparent = False
      Layout = tlCenter
    end
    object btnIncCounter: TButton
      Left = 252
      Top = 33
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actIncCounter
      TabOrder = 1
    end
    object btnResetCounter: TButton
      Left = 720
      Top = 32
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actResetCounter
      TabOrder = 3
    end
    object btnDecCounter: TButton
      Left = 486
      Top = 33
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actDecCounter
      TabOrder = 2
    end
    object chkEnableCountTimer: TCheckBox
      Left = 18
      Top = 24
      Width = 108
      Height = 59
      Hint = 'Increases the counter automatically each second.'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Enable count timer'
      TabOrder = 0
      WordWrap = True
      OnClick = chkEnableCountTimerClick
    end
  end
  object grpLoggerSettings: TGroupBox
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 963
    Height = 201
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Caption = 'Logger channels'
    TabOrder = 4
    ExplicitWidth = 962
    DesignSize = (
      963
      201)
    object lblLogViewer: TLabel
      Left = 330
      Top = 0
      Width = 362
      Height = 21
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Caption = 'Run this together with the LogViewer application'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
      Transparent = False
    end
    object lblZeroMQPort: TLabel
      Left = 897
      Top = 75
      Width = 48
      Height = 20
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      AutoSize = False
      Caption = '0000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object lblZeroMQPortCaption: TLabel
      Left = 855
      Top = 75
      Width = 35
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Caption = 'Port:'
    end
    object lblIPCaption: TLabel
      Left = 704
      Top = 75
      Width = 19
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Caption = 'IP:'
    end
    object lblIPAddress: TLabel
      Left = 729
      Top = 75
      Width = 119
      Height = 20
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      AutoSize = False
      Caption = 'localhost'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblProcessId: TLabel
      Left = 128
      Top = 158
      Width = 125
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Caption = 'Process ID (PID):'
    end
    object lblPIDValue: TLabel
      Left = 258
      Top = 158
      Width = 119
      Height = 19
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object chkLogFileChannel: TCheckBox
      Left = 26
      Top = 33
      Width = 93
      Height = 26
      Hint = 'Enables or disables logfile channel.'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Logfile'
      TabOrder = 0
      OnClick = chkLogFileChannelClick
    end
    object chkWinIPCChannel: TCheckBox
      Left = 26
      Top = 155
      Width = 91
      Height = 25
      Hint = 'Enables or disables WinIPC channel.'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'WinIPC'
      TabOrder = 5
      OnClick = chkWinIPCChannelClick
    end
    object chkZeroMQChannel: TCheckBox
      Left = 26
      Top = 74
      Width = 91
      Height = 25
      Hint = 'Enables or disables ZeroMQ channel.'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'ZeroMQ'
      TabOrder = 2
      OnClick = chkZeroMQChannelClick
    end
    object edtLogFile: TButtonedEdit
      Left = 128
      Top = 30
      Width = 809
      Height = 31
      Hint = 'Database (server or path).'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akLeft, akTop, akRight]
      RightButton.ImageIndex = 23
      RightButton.Visible = True
      TabOrder = 1
      ExplicitWidth = 808
    end
    object edtEndPoint: TLabeledEdit
      Left = 353
      Top = 71
      Width = 204
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 73
      EditLabel.Height = 31
      EditLabel.Margins.Left = 5
      EditLabel.Margins.Top = 5
      EditLabel.Margins.Right = 5
      EditLabel.Margins.Bottom = 5
      EditLabel.Caption = 'Endpoint:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      LabelPosition = lpLeft
      ParentFont = False
      TabOrder = 3
      Text = 'tcp://*:*'
      ExplicitWidth = 203
    end
    object btnZMQBind: TButton
      Left = 128
      Top = 68
      Width = 138
      Height = 37
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actZMQBind
      DropDownMenu = ppmBind
      Style = bsSplitButton
      TabOrder = 4
    end
    object btnZMQCloseSocket: TButton
      Left = 575
      Top = 68
      Width = 123
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actZMQCloseSocket
      TabOrder = 6
    end
    object chkMQTTChannel: TCheckBox
      Left = 26
      Top = 114
      Width = 79
      Height = 26
      Hint = 'Enables or disables MQTT channel.'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'MQTT'
      TabOrder = 7
    end
    object edtMQTTBroker: TLabeledEdit
      Left = 353
      Top = 111
      Width = 204
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 53
      EditLabel.Height = 31
      EditLabel.Margins.Left = 5
      EditLabel.Margins.Top = 5
      EditLabel.Margins.Right = 5
      EditLabel.Margins.Bottom = 5
      EditLabel.Caption = 'Broker:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      LabelPosition = lpLeft
      ParentFont = False
      TabOrder = 8
      Text = 'localhost'
      ExplicitWidth = 203
    end
    object btnMQTTConnect: TButton
      Left = 128
      Top = 108
      Width = 138
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actMQTTConnect
      TabOrder = 9
    end
    object edtMQTTPort: TLabeledEdit
      Left = 617
      Top = 111
      Width = 72
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 35
      EditLabel.Height = 31
      EditLabel.Margins.Left = 5
      EditLabel.Margins.Top = 5
      EditLabel.Margins.Right = 5
      EditLabel.Margins.Bottom = 5
      EditLabel.Caption = 'Port:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      LabelPosition = lpLeft
      ParentFont = False
      TabOrder = 10
      Text = '1883'
      ExplicitWidth = 71
    end
  end
  object grpCheckpoints: TGroupBox
    AlignWithMargins = True
    Left = 5
    Top = 959
    Width = 963
    Height = 110
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Caption = 'Checkpoints'
    TabOrder = 5
    ExplicitWidth = 962
    object lblCheckpointDescription: TLabel
      Left = 18
      Top = 29
      Width = 428
      Height = 69
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Caption = 
        'Checkpoints are used to mark points in code of which a pass coun' +
        't can be monitored by the LogViewer application.'
      WordWrap = True
    end
    object btnResetCheckpoint: TButton
      Left = 720
      Top = 29
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actResetCheckpoint
      TabOrder = 1
    end
    object btnAddCheckpoint: TButton
      Left = 486
      Top = 29
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actAddCheckpoint
      ImageMargins.Left = 5
      TabOrder = 0
    end
  end
  object grpValues: TGroupBox
    AlignWithMargins = True
    Left = 5
    Top = 629
    Width = 963
    Height = 177
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Caption = 'Log values'
    TabOrder = 6
    ExplicitWidth = 962
    object btnSendObject: TButton
      Left = 486
      Top = 35
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendObject
      Caption = 'Send TObject'
      ImageName = 'Item11'
      Images = imlLogger
      TabOrder = 2
    end
    object btnSendRecord: TButton
      Left = 252
      Top = 35
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendRecord
      ImageName = 'Item11'
      Images = imlLogger
      TabOrder = 1
    end
    object btnSendComponent: TButton
      Left = 252
      Top = 81
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendComponent
      ImageName = 'Item21'
      Images = imlLogger
      TabOrder = 5
    end
    object btnSendStrings: TButton
      Left = 486
      Top = 81
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendStrings
      ImageName = 'Item19'
      Images = imlLogger
      TabOrder = 6
    end
    object btnSendDataSet: TButton
      Left = 720
      Top = 128
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendDataSet
      ImageName = 'Item20'
      Images = imlLogger
      TabOrder = 11
    end
    object btnSendBitmap: TButton
      Left = 720
      Top = 81
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendBitmap
      ImageName = 'Item22'
      Images = imlLogger
      TabOrder = 7
    end
    object btnSendRect: TButton
      Left = 252
      Top = 128
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendRect
      ImageName = 'Item11'
      Images = imlLogger
      TabOrder = 9
    end
    object btnSendPoint: TButton
      Left = 18
      Top = 128
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendPoint
      ImageName = 'Item11'
      Images = imlLogger
      TabOrder = 8
    end
    object btnSendInterface: TButton
      Left = 720
      Top = 35
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendInterface
      Images = imlLogger
      TabOrder = 3
    end
    object btnSendText: TButton
      Left = 18
      Top = 35
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendText
      ImageName = 'Item19'
      Images = imlLogger
      TabOrder = 0
    end
    object btnSendPersistent: TButton
      Left = 18
      Top = 81
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendPersistent
      ImageName = 'Item11'
      Images = imlLogger
      TabOrder = 4
    end
    object btnSendSQL: TButton
      Left = 486
      Top = 128
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendSQL
      ImageName = 'Item23'
      Images = imlLogger
      TabOrder = 10
    end
  end
  object grpCustom: TGroupBox
    AlignWithMargins = True
    Left = 5
    Top = 216
    Width = 963
    Height = 72
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Caption = 'Custom'
    TabOrder = 7
    ExplicitWidth = 962
    object btnSendObject1: TButton
      Left = 488
      Top = 24
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendTestSequence
      ImageName = 'Item17'
      Images = imlLogger
      TabOrder = 2
    end
    object edtMessageCount: TLabeledEdit
      Left = 134
      Top = 27
      Width = 109
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      EditLabel.Width = 119
      EditLabel.Height = 31
      EditLabel.Margins.Left = 5
      EditLabel.Margins.Top = 5
      EditLabel.Margins.Right = 5
      EditLabel.Margins.Bottom = 5
      EditLabel.Caption = 'Message count:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      LabelPosition = lpLeft
      NumbersOnly = True
      ParentFont = False
      TabOrder = 0
      Text = '0'
    end
    object btnSendMessages: TButton
      Left = 252
      Top = 24
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendMessages
      Images = imlLogger
      TabOrder = 1
    end
    object btnSendODS: TButton
      Left = 722
      Top = 24
      Width = 225
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSendODS
      Images = imlLogger
      TabOrder = 3
    end
  end
  object grpActions: TGroupBox
    Left = 0
    Top = 811
    Width = 973
    Height = 58
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    TabOrder = 8
    ExplicitWidth = 972
    DesignSize = (
      973
      58)
    object lblLogLevel: TLabel
      Left = 257
      Top = 20
      Width = 71
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Caption = 'Log level:'
    end
    object lblLogLevelValue: TLabel
      Left = 335
      Top = 6
      Width = 63
      Height = 45
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Alignment = taCenter
      AutoSize = False
      Caption = '0'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -32
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Transparent = False
    end
    object chkActions: TCheckBox
      Left = 21
      Top = 18
      Width = 236
      Height = 26
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Log execution of actions.'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object trbLogLevel: TTrackBar
      Left = 407
      Top = 11
      Width = 535
      Height = 37
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akLeft, akTop, akRight]
      Max = 31
      Frequency = 10
      ShowSelRange = False
      TabOrder = 1
      ThumbLength = 30
      OnChange = trbLogLevelChange
      ExplicitWidth = 534
    end
  end
  object aclMain: TActionList
    Images = imlLogger
    OnExecute = aclMainExecute
    Left = 450
    Top = 324
    object actSendInfo: TAction
      Caption = 'Send Info'
      ImageIndex = 3
      OnExecute = actSendInfoExecute
    end
    object actSendObject: TAction
      Caption = 'Send object'
      ImageIndex = 10
      OnExecute = actSendObjectExecute
    end
    object actSendWarning: TAction
      Caption = 'Send Warning'
      ImageIndex = 1
      OnExecute = actSendWarningExecute
    end
    object actEnterMethod1: TAction
      Caption = 'Enter Method1'
      ImageIndex = 8
      OnExecute = actEnterMethod1Execute
    end
    object actLeaveMethod1: TAction
      Caption = 'Leave Method1'
      ImageIndex = 7
      OnExecute = actLeaveMethod1Execute
    end
    object actEnterMethod2: TAction
      Caption = 'Enter Method2'
      ImageIndex = 8
      OnExecute = actEnterMethod2Execute
    end
    object actLeaveMethod2: TAction
      Caption = 'Leave Method2'
      ImageIndex = 7
      OnExecute = actLeaveMethod2Execute
    end
    object actSendError: TAction
      Caption = 'Send Error'
      ImageIndex = 2
      OnExecute = actSendErrorExecute
    end
    object actAddCheckpoint: TAction
      Caption = 'Add Checkpoint'
      ImageIndex = 6
      OnExecute = actAddCheckpointExecute
    end
    object actIncCounter: TAction
      Caption = 'Increment'
      ImageIndex = 13
      OnExecute = actIncCounterExecute
    end
    object actDecCounter: TAction
      Caption = 'Decrement'
      ImageIndex = 12
      OnExecute = actDecCounterExecute
    end
    object actResetCounter: TAction
      Caption = 'Reset'
      ImageIndex = 14
      OnExecute = actResetCounterExecute
    end
    object actSendTestSequence: TAction
      Caption = 'Send testsequence'
      ImageIndex = 16
      ShortCut = 116
      OnExecute = actSendTestSequenceExecute
    end
    object actResetCheckpoint: TAction
      Caption = 'Reset checkpoint'
      ImageIndex = 15
      OnExecute = actResetCheckpointExecute
    end
    object actSendClear: TAction
      Caption = 'Send clear'
      Hint = 
        'Sends clear command to the active log channels. This is typicall' +
        'y used to clear all messages in the receiving application.'
      ImageIndex = 5
      OnExecute = actSendClearExecute
    end
    object actSendODS: TAction
      Caption = 'Send OutputDebugString'
      Hint = 
        'Sends a message using the OutputDebugString native Windows API c' +
        'all.'
      OnExecute = actSendODSExecute
    end
    object actSendComponent: TAction
      Caption = 'Send TComponent'
      Hint = 'Sends a component object exposing all its published properties.'
      ImageIndex = 20
      OnExecute = actSendComponentExecute
    end
    object actSendRecord: TAction
      Caption = 'Send Record'
      Hint = 'Sends a Pascal record.'
      ImageIndex = 10
      OnExecute = actSendRecordExecute
    end
    object actSendStrings: TAction
      Caption = 'Send TStrings'
      Hint = 'Logs content from a TStrings instance.'
      ImageIndex = 18
      OnExecute = actSendStringsExecute
    end
    object actSendDataSet: TAction
      Caption = 'Send TDataSet'
      Hint = 'Logs content of a TDataSet instance.'
      ImageIndex = 19
      OnExecute = actSendDataSetExecute
    end
    object actSendBitmap: TAction
      Caption = 'Send TBitmap'
      ImageIndex = 21
      OnExecute = actSendBitmapExecute
    end
    object actSendRect: TAction
      Caption = 'Send TRect'
      ImageIndex = 10
      OnExecute = actSendRectExecute
    end
    object actSendPoint: TAction
      Caption = 'Send TPoint'
      ImageIndex = 10
      OnExecute = actSendPointExecute
    end
    object actSendScreenshot: TAction
      Caption = 'Send Screenshot'
      ImageIndex = 21
    end
    object actSendInterface: TAction
      Caption = 'Send IInterface'
      OnExecute = actSendInterfaceExecute
    end
    object actSendText: TAction
      Caption = 'Send Text'
      Hint = 'Send text (lorem ipsum)'
      ImageIndex = 18
      OnExecute = actSendTextExecute
    end
    object actSendPersistent: TAction
      Caption = 'Send TPersistent'
      ImageIndex = 10
      OnExecute = actSendPersistentExecute
    end
    object actSendMessages: TAction
      Caption = 'Send messages'
      OnExecute = actSendMessagesExecute
    end
    object actSendSQL: TAction
      Caption = 'Send SQL statement'
      ImageIndex = 22
      OnExecute = actSendSQLExecute
    end
    object actZMQBind: TAction
      Caption = 'Bind'
      OnExecute = actZMQBindExecute
    end
    object actZMQBindToEphemeralPort: TAction
      Caption = 'Bind to ephemeral port'
      OnExecute = actZMQBindToEphemeralPortExecute
    end
    object actZMQCloseSocket: TAction
      Caption = 'Close socket'
      OnExecute = actZMQCloseSocketExecute
    end
    object actZMQBindToDefaultPort: TAction
      Caption = 'Bind to default port (5555)'
      OnExecute = actZMQBindToDefaultPortExecute
    end
    object actMQTTConnect: TAction
      Caption = 'Connect'
      Hint = 'Connect to MQTT broker instance.'
      OnExecute = actMQTTConnectExecute
    end
  end
  object tmrSendCounter: TTimer
    Enabled = False
    OnTimer = tmrSendCounterTimer
    Left = 456
    Top = 104
  end
  object tmrSendValue: TTimer
    Enabled = False
    Interval = 400
    OnTimer = tmrSendValueTimer
    Left = 528
    Top = 156
  end
  object ppmBind: TPopupMenu
    Left = 408
    Top = 156
    object Bindtodefaultport55551: TMenuItem
      Action = actZMQBindToDefaultPort
    end
    object Bindtoephemeralport1: TMenuItem
      Action = actZMQBindToEphemeralPort
    end
    object Bind1: TMenuItem
      Action = actZMQBind
    end
  end
  object imcMain: TImageCollection
    Images = <
      item
        Name = 'Item1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000003554944415478DA55937D48545918C69F733FC6F9701C4D1DC41A159B
              ADACA68C2DA5596ADD45CA20C8ADA0A8DD58A9A03F0A291389A5462289A896A0
              88FE7077DD5DDA926229B68F35A335D949B6489B89D540CB8F510B1BB3EB78C7
              99B977E676EEB1CCCEB9877BB9E7BEBFF73DCF7D5EA2691A668EC048C8F997F7
              79C5939E6049F7E01B97FE6E5ECE2C7FE1DCCC96F5EEFC0687DDDA33F37BF201
              904868DCC5E6AEAADFEE741D2972CD313B3253B8AC8C6410BAF76A54A66029DE
              E61B8856AC5BE4D9565AF023C791C434400FDE73FA6E6B34410ACB563A2D1155
              C3F8A482C9681C84124C061E29660392780DB7BCDD6193403A2E5495AED6210C
              F07B5367F59DF6414F997BBEA52F2803B4A8F2E2D948B318C073042FC722B8DF
              39827054454EA605B7EE3F0B97AD70D47EB776E14932F04A72561C6FEAD8B466
              49F2B014834CB3EE5BF71972ED664C4CAAA059906C14F07F40C2CFF77AD97356
              8A01579B7C72C3A1B585E4ECD5F63A7FFFDB9A3C47061F084E82D000CF161744
              8183E7921F993623AACB0B1051E2387CD10FBDE239E966F4075EC717E7D84E90
              EFEB6E7AD3D26D6E15024211051C3DF4F62FF361A6997E6AEEC1AA8576ECF87A
              2E065ECB387AD9C70056930821A16274F4ED03B272F7AFE35FB9175803C130D4
              8446452360938AA767DFFFCD6264A59AD0D8FA027FB70F3180C013386815F7BC
              5D2152BCF39771F772A77590963F0DA0C1FABD7AF31214CDB7E31FDF30CEDFE8
              64C1FA5F1729203BDD84078FBA43E45BCFB57FE3BCF885A2F108D323800108FB
              FFE72B572349E471E08217D2440CCC319460368A1009155855BCE4CCA5B663B7
              1FF6D7D8525384A01461E5B38B2D0E53317AE854767D64D0A34963925A56947B
              82F40D8F3937565FF6E5E6659B25594194AA8DF7159CAD2C61A0BD675AA64074
              19450E36EA8FBEDEA1F09F27B72E6546AABFFEF860FDF58EDAB4F45916498E21
              A64C39B0BE660DCBBAE75433E2541F51E0919A9C8437C15179D78665B5BB367C
              7E6ADACADB7E686C7D3E24155AAC568B4A5DAE5088AA259813F569A05A704483
              1C9A909DB36D4FFEA8DBF2D1CA1F9AA9FEDAA3AA738D6D1E5E108D9C20F20651
              607B314585168FABB1582456B9D5EDD959BEE2D3669A39744DAE34FB2BFE7B1A
              2879D63BC2DAB920DFEE2F76E5B46C2E7535E465A77DD2CEEF0059D180DE8CD3
              74550000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item2'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002E24944415478DA65535D485361187EBFB3A3F338CF9C4BFB994E656A
              90E9141C5842CAAE22A28802BD2BA620585D68D84DD8B6B38548769110515E28
              91108697411711DA122F32BA509BE16FDB749ABA1FE7EFB67376FACEB769335F
              78F8CEC7799EE77BBFF7FD5E248A2224071F0CAA967A7A5A8323C346DEEF574B
              906B34DE9CFAFAC19CFA8641B956EB49E6A3030391E76997DD6E5E1F7ADFA6B9
              792B23FD7C29D06C0688511EA23E3FECBB1661F5ED8078D2D4C869CD16EE98C1
              4C53539F4CE04D27AE5DC569085818954C09C81EAF282505428E110046D15FF8
              EA7533A2699E18B86D9C25E2745AB3AF5C06010B0F04592DF788F9C6D32E0001
              9B0951B2DF9B980279B9DEAEB1DACC68DFEDD63AEB2EB9F2DA1E20483A75776B
              0B72391B1178DADB80A1696C9CC808C7D6E8D7ED12C79816799F75B7EF8C8D76
              67E8F5246D2029F3E0F30740F7E22521CF36DE0135C3FCBB0A466C770798EA0B
              1CFA596D1867CBF5062A9D899F2008645D5E5F87D28177C460F2C675C8C3053D
              CC005F47A66441086E3AD064BE6699BD58A341A9A987A74B585C5B838A0F1F89
              C1B8B10E4AB0205ED028206C20D7E9607F766E0AFDAA2C9B4431B14C9626075C
              55A0A4BB22040BA16DA8F8F499187CABA98673AC02F75A041409432C128198CF
              0714CB7AD1BCB1F6CBDEA8A396F4340129DCB8BB954BABE4FB47EE6928A6882F
              24F3E45586EF68CD66B16ED8394BB2585AB7B1C174E291966271E67F6229B2BA
              BA1F223E10502D1617B8C450484908284E0860F1A9793721FE29CA876C74548C
              28249E9971159087E4E3CCB660A7FD7132218CC1CCFC8E3F9CB385A038F89108
              6587D59AD961E188813407BEBBCDBD3B6FFA4D2893055A9D0D14AE3AA4318064
              348831DCDA7018C4D026F00B0BA0B86DEA57F7F6351E990529369F7096502767
              91E5E6A1D42A03D0BA22404C1AC4BC2B109D7642646E36C436B7F4281F996DD2
              1C1C3320E3ECF1687787061BF6308415AF86CA52FB29B5DA2FAF330EB3F75B9F
              532A553099FF17F61867FBA41871700000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item3'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000003434944415478DA55534B4C1351143D6F4AF94C0B456D01AB2D7E4689
              9F8652C4184D6AA3D11521246E4C080986AD312462D0188585E527B109716B44
              16AEDC901817159382DFA07C846022B64A0B886D212994297468A7DE197F7897
              F7BD7BEE3DE79ECB32990CB6C67A28248407062EAD8E8FBBD666666C4A4E5F56
              36595051E12BAEA9E9CBB35AFD5BFFB33F001959E616FAFBAFCE3D7A74DBE470
              F079160B975B52A2BE6DFCF881F5F9F974646424696D6C6CDD555F7F8F719CFC
              1740299E6A6C1C463269DF79E68C0EEBEB48C76290455105E0F47A6415162293
              9B8BEFCF9F2718CF8FDB1E3C702A202AC07C5FDFB5E567CF5A7751F1F2F434A2
              4B4BB0180CE018A3160CB22C632E1E87C96884F1C811CC79BD09637575DBEE86
              86BB2C110C0A137575E37B6B6AF4A96010311ADB74EE1C663C1E58F2F2D409E6
              363670B0A909D1C1416C233A1AAB155F0706C48AC78FED2CE0F1B813A3A32D06
              B35923114049733374C78E213A3A8A4F1D1D2AC0E11B3760AAAC8448B9704F0F
              B2F7EC416C6121CD3B1C5DECC3C58BAFF5858527B3A84B6A65055C7636CCD7AF
              4367B7233236A60214391C102726B0D8D5055992A0217AA99C1CACC5626FD80B
              9B6DD57AF66CBE343B0B6C6EAA9C4120C5376F6207152AB14C4061B71B8CDE15
              CD9856AB4E111C1C8C33EFA143AB56A7333F190A01A91494DD2C92680E1ABFB8
              AA4A0508BF7F8F31A261D668C094D52900160B42434371F6B6B6F6557626734A
              43C5E9B5357CA7E2CAEE6EEC3C7102ABC4598902E2BFF8EE1DC65A5A60E63870
              3A1DD2042201AFD9A7F6F63B4B4F9FB6E49B4C595224827D9D9D303A9D581919
              819FB45042A09CE1F8712CBD7C896F94D39A4C8847A3295A65178B0702C290CB
              F5B1C866E3D324A28178175FB8802FA48122986A24D2E4407B3BC24F9E802C0E
              AEA00091A9A9C4699FAF5C35D24C6F6F73E0FEFDB6EDA5A53A65137232F9DBE8
              EC9FE9E91F47CA6BC991CBB3B3E2FECB97DB0E5EB9D2F3D7CABEF3E78745BFDF
              4E547494544128FF0B87C4538A95ABA1D1459D204CB8BCDE7F56FE734C9F3D9E
              ABD36E776B0ECFE76AB45A8D96E7D5CE9B8A472429951445E9E8AD5BAD654D4D
              FF1FD3D688FBFD42E0E1C34B91E161576C72523DE76DE5E593454EA76F5F4343
              5FBE20FC77CE3F01B3D8870E7834B9A80000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item4'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000003194944415478DA5D937D4813611CC77FCFBDCCB9DB8B36C53067C346
              2FA429BDBB40EC851C4461486451D168457FD51F35248A6691942425D41F4146
              2A84BD615660DA1B254DC3C872511946CD96523AB3DBBAB9DDEE763D77EA9AFD
              E0B93BEE9EDFE7F97E9FFB3E489224482CDFCFA0E57EE717FBDBCF23C5FDDF7F
              E5C9EFE69A6678F2E7A43FDB68CDA93765E83E27CE475380982411D71EF61D6A
              7CF8E1F8F2BC2C8D295D4FCC4CD302C2DF7E8C72E01B66C5AEDE6F11BB2DD7B5
              7DFDFC730442B138406EDE5FF3A4231283025BA185090B1204C6A320B31126C8
              10BD460549A404ADEEFE503245BCB974786D910C51008DEDEF9D8F7A065D36EB
              3CC6EBE7E04F5800464582B37481A2AEE66E1F70BC083A3509D9E90CB43EEF0B
              952CCBAADC55B2F02CFAF62360B19F6E7B53B67E917688E5818B88CA8A0C9E7C
              B46CA102A86AFE00210C95CD6AD514CCD4ABE0567B2FD770C456802EDCEEA9F2
              0CFCAE309BD2489F7F1C1081809009583B934429F75038AA80644BB2E22CA306
              067C23626EB6A11AEDAE6A75A71AF556012808E289D817EEC10326CD2794DC2C
              0F5D320D544C80D151B61315EE6D08ACB6CED7F9FC21106252BCF9FCBE154091
              0444A22238AF744F0026211489C084553C757F0CA2157BAE06AC4B2DBAEF587E
              1C202F8E2F970F1629CF8EDA8E0405003406641A93A1F3557F10ED70B5BC1049
              7A55542227BCC62D003454AC512CED3CF324AE402668D434D048004288BA516D
              D3CB530FBABD1586143DE567C371EFF2CA4DC76C8ADC2D27DA94F6A9D0A619D4
              C08EB1826DB9B91A0D0C8D59363BAFF7CE36676A582EAA7896BB9B4F6E0012EF
              010E19F03803E5271F280AD43401064605DEAF83A13B67CBF39520D5B5F41CAE
              BBF7BA32D56864588E071E43D07F7F402E9A2221459B04BFFC7ECEB16949A5A3
              74714D3CCADB8EDCE8F832C416303A1D23E0944731449094B8034510A0A249BC
              1F1270C13FDC9C4CC3DBA6D35BFF4579EA30D535BF3A74F166978BA4683541D1
              A48AA6946F7C14A75014059E0FF307CAAD2EC7E665D30F53627907C72CB71F7B
              EC5DEF7CC59FBE0E2BC779414E866765AEE959D9BABC7AF3ACD469C7F92FB2D2
              6BE27DF91EC30000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item5'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002A14944415478DA8D9351485A5118C7BF736FA65DB5B9315BCB696DDD
              168DB9CAC70D9C14F424123D051118BE46046DF4D4EC6105B131217A8DAC875E
              43081FAC81C9DA201636651B2B1D6A19CD16587A35EFAEDE9D7B9D65DB823E38
              5CF8CEFDFFCE77BEF3FF10CFF3501ED9588CFEE1720D9CF8FDA6F4F6B65EC829
              9A9B03D5EDEDDE5B168BB34AA70B95FF8F4A00BE5020E20B0B23BBF3F32FD406
              0355A5D512B2DA5A71EFF4E000B27B7BF9C4C6464E67B3D935FDFD6F104114CE
              00823868B3F920976BBBDDD121876C16F2C92414184604100A0554A854C0CB64
              B0BFB2924114E5D7CFCE1A058808D8733A9F1FB9DD760D16B33B3B904FA7B18A
              F85323C2E515AB243148DAD404BB1E4FE6A6D93C7EC76A7D8532D128BDD5D7E7
              BF6BB128B868140AA95451545AA5102078914A25903A1D7C77B998F6C5C53614
              763826329B9BA3D7EAEA4816031016A1BFC56510A1E2CA860648C6E379CA6098
              421F7B7BD7152AD5E38AD35308C5E345C0598BD1BF00FC69D46880934A219D4C
              BE476FF5FA135D67A7928D44E0C1D2125C25BEF6F4885544575753C8D3D272A2
              331A95B9580C1EB9DD570204CD66A8D46A21B6B696421FBABBDF55F2FC1392E3
              8ADD2FF5E0B2C0D720E472C84B24C002ACA32F93932F7F2E2F8F2AD5EA0A3691
              38175F06C100895A0DA9C3430E3FE5144A85C3F49AC9F4A946AFA7F2C7C7C0E7
              72970384D3B19988EA6A48048399A75E6FAB68A4EDE9E967E19999F11BF5F572
              0E430AFF830862DC790976E45124C2340E0E8EDF1F1A7A7D66656F57978F0985
              DAF055E438294270BEC82149512C3C212E9D91D3F496C9E339B7726998BE391C
              239F2726EC528A9291120929A128F1E45FD8231CCB723986611F8E8DD99B8787
              2F0E5379A442213A3C373790F0F94CC940401CE7EBADAD811AA3D17BCF6A752A
              69FAC238FF06305C400EDDC6F0120000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item6'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002AF4944415478DA8DD25D4853711400F0737777B7BB4DE6643AB760A5
              A6A3A26950A994A169900AB94A4A2488A0B4A2B03030641166207D4116F5E04B
              2C0A02AB077DC8F530339AA649E6E6DC9A28EAA6CDA99BFBD0EDEE6EBBB79BA1
              585AEC3C1E0EBFFFFF7C20344D43AC61D03EB932D0F1A0DE3B6797EC3FD9D8B8
              F7A8BA01891530689FD60CBEBBD7A4D87540204B9640FB6B4DB046E3E6C70410
              7E9758539B31995BA012E8B4EDB067B71246666853D59D8FCA9880FEB6A61BF6
              FE57EA6DA932DCB3300BBA217FA0E2DAF33245669EEEBF80CD58A217888FBC6D
              7BFCB04ECE25A45C8480BE29D192B2B0EA51F1A93AF5AF9A7F02D14814B7EAA5
              41616266D83B6FC4C8791A8C7D3C32C0CDFE527DABB50045D1E87F81DE0E6D8B
              A967BC5ABE3504E93B09C0D8EF617AC2404B526AEEA665DD5423088BDA10A029
              029FB15E7AE17577977AE68B70AB2985651EF08138C90539078D10091B202DEB
              765DAAF2EAFD7540949C4BB29B4FE830369A2EDBACE2D17414C8A00DC6AD2E78
              F38C8211331FCED426BE2CAC6838CB6271C83F002AE28DB70F95757305A20C51
              421607C5840CE804BFDB022C7411C2C40FD0778649D5C54109C689F7AE3CBA0C
              D0D120CF3E7CBC0BE3B09442D10E1EB1E80441BC0C42011B789C66E0F28965C0
              34C8260F9D9E580FD84DE53A14BCB9712205DF69EB0479BA0AC8D0342C384CCC
              CFDC10A11600C729F8D6C7214BCE3100F72FE0FBA7785A2CC907C754172449F3
              214E9800A3E60FC0E74480CD5E04979B02E926E6A07AB0C8B1CBB30216FABBFF
              5520E0D1E78F7D2DED2299748AE2308C99BBC34B3E1F2695D0800B28B00CA3A0
              D80E601E16BBCACE4F26AEDDDAEA1043C4B4DC31DA5C6FB7B45C089334C22C80
              4A9690CCB0D94CEF086C494328042F6FCD2ED6546E08AC8D806F2CC3D27BBDD9
              31D99317F0FB842BF9A24ADD3EB12CE7F3DADA9F469056F0064F575100000000
              49454E44AE426082}
          end>
      end
      item
        Name = 'Item7'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000028C4944415478DA63FCFFFF3F03B1E0CED30B86358BC2B64B8B28DFCE
              F6EECA5692D2BDC4488A01D953EDCF591B5819FCF9FB8B61EBD18D3FFCCC533B
              8836E0E8D5AD7ECB0EB52E4BF6C9E2FEF6EB1DC3DFDFEC0C33374D784E94017F
              FFFE654EE833B817E5162FC7C8FA8E41904B8161FFD97D3F45B8D5271265C0E6
              1373D38FDC58D517EA14C9F5F2F355065E166586FE95AD9F97945F952168C08F
              9F5FB9A3BBB49E6407970A7CF97D9741825F8761FDA1B5DF8D957CAA426CB226
              103460E1EED6A67B6F4E973A9B3872BCFFFA9081F99F28C3E29DF35E2E29BB2A
              C7CACCF60B6EC0FE8BEBC2676FAF9868A1E9BD3EDBB73B979989E5CF87CFAFC5
              E27B0DEE67071771BDFD7A95415AD09061D99EA5DFC2AD4B921D0D425680F481
              0DB8F3F49241E95C9F63112E519C676F9EF9F6E3FBDF6B4DB1ABBCE7ED6E6AFB
              F6FB71ACBEAA26DBF7DF9F18BE7C65603876E9D48DD9F9A7B4181919FFC30D48
              EE37B969ACA1AB262122C4C0C6C2CD70E5DED53FE76E5CF8F0F3CF0F9E44AF44
              8EB75F6F3188F369322CDBBDE27B69F02C1F4315FB7D302F820DA85918BAF5FB
              DF57CE965A26EC1FBE3D61E0E1106178F7E9EBFFBF7F7FFD13E0E364FEFFFF1F
              C3B3571FFEBD79FFF3484FEA367BE430021BF0FDD7179EC6C5D1EB5F7FBE6765
              6B60CEF5E3F77B06162636062E764186B75F1E320870CA30AC3BB8ED7B6FCA36
              0B50F2C530000480B630CDDA56D3BDFBFCF20C3B03232E1696FF0CDF7EBC63E0
              E1140186D18BDF029C2AEB6B221784A3C7124634EE39B73266E2A68299866A6A
              5CA2FCFC0CBFFFFC67D87FEEF4F7B98567D4C504641E13340004AE3D3A635EB3
              2078BBB4B800DFD71FBF7E9B28F9CDCCF2ED2CC0964E7026A4D71F9F4A572D08
              D9C1CCC8F0AF2775873D0F27FF076CEA0098674197E681CAAC0000000049454E
              44AE426082}
          end>
      end
      item
        Name = 'Item8'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001F14944415478DA63FCFFFF3F033EB07EDFD518570BD58D3C5C6C9FB1
              C933E23360FAAA13155D0B0FB45D5C5520C4C7CDF181680380428C7553774DDB
              73F65EDCDB779FD9CE2CCB1525DA809FBFFEB26777AC5F73EBF10767434375CE
              BDFB4EFF3CB6304B8228033E7DF9C91F5BB362CF975FFF743434943898599819
              0E1D3AFBF7C3A7EFCCD8345F5A5D200837E0F9DBCFD21165CB0E717073CBCA2B
              CAB0BE7AF78D8183938541988F93818911C35E86FDFB4F815D0636E0D6C3375A
              E115CB0E4A498A0A4A488931BF7CFF83E1EF3FDC812B2FC1CB70F8D0999FC717
              654B30FEF8F987DD2E69FA3D762E4E49252539C6C7AF3EC335237CF71F6EF37F
              2054911160387EFCC2CF938B73202EB874FB857174D5F27D323212BC9C5CDC8C
              8F5F7F66C0973CD4E50419CE9EBBF6F3D412A8012000F24658F9D2C322222202
              DC7C3C4CCFDF7C65E0E5626510E2E30086013410A08101A2CE9FBF8E6A00083C
              7CFE4129A464C9316E3E3E615E7E3E16166626863B37EFFEFEF6E31713BA2B80
              DE644689051800C54670D192E34C6CEC12C222C2AC37AEDFFA796441963C1F17
              FB477443D8D9587E604D896F3F7C130D2D5D72F4DB1F46F9F7EF3FFC3FBD3497
              B884840C3E7DFD21105EB6ECE0F5FBAFF4404E25D90010F8FAE337776AE39A4D
              336A0283C93200047EFEFAC3010CCC3FCC408C4D1E005B66FD6AE8BC44D90000
              000049454E44AE426082}
          end>
      end
      item
        Name = 'Item9'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001E84944415478DA63FCFFFF3F032EB068EBB9AC386FA3690C7800233E
              03B4827ABF0438EA2C6DC971CB646264FC47B201EA013DDF4585F918D46585F6
              4EAB0A0C666763FE49B2011EAEE61C376E3CF8C1CEF4EFEA92D608673E1EF68F
              18066885F47DFEF6ED170FBA01AC2C4C0C9EEE960C7FFFFD677878EFE9EF4F9F
              3E3E59D9156D2B29CCFB14C500904DCE4EA61C4C4C4C0893191918408EFBF8E5
              27C387CF3F192445B8185E3C7FF5F7F1E397EF577644D9ABC98B5C4331C0DEC1
              84E3E9AB2F0CFF70F8880968A0982027C387B7EFFFDFBEFBF8EBBCFA102F331D
              D9C370032C2DF5381EBEF8CCF00F1CD6305318112423D83A0679717E86E7CF5E
              313C7EFCE2DB91F9998A70030C0D3538EE3DFD047401EE409516E161F8FFEF37
              C3DDBB8FBF4DABF00FB63751DA01374047478D838599094503C8AC0FC03078F7
              F90783A43037C39F9F3F191E3F79F6656E5D88B7B9AEEC219458F8F9E33727BA
              8DCCCC4C8C5ADAAA4CBFFFFE67F8F5FDDB7FA0D3BF2C6B8F74D253953883120B
              3F7FFD6507DAC7886E805ED884F7CAAA4A1CDFBE7CF9F7FEDDFB8FAB3BA3ECD4
              1444AF909490040405D9FEFEFEF6664D678CB582B4E01D9252A24660CF37015E
              AE77EB7A622CA5C4F81E635383D780B0B2A587A6570586080B70BDC2A506AF01
              DFBEFFE2E1E264FBC28007000039A8F8CAD93458040000000049454E44AE4260
              82}
          end>
      end
      item
        Name = 'Item10'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000020B4944415478DA63FCFFFF3F032580717019F0E9D327BE03070E38EC
              DAB5CBEDDEBD7B4AEFDEBD1302890B0909BD535252BAE7E6E6B6CBC1C1E1001F
              1FDF271403809871D3A64D7EF3E7CF4F95909050F1F0F010BB78F122F7C18307
              D9408AECEDED7FE9EBEB7FDDB163C7AB172F5EDC494C4C9CEDE7E7B7899191F1
              3FE3F7EFDF39A64F9F9E79F9F2E5D09C9C1C2D3D3D3DFED5AB57337CFDFA9561
              F6EC39105B181918F2F3F319424343192E5DBAF471CA9429D774757557676666
              4E67049AEAB167CF9EB2D2D2526B5E5E5EB08DC78F1F67707272629833673EC3
              82058B18B8B8B819C4C4048006CE061BF8F9F3E75FDDDDDD475D5C5CBA181312
              1216989A9A1A444747EBC3FC151414C4505050C1505D5D05E61B1A5A303C7972
              8D61DDBA75F0F05ABA74E9C5D3A74F5F60DCBF7FBF2330E00A525252DCD8D9D9
              3940929595950CDCDC720C3F7EFC002BE6E0E0007AE911437B7B3B98FFF3E7CF
              1F73E6CC0105E80446A02276A0C9C1B76EDDF2060247191919C9DDBB7733ECDA
              75904143C31AACE1C68DA30C6E6EF60CAEAEAE40973C79BE75EBD6FD6A6A6A5B
              812E5D0B8F8513274E58005DE30E74858EB6B6B6CEEDDBB725CF9C39C30B32C0
              C4C4E4B3AAAAEAF3AB57AF5E01DA7EC5D1D171A78585C509702C20A7836FDFBE
              715DBB764DFBC2850B06AF5EBD12FBF2E50BD8001E1E9ECF626262AF0C0C0C2E
              6869695DE5E2E2FA364893F2801800007FCBFA7A484423890000000049454E44
              AE426082}
          end>
      end
      item
        Name = 'Item11'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002694944415478DA8D534B6B9A4114BDE3AB46414DD4445D54497CA11B
              1FE417C47423AE74934DDB7537DD95927D0B5D343FA0500ADD240B5FE0B2B8E9
              5204952881BC50F18D1825BE35A6F74E30A4695A7A61F8E69BB9F79C73CFCCB0
              DBDB5B781CC562F1E3F9F9F9CB7ABD6EA07FBD5E5F339BCDDF4D26D3FEE35CF6
              18603299AC2712894A2814123F5C0F87C333BFDF6F5A5959A9FD01408C979797
              7BB55AED39FE0B944A25582C16B0D96C40FB27272780FBD0EBF58031B630180C
              E5CDCDCD4352C401107D8C8CCF96A8A55209D2E93407A1383D3D85EDED6D301A
              8D0F154DB046CA30F15BB95C7E4D6C76BB9D6FA65229E8743A7C50ACADADF141
              204B45E811ACAEAE1EB2482432733A9DA2C16040F2402814C2D9D9190483C1DF
              BCC13CAE68369B01E5DEDCDC505B73168FC7BB3B3B3BCAE17008FD7E9F83542A
              15D0E974B0B5B5C58B2F2E2EA0D96C8246A3E17B1434CFE5723D168BC5BA3E9F
              4FB9582C38EAF5F535A099201289402291F06462AD56AB2093C9402CBE3B1C85
              427107108D46BBBBBBBB4A2A9ECFE74040D3E9144811CDDBED368C4623DE1A81
              9242CA95CBE5707C7CDC6347474793402020A1C5656FC44860F46DB55A201008
              EEBDA0350221804C263361C964F2072EF8C8209245CC34968A1A8D0667A72065
              2A958A13519B52A9F427C344099AF42A9FCFBF53ABD5663A4E2A202FC6E331EF
              9D820A09904C448292CBE5FAEC7038BEDE5F654C5E2F140A6FF18CDFE02D5359
              AD56AE820A483231E2DBE863D1172C3EC016AA4FBE85ABAB2B67369BDDC78BB2
              E7F57A59B7DBE5201B1B1B118FC7F341ABD566FEF9989681452FD0A4F7285BE2
              76BB3FE1BD4F3C95F75780FF8D5FC67070A75DC8F2FB0000000049454E44AE42
              6082}
          end>
      end
      item
        Name = 'Item12'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001804944415478DA63FCFFFF3F032580711818E051EDFCFEFD970F02FF
              FEFF838880CC636440B019D0F8503613231383208FC00746F37C93FFFD69750C
              7F81908111280B328709A2186626231354235CEE3F03331016CE6A626034CB33
              FE5F1993C6D09BBD12A890894125FE07C3DD859C608D8AD1DFC1F4FDA510BE72
              FC77863B0B398006FF63289E1ACED0BE641603D80589FE5E0C1B5AAE3030010D
              908FFBCAF068310FD0424606D9C82F60573F5AC60D76817CEC1786878BB819FE
              010D08A8D16198BF711BC805A6FFDD9DF5185E7D790D76270832039DC90634EC
              FF5F46865FFFFE32FC63820406D84B8C90C010E31165D8B9F71203A329D08053
              6D150C4F57AE66900E0D6678BA7A1D837478088361791B58D3F9CE2AA0DC1AA0
              5C10C393D56B1964C06AD602D5843298557530303A973B7EFAF0FD0BEF5F60C0
              C0029919189892FC22CF54C5D4CE1EBA7DCC1726C78014292035029C3C9F19CF
              9F3F6FF0F7EF5F66F4F8656363FBC5CBCBFBF9EDDBB7C2B8D2003333F3DF4190
              1207DC00009A0CA1AFA9940BE20000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item13'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001634944415478DAA5D3CD8A82501407F073C15C150445042D7D823EA4
              AD3E4004F3022D661945602241B89020A482285ACEC21718901E40B751D613B4
              14420A025B557073EEA566275A07CE4ACECFFFBD7A10C6183E29F40496CBA5E5
              799EC0B22C3C9B6118F2EC7EBFC3F57A7D752E97B36BB59AF8024CD3B4104242
              A15088F456D7752198B3EBF5BA4880F97C8EABD56AACE8ABD50A5AAD1622C074
              3AC53CCFC702D6EB35743A1D0A8C46A3B700599629301C0E71A55281F3F90CB7
              DB2D74309148403A9D86CD6603BD5E8F029AA6E172B90CC7E3111A8D46286018
              0664B359701C075455A540BFDF27094EA713885FDFA180F5FB03994C8624180C
              061450148524F07D9F7CF3B0FAFB3752A91449A0EB3A052449C2A55229D6256E
              B75B984C261468B7DB6F01B3D98C02CD6613178BC558C06EB783C5624181208A
              75B95C847C3E1F69F87038403299B483A38BAF651A8FC7D67EBF8FB44C1CC7D9
              DD6EF77F993EA907B7AECBE1F3632F810000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item14'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001D14944415478DAA5933B6B024110C7E7402D44515054B0F413F8C256
              FB1C9298A784A448914234828A08622182880A46B14891222198A709627A6DC5
              D727B014541414C542858DB78B975C24226460B96567F6B7FF999BA11042F01F
              A3568062B158EAF57A66814000ABC5E3F1B06FB158C06C36639742A128D3346D
              61018542A144519459AD566FF56ABBDD86E5BDB2D56AB5604026934126938913
              D4EFF7F16B8C316AE47239C75FA954C0E9745218904AA590D168E404743A1DB0
              D96C789FCFE741A55271FCD56A15DC6E3701C4E3F13540B7DB85FD8323BC7F7B
              7D06A552B906F0F97C04108D4691C16080E17008F3F99C0DDA3BB4E3EFFB4B8E
              3DE3F3F920954AA156AB412010208070388CF47A3DCE7BF7F87C63013F9EEE70
              3DEAF53A8442210208068358C1603000EBC9C54640E1F1166432195610894408
              C0EFF76305E3F118FFF395D1F64BD223B91BF68CE90DB1588C15C4623102F078
              3C48A7D3715E62EAB173EAC0FBCF872CCEFBA7351A0D48269304E072B9D600A3
              D108E8B32BA2E0FE1A2412C91A209D4E1380C3E1405AAD9613309D4ED97418D9
              42A190E36F369B90CD66096029A534994CCCBF9BE52F639A4C24129597A95BD8
              614A2412A556ABB5D53069349AB2D7EBFD1EA6FFD8174A38F5E1F7F60C4E0000
              000049454E44AE426082}
          end>
      end
      item
        Name = 'Item15'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001794944415478DAC593BF6E825018C5BF3BF804D449121A1A21104AE1
              0D1C8C0FA0460725EAE02C130FC1446707FFA083467D00E3E01B5081D0504343
              133B499FC0C15E3A902E77681C7A936FF8E5CB39C93DF75C74BD5EE196836E36
              984EA79A6559FA5F85BAAE5B9AA6D9485114475555E572B964CBF3F90CF97C9E
              C8B95C0E1CC779C1A32259960F9224C9954A053A9D0E4C2613188D46D0EBF588
              BCDD6EC1F77DF770383C212CF6445194E6F33954AB55D86C36502A9560BFDF13
              B9D56A411004BEE7798F481084008F309BCDA056ABC17ABDCE04246EB7DBF08A
              0F361111C77121CFF39C6DDB50AFD761B55A650212E3F0200CC3373C3C625936
              C2066C7AB746A301CBE532139038CD028BDFA3287A400CC3C4C56291198FC7D0
              6C3661B15864021277BB5D381E8F1F711CDFA342A170C2068572B9FC134E1AE6
              7038847EBF4FE4DD6E971A7C9E4E271A511495E0A7A47EF7002F80A66922A73D
              705DF72B49923B845B38300CC3FC6B134DD3340683C1F3ED7FE1DF0DBE0173FF
              F6E1A7ABF8050000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item16'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000003014944415478DAA5D3596813511400D0974C2699CE4C3B26D3D825B5
              B5D63DADA64905E98215B7D2DA7E44145A82A0088AF6C3BA55100B8AE2861B58
              51113FA4B68A900FA3A26240A90B6A12D32504C4D4B6B669B3CE649F2C936846
              2CD60DC4FBF9DEBD87FBB8F7F152A914F89FE0FD0CB8C26CFE8B9198DAE28C55
              7EF6B1F3D367B308E8837CA6F0557591502B4521FB1F8197A34CE3CDBED051B1
              44942D2644928C0C019ABE8D4412619A8E7A292AEADEBC14EBA82A4474BF00BD
              C3E1C6AE81D0F1B252B2CCEF6301CB4EEF0C827800CF8480C5E219D02CC10ED5
              CC467553803394C86F7FE878505E9EAB703A228089B28CDF171E6798049D4E42
              10C18C2C029521420891648B407FBFC37CBA21B7612626B073408FC9DBFAD69D
              6817C170412412635CEE9045B38C3C57BF88E84E030FADBE96AE779E3DD92426
              47326024CCC4C6AA7285A79A95924B1CB05FFBA907926635053C51940E846D2D
              E5644743A9B8FBC7273C18A45ABA4DEEA3442656826050180A86EF9D51173773
              80FA72BF61AEBC40E5758580D74919B53B9754FC6E64E93C822454182E02F611
              2797C701EBCF1B0C45F24215ED8B02DF98C378BFADA2426B98D0BC1DA26B4E6E
              5AB4FD3BB0B1D3F41CCB211522910078471DE6BBBB942B3860C70D734F80209B
              E8380F85288FAD4C023F328CD0CBD3458F0F5472DD3C1D74D5DF31BBDA002196
              09926C348F0DEA3BD40BF771C075FD50ABD6166CA7899C82AC0C0188F70D18F3
              ABCB55F617EF8DFAC3B5157DC394F29E617C935F2C5DCC42423E8FF2F8D716E3
              3DEB14B93A0E98A422F9DBAE196ED1A58ADAC9600294488420144B02E8CD6BE3
              852DCBB6BF1A0EACF16712795F8B5310FFEB58C74627B7AE9C738540617A6A91
              8EDC361F7BD26FAFE3AF5EA5B2524920C3F9007FAE379EDD53B7CFEA67E58914
              8F9FCE4327C6FD95F3C8DEE21C7C686A91BA9F7DD45CBC6FD98D6D68547D0E7D
              03099807209DCE78E660D35EB79F910A99480C4FC6820B6459D63C313A316D95
              7774F65E35D95CAA9FC7A62C911A0F6E549C88C793308EC24112473C42981FFB
              EB6FFCD7F802FBAF72F067A43D630000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item17'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000024F4944415478DA63FCFFFF3F032580119B01FFFE7CE5BE7F3161013E
              8DCAC6AB43711AF0E1C566DF6F9F0ED44AC9DB9BFEFFFB1D28F29F8191858FE1
              C387A71FEE5E5A754EC3A4BB985BC0E0027603FEFF637A7029778A98BC4538EB
              AF3B4260CDAC820C9F7F707CB87775DD3935E3EE625E418866AC067C7E77CAEC
              CDE3195DD2F216F6BFBFDC6060621562F8F69BF7E3836B9BCFA999F614216BC6
              6AC0C32BF54DDCBC6C89AC8C6F6498983918BEFC14F874FBF2F68B867613F2D0
              356318F0E3DB23B9BBE78BE728AA5BBBFCFC748D91954B9EE1DECDE35BD135E9
              D96FF1C16AC0E39B53B3FFFDBE912F2A2EA7FAF7F76758A0809431FCFDC7F2F3
              F1FD73C764D40B1B05C5EC0F6235E0C476A7033F7FFE65FEF9E32FF39F3F7F98
              347494CD7F7E7BC5C0C629F9E7E3FB7727A595132688CB87AC21980E3EBC3E63
              72EF72ED34611146530606E6FF6F5FFF3E2B2C1D30575E236346CFD14F3375C5
              590FBBAB702EC169C0FD6B3332BEBF5F552E2CF25BE1D95396AB5C02CE2BD48C
              AA5B1981CA13563D3B035213AECF37C1539D670956034EEE895F28257125F0FD
              5BE637736EA6FD7FC566FF0164174CDEC14DC5F8C0AE3B67A34D84276018F0ED
              CB53E9933BC3D68A897F91FDF8596F5FC7ED021DD7206383EB9F8069E40F429D
              183B03C39DDD17CE62A6839B6B836F5FECAC6664917862E53E2BAD6ED39BC64B
              0F3F1A23ABF968676DCC7FE8E8D95427454C179CDC53DCFBF6D5253D1BCF1919
              7C82CA77D1BD6757B70B1C06D9EEEA13C2ADE531C360ED1CD3D3B69E5372C4A4
              CD4F326001D9338ECEB4D6103F1CE5A0028E0500476E24F90BDBEEBE00000000
              49454E44AE426082}
          end>
      end
      item
        Name = 'Item18'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000022A4944415478DA63FCFFFF3F0332F8FBF6ADF0F7458BE2FE5CBAA4FF
              EFC70F0E260E8E1F2C7A7A1739E3E216310B0BBF6540038CC806FC79FE5CF273
              4D4D2B2B0F8F2EA7A2A23A332323EFDFFFFF3F7FBF7FFFE6EF2F5F2EF3B6B454
              B3484A3EC769C0979E9E1286274FA27984840CFE7FF98250C4C3C3F0E5DDBB0B
              0C32324B794A4A7A701AF0262E6EB1A0AE6E00E3CB973CABDFBF67D8F1F93383
              072F2F43A8A020C37F71F12FEF2F5FDE20B268512C4E035E8686AE1635360EFA
              FFF22553D4AD5B0C1FFFFE65E067666658A6A6C6C0282EFEEFF5D9B3EBC457AF
              0EC569C0D3A4A4F942D2D27EAC1F3F0A2D78FE9C61DDAB570C4162620C099292
              0CBFF9F9DFBD7BFA7493F4BC7989380D783D7366FACF2347D2C4C5C58DFEFFF8
              8150C4C1C1F0F2E5CB73EC3636B344D3D367E234E0F79B3722779293E74A292A
              3A70FCFCC90713FFC1CEFEE9D9FDFB0754E6CE4D6615117983D3001078367F7E
              E2C70D1BB295E4E48C81E98001980E18EE3D7A74963F2060AA5462E27CBCE900
              9C16BE7EE53E1F13B3448697D78E9F8545E8E39F3FEF9E7CFE7CC870C9921816
              6EEEAF040D008147CB9645BD58B0A04C475656FFCAE3C717251212BAE4A2A296
              316001580DF8FDF933EF5E0F8F1DD62A2A5647EFDC39E6BC6387072B2FEF67A2
              0D00814D1616271C9495CD0FDCBD7BD2EFC4090B061C00A7016BCDCC4EB92829
              99EEB977EF74F0A95366241BB00664808282E99E070F4E879063C0B559B3D2AE
              CD9993A2959232472B2D6D162E030041D60DF0C6C4A4A10000000049454E44AE
              426082}
          end>
      end
      item
        Name = 'Item19'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000009C4944415478DA63FCFFFF3F032580916A06343434345EBD7A550BC8
              078931FEFBF78F099D0DA241584F4FEF7257575729755D101C1CBC16DD36645B
              77ECD8E141DB30F0F7F7DF886C23321B1B3E7AF4A835755DE0E5E5B50D9FAD87
              0F1FB6A56D18B8BABAEEC6E76710FEF5EB17DBCF9F3FD97FFCF8C1F1E0C10305
              EABAA0A0A060E2D9B3678DD06DFDFDFB37ABBEBEFEC5850B17C6D3360C06CC00
              00FFB3E5E1AFC1210D0000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item20'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002404944415478DA633C7DFDBAEFFBAF5F2519C80082DCDCCF19779E3C
              99EE666636831C03769D3A95C1B8FDC4890C7773F3E9AE671918FEFFFF0FC7FF
              FEFD03E3FF201A990F953FED2CC000B43C9371CBD1A3591E969653DDCF3132B8
              2A3130ECBCF38FC14511A268C7EDBF0C4E0A108DBBEFFE87B3F7DC636038E9C4
              CFB0E3F8F16CC64D870F67BB5B594DF13ACF04D6A4C8FC9D4189E51B8A0BFE62
              714193853CC3CE63C77218D71F3890EB616333C9E70233D805FF9EBC6528D112
              00FBF1F4AD5B0CA66A6A60F6B1AB5719ACB4B5C1EC43172F3258EBEB33EC3872
              248F71EDBE7D79EEB6B613FD2FB2804D76617FC7E0CCF10E6C1B271B1BC3E76F
              DFC0E25CECEC0C9FBE7E85BBC0424F8F61E7E1C3F98CAB77EF2E70B3B3EB0FBA
              CC067741BE3A2FD8A60B77EF3218282B435C73E30683A98606987DFCCA150653
              1D1D865D870E15322EDFB1A3C8DDC1A137F40A3B860BD8595919BE7EFF0E16E7
              00BAE60BD435206C04F4CECE03078A19976CDD5AECEEE4D413719503EE826C55
              6EB04D57EEDF67D0515404B3CF03C3C3101A1EA7AF5D63D0D7D262D8B96F5F09
              E3C24D9B4A3D5C5CBAA2AE7162B880958585E1FB8F1F607136A06BBE415D03C2
              BA9A9A0C3BF6EC29639CBB7E7DB9A79B5B47EC752EB80B329439C1365D7FF890
              41535E1ECCBE74E70E839E8A0A987D0E181E5AC0F0D8BE6B5705E3CC952BABBC
              BDBD5BE36F7063B80039EED1D38186BA3AC3D6AD5BAB19BB66CD6A8C8F8FAFFB
              F9EB1749F9801D18A80B172E6C629CBF7A75EABDC78F95C8C94C4AB2B2F71841
              CEA1040000B51C6547B45525670000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item21'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000774944415478DA63FCFFFF3F030C4C98B210C121025CBA748D8111D9
              80AABABEFF6D4D45441BE0E61D430503DABB676075F6AD5BF718E6CDEA246C40
              656D2F565B93D2CA1926F65433F0F2F1D1D880C4D4B2FF4F9E3EC5AA60EDF269
              840D400E44A0ADFF41B62203920C188D85211B0BE82EC0E51D5C00007B947EF3
              4DC851690000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item22'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001E74944415478DAA5534DAB5241187EDE99F3E1BD10B46D69BF208216
              97366D5BB70BC1921032706198A01745D2851B058D23288882F7170411EDEE32
              88FB17FC034164515CF5CCDB33A77FA0037366CECC3BCFD730A2AA38A5C9C900
              CBE5321191D7C71C26F954168B85160A85A3D8D7EB35643E9F6BB158C4B38F5B
              8401101841C41E2ABB0542CE63FF6F0C4793ED911BEF2E42AC562BC8743AD552
              A984E79FB6B0DCB4EA410CCE58178406D6128073230A13048824803A87CB0B0B
              AA874C26132D97CB78F1F9177C9C81088B0DC4192A72C8190B1B28620259F1EC
              8294C1BF7F1C61369B4186C3A1562A15BCF9F21BAC4556C3224B160E541081C4
              C8C14102EB97709B027D02244902190C065AAD56F1F67A9BF9231F99E9D91F06
              553008A7B482147164714EB0C34EF1EA81C5783C86F47A3DADD56AF8F0ED27EE
              18E08C45268A5928942F388F0DFE1E7670E241B91607903DF0E89E80EA21ED76
              5B1B8D06AE6EBE23276460624118923EF261D3B7FF1818FA0E2DB53B8B7DEAF0
              E47E0CAA87349B4D6DB55A78B9BAA1B7036EF78A3F0760EB1896BFB0F4BF9594
              688E1918E2E562E0BAFC10FD7E1F52AFD7B5D3E9E06EEF2B183CDC6E07908146
              39661059985E0593CD020E63C58FCBA7E876BB10FA573F39A67962198D46C966
              B339EA2DE4F3F9E9C9AFF11F0391B34D805887F60000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item23'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001CB4944415478DA63FCFFFF3F03258071181870F8F0619B0913261488
              8A8ABE0602D1356BD684BC7CF9527CCA942939D7AF5FD7E4E3E3FBE4EBEBBB39
              2020604303105CBD7A551BA4312C2C6C1508330225D6878787AFD4D1D1B9525B
              5BDBBC7EFDFAC0EAEAEAD6DFBF7FB3969797775EB870C1006458636363BD9E9E
              DEA5C0C040B0FA888888156017787B7B6F0109262727CF5DBE7C7924102CF7F1
              F1D90252141B1BBBF8EFDFBFCCFEFEFE1BA3A2A2968130480EC6061BB06FDF3E
              C78E8E8E0A906D4E4E4EFB4082EEEEEE3B63626296800C40E7A3CB810371EFDE
              BDCE2D2D2D3540D0E2ECECBCD7D1D1717F7C7CFCC2848484052045C87C7439C6
              3973E624839C0FF227C80B478F1EB50682A32045696969B3405EB0B3B33B9494
              94340FA40E2407639F3B77CE88D1D8D8F8CC8C193332CE9E3D6B3C7BF6ECD433
              67CE98A4A6A6CEFEF9F3277B777777E98913272C9A9B9B6BA74F9F9E696A6A7A
              DAC4C4E40C481EA8EF6C4646C60C463939B987B2B2B28F814076F2E4C9B97E7E
              7E9B40EC9292921E90666161E1B72097A4A7A7CF0405ECC99327CD6169C0CACA
              EA18DE84D4D9D959BE73E74EF74D9B36F9F1F0F07C2139257EF9F28507E4A28B
              172FEA6FDCB8D1DFC6C6E6C8E0CB0B0072A10DA6115656660000000049454E44
              AE426082}
          end>
      end
      item
        Name = 'Item24'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002B24944415478DA8DD36D4853511800E077F76EABB9A10B476325B49C
              281439951812FD89FAB3FCB87E148585A415A429A6A2652606265234512BAC64
              7EC0CC4CFB70A9DB74A649E91F6383B028A184C03E4016C8A6DEEBEE3AE7B69D
              D80FA10397F79ECBFB3EE73DE7DE2B0A04028087D3DA55E0F32C6AE13F46C436
              EDE291CC822E7C2F0A01D6EEBA86C30715D7BC4B1F41C4072042150B32650C50
              B434BC5A2C06EBC8871B99671AEAC280A1CEDA46E658CAD5002A66577E8277E9
              13CCB9E641A44D0FAB174924F0EBF3D7F6BC22537118F0DC5CD3C4E4245F0970
              5C30510AEED7D3B0DB781B589E07CEEF071EE5BADD6E5898ED7730B9E5A5713A
              DD02019E7554DF62B293ABFE01127837310EBAF43BC06E6CC02A7ABE8EA2CBE5
              823CA3112C0303C5A929294E023C7D506962B2922B788E15E614EA60D6360CBF
              E34A40A95482D7EB85789D0E461D0EB29D358FA78E0003ED975A18465FE667FF
              02B4540A33361BA8D3DA80475BC079D10A05E9442E97C3AB91915202F4DF2BB9
              CB64245DF4B3EB41600BBCB58F029B580D515151B0BCBC2C2018C397C16080B1
              A1A132023C6E2B6A6732F4173682801801D3761BC41E3793A2502738AA552A78
              3938584E80DE96F31D596989E7C20107D0A9F5C219E00E7027B8D8E7F38141AF
              87277D7D9504B0980A3B99A3FB0AB8202041C0143AB03DF98F8455432B6B2223
              8548A3B764B158AA08D07DF3B4253B4D7F8A63D782C05698B48F83E25093508C
              BBC02BEF4F48003F9AD3340D5D3D3D970970BF3ED37A32F74006BBBE2ACCA552
              194C389C9053352E7C40380F17F2C14E28043C349B6B08606EAE68DE49CD9D8D
              8DDF2B8BDEAE91E08431BB134ED44C02CE10B6816208038A02536BEB7502E031
              39F622EB8BDB66A4B81F09BB7628B5CEA93714155FD8BBD95F19A3567F0B0342
              63FEBD3BC935339CBEE2F9AE61F26B1B37036432D9EA1FE7B67833D83EE13F00
              00000049454E44AE426082}
          end>
      end>
    Left = 480
    Top = 540
  end
  object imlLogger: TVirtualImageList
    AutoFill = True
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Item1'
        Name = 'Item1'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Item2'
        Name = 'Item2'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Item3'
        Name = 'Item3'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Item4'
        Name = 'Item4'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Item5'
        Name = 'Item5'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Item6'
        Name = 'Item6'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Item7'
        Name = 'Item7'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Item8'
        Name = 'Item8'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Item9'
        Name = 'Item9'
      end
      item
        CollectionIndex = 9
        CollectionName = 'Item10'
        Name = 'Item10'
      end
      item
        CollectionIndex = 10
        CollectionName = 'Item11'
        Name = 'Item11'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Item12'
        Name = 'Item12'
      end
      item
        CollectionIndex = 12
        CollectionName = 'Item13'
        Name = 'Item13'
      end
      item
        CollectionIndex = 13
        CollectionName = 'Item14'
        Name = 'Item14'
      end
      item
        CollectionIndex = 14
        CollectionName = 'Item15'
        Name = 'Item15'
      end
      item
        CollectionIndex = 15
        CollectionName = 'Item16'
        Name = 'Item16'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Item17'
        Name = 'Item17'
      end
      item
        CollectionIndex = 17
        CollectionName = 'Item18'
        Name = 'Item18'
      end
      item
        CollectionIndex = 18
        CollectionName = 'Item19'
        Name = 'Item19'
      end
      item
        CollectionIndex = 19
        CollectionName = 'Item20'
        Name = 'Item20'
      end
      item
        CollectionIndex = 20
        CollectionName = 'Item21'
        Name = 'Item21'
      end
      item
        CollectionIndex = 21
        CollectionName = 'Item22'
        Name = 'Item22'
      end
      item
        CollectionIndex = 22
        CollectionName = 'Item23'
        Name = 'Item23'
      end
      item
        CollectionIndex = 23
        CollectionName = 'Item24'
        Name = 'Item24'
      end>
    ImageCollection = imcMain
    Width = 24
    Height = 24
    Left = 552
    Top = 396
  end
end
