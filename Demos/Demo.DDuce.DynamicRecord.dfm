object frmDynamicRecords: TfrmDynamicRecords
  Left = 0
  Top = 0
  ClientHeight = 609
  ClientWidth = 784
  Color = clWindow
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object splHorizontal: TSplitter
    Left = 0
    Top = 274
    Width = 784
    Height = 6
    Cursor = crVSplit
    Align = alBottom
    Color = clScrollBar
    ParentColor = False
    ExplicitTop = 278
  end
  object pnlBottom: TGridPanel
    Left = 0
    Top = 280
    Width = 784
    Height = 329
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 37.500000000000000000
      end
      item
        Value = 62.500000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = pnlRecordInspector
        Row = 0
      end
      item
        Column = 1
        Control = pnlBottomRight
        Row = 0
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end
      item
        SizeStyle = ssAuto
      end>
    TabOrder = 0
    object pnlRecordInspector: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 288
      Height = 323
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object pnlRecordInspectorHeader: TPanel
        Left = 0
        Top = 0
        Width = 288
        Height = 19
        Align = alTop
        BevelOuter = bvNone
        Caption = 'DynamicRecord content'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentFont = False
        TabOrder = 0
      end
    end
    object pnlBottomRight: TPanel
      Left = 294
      Top = 0
      Width = 490
      Height = 329
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object pnlRightBottomHeader: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 484
        Height = 19
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Conversion methods'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentFont = False
        TabOrder = 0
      end
      object pnlTRecordRepresentations: TGridPanel
        Left = 0
        Top = 25
        Width = 490
        Height = 304
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 100.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = grpAsCommaText
            Row = 0
          end
          item
            Column = 0
            Control = grpAsDelimitedText
            Row = 1
          end
          item
            Column = 0
            Control = grpToStrings
            Row = 2
          end
          item
            Column = 0
            Control = grpToString
            Row = 3
          end>
        RowCollection = <
          item
            Value = 25.113906109260300000
          end
          item
            Value = 24.947071570572670000
          end
          item
            Value = 24.948130782096440000
          end
          item
            Value = 24.990891538070580000
          end>
        TabOrder = 1
        object grpAsCommaText: TGroupBox
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 484
          Height = 70
          Align = alClient
          Caption = 'AsCommaText'
          TabOrder = 0
          object mmoAsCommaText: TMemo
            AlignWithMargins = True
            Left = 5
            Top = 18
            Width = 474
            Height = 50
            Margins.Bottom = 0
            Align = alClient
            BevelInner = bvNone
            BevelKind = bkFlat
            BevelOuter = bvRaised
            BorderStyle = bsNone
            Color = clWhite
            DoubleBuffered = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentDoubleBuffered = False
            ParentFont = False
            ScrollBars = ssVertical
            TabOrder = 0
          end
        end
        object grpAsDelimitedText: TGroupBox
          AlignWithMargins = True
          Left = 3
          Top = 79
          Width = 484
          Height = 70
          Align = alClient
          Caption = 'AsDelimitedText'
          TabOrder = 1
          object chkQuoteValues: TCheckBox
            Left = 11
            Top = 22
            Width = 83
            Height = 17
            Caption = 'Quote values'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnClick = chkQuoteValuesClick
          end
          object edtDelimiter: TLabeledEdit
            Left = 61
            Top = 47
            Width = 64
            Height = 21
            EditLabel.Width = 49
            EditLabel.Height = 13
            EditLabel.Caption = 'Delimiter:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            LabelPosition = lpLeft
            ParentFont = False
            TabOrder = 1
            Text = ';'
            OnChange = edtDelimiterChange
          end
          object edtQuoteChar: TLabeledEdit
            Left = 157
            Top = 20
            Width = 17
            Height = 21
            EditLabel.Width = 60
            EditLabel.Height = 13
            EditLabel.Caption = 'Quote char:'
            LabelPosition = lpLeft
            MaxLength = 1
            TabOrder = 2
            Text = #39
            OnChange = edtQuoteCharChange
          end
          object mmoAsDelimitedText: TMemo
            AlignWithMargins = True
            Left = 180
            Top = 18
            Width = 299
            Height = 50
            Margins.Bottom = 0
            Align = alRight
            Anchors = [akLeft, akTop, akRight, akBottom]
            BevelInner = bvNone
            BevelKind = bkFlat
            BevelOuter = bvRaised
            BorderStyle = bsNone
            Color = clWhite
            DoubleBuffered = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentDoubleBuffered = False
            ParentFont = False
            ScrollBars = ssVertical
            TabOrder = 3
          end
        end
        object grpToStrings: TGroupBox
          AlignWithMargins = True
          Left = 3
          Top = 155
          Width = 484
          Height = 70
          Align = alClient
          Caption = 'ToStrings'
          TabOrder = 2
          object mmoToStrings: TMemo
            AlignWithMargins = True
            Left = 5
            Top = 18
            Width = 474
            Height = 50
            Margins.Bottom = 0
            Align = alClient
            BevelInner = bvNone
            BevelKind = bkFlat
            BevelOuter = bvRaised
            BorderStyle = bsNone
            Color = clWhite
            DoubleBuffered = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentDoubleBuffered = False
            ParentFont = False
            ScrollBars = ssVertical
            TabOrder = 0
          end
        end
        object grpToString: TGroupBox
          AlignWithMargins = True
          Left = 3
          Top = 231
          Width = 484
          Height = 70
          Align = alClient
          Caption = 'ToString'
          TabOrder = 3
          DesignSize = (
            484
            70)
          object chkAlignValues: TCheckBox
            Left = 11
            Top = 24
            Width = 79
            Height = 17
            Caption = 'Align values'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = chkAlignValuesClick
          end
          object mmoToString: TMemo
            AlignWithMargins = True
            Left = 96
            Top = 18
            Width = 383
            Height = 50
            Margins.Bottom = 0
            Anchors = [akLeft, akTop, akRight, akBottom]
            BevelInner = bvNone
            BevelKind = bkFlat
            BevelOuter = bvRaised
            BorderStyle = bsNone
            Color = clWhite
            DoubleBuffered = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentDoubleBuffered = False
            ParentFont = False
            ScrollBars = ssVertical
            TabOrder = 1
          end
        end
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 784
    Height = 274
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitHeight = 278
    DesignSize = (
      784
      274)
    object pgcMain: TPageControl
      Left = 63
      Top = 0
      Width = 721
      Height = 262
      ActivePage = tsContactObject
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      ExplicitHeight = 266
      object tsContactObject: TTabSheet
        Caption = 'TContact object'
        object lblContact: TLabel
          Left = 0
          Top = 0
          Width = 713
          Height = 234
          Align = alClient
          AutoSize = False
          Color = clInfoBk
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Consolas'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          Transparent = False
          ExplicitLeft = 13
          ExplicitTop = 33
          ExplicitWidth = 364
          ExplicitHeight = 200
        end
      end
      object tsDataSet: TTabSheet
        Caption = 'DataSet'
        ImageIndex = 1
        object grdTest: TDBGrid
          Left = 0
          Top = 0
          Width = 713
          Height = 234
          Align = alClient
          DataSource = dscTest
          Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
          TabOrder = 0
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Segoe UI'
          TitleFont.Style = []
        end
      end
      object tsTestClass: TTabSheet
        Caption = 'TTestClass'
        ImageIndex = 2
        object lblTestClass: TLabel
          Left = 0
          Top = 0
          Width = 713
          Height = 234
          Align = alClient
          AutoSize = False
          Color = clInfoBk
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Consolas'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          Transparent = False
          ExplicitLeft = 37
          ExplicitWidth = 245
          ExplicitHeight = 364
        end
      end
      object tsTestRecord: TTabSheet
        Caption = 'TTestRecord'
        ImageIndex = 3
        object lblTestRecord: TLabel
          Left = 0
          Top = 0
          Width = 713
          Height = 234
          Align = alClient
          AutoSize = False
          Color = clInfoBk
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Consolas'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          Transparent = False
          ExplicitLeft = 13
          ExplicitTop = 33
          ExplicitWidth = 364
          ExplicitHeight = 200
        end
      end
      object tsDynamicRecord: TTabSheet
        Caption = 'DynamicRecord'
        ImageIndex = 4
        object lblTestTRecord: TLabel
          Left = 0
          Top = 0
          Width = 713
          Height = 234
          Align = alClient
          AutoSize = False
          Color = clInfoBk
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Consolas'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          Transparent = False
          ExplicitLeft = 37
          ExplicitWidth = 245
          ExplicitHeight = 364
        end
      end
      object tsAssignments: TTabSheet
        Caption = 'Assignments'
        ImageIndex = 6
        object pnlAssignments: TGridPanel
          Left = 0
          Top = 0
          Width = 713
          Height = 234
          Align = alClient
          BevelOuter = bvNone
          Color = clWhite
          ColumnCollection = <
            item
              SizeStyle = ssAbsolute
              Value = 60.000000000000000000
            end
            item
              Value = 25.000000000000000000
            end
            item
              Value = 25.000000000000000000
            end
            item
              Value = 25.000000000000000000
            end
            item
              Value = 25.000000000000000000
            end
            item
              SizeStyle = ssAbsolute
              Value = 80.000000000000000000
            end>
          ControlCollection = <
            item
              Column = 0
              Control = lbl00
              Row = 0
            end
            item
              Column = 1
              Control = lbl01
              Row = 0
            end
            item
              Column = 2
              Control = lbl02
              Row = 0
            end
            item
              Column = 3
              Control = lbl03
              Row = 0
            end
            item
              Column = 4
              Control = lbl04
              Row = 0
            end
            item
              Column = 0
              Control = lbl20
              Row = 2
            end
            item
              Column = 0
              Control = lbl30
              Row = 3
            end
            item
              Column = 0
              Control = lbl40
              Row = 4
            end
            item
              Column = 0
              Control = lbl15
              Row = 1
            end
            item
              Column = 1
              Control = btnClearFRecord1
              Row = 1
            end
            item
              Column = 2
              Control = btnFRecord2Clear1
              Row = 2
            end
            item
              Column = 3
              Control = btnFDynamicRecord1Clear1
              Row = 3
            end
            item
              Column = 4
              Control = btnFDynamicRecord2Clear
              Row = 4
            end
            item
              Column = 2
              Control = btnAssignFRecord2ToFRecord1
              Row = 1
            end
            item
              Column = 3
              Control = btnAssignFDynamicRecord1ToFRecord1
              Row = 1
            end
            item
              Column = 4
              Control = btnAssignFDynamicRecord2ToFRecord1
              Row = 1
            end
            item
              Column = 1
              Control = btnAssignFRecord1ToFRecord4
              Row = 2
            end
            item
              Column = 3
              Control = btnAssignFDynamicRecord1ToFRecord2
              Row = 2
            end
            item
              Column = 4
              Control = btnAssignFDynamicRecord2ToFRecord2
              Row = 2
            end
            item
              Column = 1
              Control = btnAssignFRecord1ToFDynamicRecord1
              Row = 3
            end
            item
              Column = 2
              Control = btnAssignFRecord2ToFDynamicRecord1
              Row = 3
            end
            item
              Column = 4
              Control = btnAssignFDynamicRecord2ToFDynamicRecord1
              Row = 3
            end
            item
              Column = 1
              Control = btnAssignFRecord1ToFDynamicRecord2
              Row = 4
            end
            item
              Column = 2
              Control = btnAssignFRecord2ToFDynamicRecord2
              Row = 4
            end
            item
              Column = 3
              Control = btnAssignFDynamicRecord1ToFDynamicRecord2
              Row = 4
            end
            item
              Column = 5
              Control = btnAssignFieldValueToFRecord1
              Row = 1
            end
            item
              Column = 5
              Control = btnAssignFieldValueToFRecord2
              Row = 2
            end
            item
              Column = 5
              Control = btnAssignFieldValueToDynamicRecord1
              Row = 3
            end
            item
              Column = 5
              Control = btnAssignFieldValueToDynamicRecord2
              Row = 4
            end
            item
              Column = 1
              Control = lblFRecord1
              Row = 5
            end
            item
              Column = 2
              Control = lblFRecord2
              Row = 5
            end
            item
              Column = 3
              Control = lblFDynamicRecord1
              Row = 5
            end
            item
              Column = 4
              Control = lblFDynamicRecord2
              Row = 5
            end
            item
              Column = 5
              Control = pnlField
              Row = 5
            end>
          ParentBackground = False
          RowCollection = <
            item
              Value = 13.261174032709310000
            end
            item
              Value = 13.005346466640740000
            end
            item
              Value = 12.811117535453830000
            end
            item
              Value = 12.621610380514640000
            end
            item
              Value = 12.403315687245580000
            end
            item
              Value = 35.897435897435900000
            end>
          TabOrder = 0
          ExplicitHeight = 238
          DesignSize = (
            713
            234)
          object lbl00: TLabel
            Left = 28
            Top = 9
            Width = 3
            Height = 13
            Anchors = []
            ExplicitLeft = 22
          end
          object lbl01: TLabel
            AlignWithMargins = True
            Left = 63
            Top = 3
            Width = 137
            Height = 28
            Align = alClient
            Alignment = taCenter
            AutoSize = False
            Caption = 'FRec1: DynamicRecord;'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            Transparent = False
            Layout = tlCenter
            ExplicitLeft = 190
            ExplicitTop = 12
            ExplicitWidth = 46
            ExplicitHeight = 13
          end
          object lbl02: TLabel
            AlignWithMargins = True
            Left = 206
            Top = 3
            Width = 138
            Height = 28
            Align = alClient
            Alignment = taCenter
            AutoSize = False
            Caption = 'FRec2: DynamicRecord;'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            Transparent = False
            Layout = tlCenter
            ExplicitLeft = 325
            ExplicitTop = 12
            ExplicitWidth = 46
            ExplicitHeight = 13
          end
          object lbl03: TLabel
            AlignWithMargins = True
            Left = 350
            Top = 3
            Width = 137
            Height = 28
            Align = alClient
            Alignment = taCenter
            AutoSize = False
            Caption = 'FIntf1: IDynamicRecord;'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            Transparent = False
            Layout = tlCenter
            ExplicitLeft = 447
            ExplicitTop = 12
            ExplicitWidth = 86
            ExplicitHeight = 13
          end
          object lbl04: TLabel
            AlignWithMargins = True
            Left = 493
            Top = 3
            Width = 137
            Height = 28
            Align = alClient
            Alignment = taCenter
            AutoSize = False
            Caption = 'FIntf2: IDynamicRecord;'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            Transparent = False
            Layout = tlCenter
            ExplicitLeft = 585
            ExplicitTop = 12
            ExplicitWidth = 86
            ExplicitHeight = 13
          end
          object lbl20: TLabel
            Left = 0
            Top = 61
            Width = 60
            Height = 30
            Align = alClient
            Caption = 'FRec2 :='
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            Layout = tlCenter
            ExplicitTop = 63
            ExplicitWidth = 48
            ExplicitHeight = 13
          end
          object lbl30: TLabel
            Left = 0
            Top = 91
            Width = 60
            Height = 30
            Align = alClient
            Caption = 'FIntf1 :='
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            Layout = tlCenter
            ExplicitTop = 93
            ExplicitWidth = 54
            ExplicitHeight = 13
          end
          object lbl40: TLabel
            Left = 0
            Top = 121
            Width = 60
            Height = 29
            Align = alClient
            Caption = 'FIntf2 :='
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            Layout = tlCenter
            ExplicitTop = 123
            ExplicitWidth = 54
            ExplicitHeight = 13
          end
          object lbl15: TLabel
            Left = 0
            Top = 31
            Width = 60
            Height = 30
            Align = alClient
            Caption = 'FRec1 :='
            Color = clBlack
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            Transparent = True
            Layout = tlCenter
            ExplicitTop = 32
            ExplicitWidth = 48
            ExplicitHeight = 13
          end
          object btnClearFRecord1: TButton
            AlignWithMargins = True
            Left = 63
            Top = 34
            Width = 137
            Height = 24
            Action = actFRecord1Clear
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            ExplicitTop = 35
            ExplicitHeight = 25
          end
          object btnFRecord2Clear1: TButton
            AlignWithMargins = True
            Left = 206
            Top = 64
            Width = 138
            Height = 24
            Action = actFRecord2Clear
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            ExplicitTop = 66
          end
          object btnFDynamicRecord1Clear1: TButton
            AlignWithMargins = True
            Left = 350
            Top = 94
            Width = 137
            Height = 24
            Action = actFDynamicRecord1Clear
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 2
            ExplicitTop = 96
          end
          object btnFDynamicRecord2Clear: TButton
            AlignWithMargins = True
            Left = 493
            Top = 124
            Width = 137
            Height = 23
            Action = actFDynamicRecord2Clear
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 3
            ExplicitTop = 126
            ExplicitHeight = 24
          end
          object btnAssignFRecord2ToFRecord1: TButton
            AlignWithMargins = True
            Left = 206
            Top = 34
            Width = 138
            Height = 24
            Action = actAssignFRecord2ToFRecord1
            Align = alClient
            TabOrder = 4
            ExplicitTop = 35
            ExplicitHeight = 25
          end
          object btnAssignFDynamicRecord1ToFRecord1: TButton
            AlignWithMargins = True
            Left = 350
            Top = 34
            Width = 137
            Height = 24
            Action = actAssignFDynamicRecord1ToFRecord1
            Align = alClient
            TabOrder = 5
            ExplicitTop = 35
            ExplicitHeight = 25
          end
          object btnAssignFDynamicRecord2ToFRecord1: TButton
            AlignWithMargins = True
            Left = 493
            Top = 34
            Width = 137
            Height = 24
            Action = actAssignFDynamicRecord2ToFRecord1
            Align = alClient
            TabOrder = 6
            ExplicitTop = 35
            ExplicitHeight = 25
          end
          object btnAssignFRecord1ToFRecord4: TButton
            AlignWithMargins = True
            Left = 63
            Top = 64
            Width = 137
            Height = 24
            Action = actAssignFRecord1ToFRecord2
            Align = alClient
            TabOrder = 7
            ExplicitTop = 66
          end
          object btnAssignFDynamicRecord1ToFRecord2: TButton
            AlignWithMargins = True
            Left = 350
            Top = 64
            Width = 137
            Height = 24
            Action = actAssignFDynamicRecord1ToFRecord2
            Align = alClient
            TabOrder = 8
            ExplicitTop = 66
          end
          object btnAssignFDynamicRecord2ToFRecord2: TButton
            AlignWithMargins = True
            Left = 493
            Top = 64
            Width = 137
            Height = 24
            Action = actAssignFDynamicRecord2ToFRecord2
            Align = alClient
            TabOrder = 9
            ExplicitTop = 66
          end
          object btnAssignFRecord1ToFDynamicRecord1: TButton
            AlignWithMargins = True
            Left = 63
            Top = 94
            Width = 137
            Height = 24
            Action = actAssignFRecord1ToFDynamicRecord1
            Align = alClient
            TabOrder = 10
            ExplicitTop = 96
          end
          object btnAssignFRecord2ToFDynamicRecord1: TButton
            AlignWithMargins = True
            Left = 206
            Top = 94
            Width = 138
            Height = 24
            Action = actAssignFRecord2ToFDynamicRecord1
            Align = alClient
            TabOrder = 11
            ExplicitTop = 96
          end
          object btnAssignFDynamicRecord2ToFDynamicRecord1: TButton
            AlignWithMargins = True
            Left = 493
            Top = 94
            Width = 137
            Height = 24
            Action = actAssignFDynamicRecord2ToFDynamicRecord1
            Align = alClient
            TabOrder = 12
            ExplicitTop = 96
          end
          object btnAssignFRecord1ToFDynamicRecord2: TButton
            AlignWithMargins = True
            Left = 63
            Top = 124
            Width = 137
            Height = 23
            Action = actAssignFRecord1ToFDynamicRecord2
            Align = alClient
            TabOrder = 13
            ExplicitTop = 126
            ExplicitHeight = 24
          end
          object btnAssignFRecord2ToFDynamicRecord2: TButton
            AlignWithMargins = True
            Left = 206
            Top = 124
            Width = 138
            Height = 23
            Action = actAssignFRecord2ToFDynamicRecord2
            Align = alClient
            TabOrder = 14
            ExplicitTop = 126
            ExplicitHeight = 24
          end
          object btnAssignFDynamicRecord1ToFDynamicRecord2: TButton
            AlignWithMargins = True
            Left = 350
            Top = 124
            Width = 137
            Height = 23
            Action = actAssignFDynamicRecord1ToFDynamicRecord2
            Align = alClient
            TabOrder = 15
            ExplicitTop = 126
            ExplicitHeight = 24
          end
          object btnAssignFieldValueToFRecord1: TButton
            AlignWithMargins = True
            Left = 636
            Top = 34
            Width = 74
            Height = 24
            Action = actAssignFieldValueToFRecord1
            Align = alClient
            TabOrder = 16
            ExplicitTop = 35
            ExplicitHeight = 25
          end
          object btnAssignFieldValueToFRecord2: TButton
            AlignWithMargins = True
            Left = 636
            Top = 64
            Width = 74
            Height = 24
            Action = actAssignFieldValueToFRecord2
            Align = alClient
            TabOrder = 17
            ExplicitTop = 66
          end
          object btnAssignFieldValueToDynamicRecord1: TButton
            AlignWithMargins = True
            Left = 636
            Top = 94
            Width = 74
            Height = 24
            Action = actAssignFieldValueToDynamicRecord1
            Align = alClient
            TabOrder = 18
            ExplicitTop = 96
          end
          object btnAssignFieldValueToDynamicRecord2: TButton
            AlignWithMargins = True
            Left = 636
            Top = 124
            Width = 74
            Height = 23
            Action = actAssignFieldValueToDynamicRecord2
            Align = alClient
            TabOrder = 19
            ExplicitTop = 126
            ExplicitHeight = 24
          end
          object lblFRecord1: TLabel
            AlignWithMargins = True
            Left = 63
            Top = 153
            Width = 137
            Height = 81
            Align = alClient
            AutoSize = False
            Color = clCream
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            Transparent = False
            ExplicitLeft = -6
            ExplicitTop = 42
            ExplicitWidth = 211
            ExplicitHeight = 39
          end
          object lblFRecord2: TLabel
            AlignWithMargins = True
            Left = 206
            Top = 153
            Width = 138
            Height = 81
            Align = alClient
            AutoSize = False
            Color = clCream
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            Transparent = False
            ExplicitLeft = 312
            ExplicitTop = 13
            ExplicitWidth = 3
            ExplicitHeight = 13
          end
          object lblFDynamicRecord1: TLabel
            AlignWithMargins = True
            Left = 350
            Top = 153
            Width = 137
            Height = 81
            Align = alClient
            AutoSize = False
            Color = clCream
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            Transparent = False
            ExplicitLeft = 517
            ExplicitTop = 13
            ExplicitWidth = 3
            ExplicitHeight = 13
          end
          object lblFDynamicRecord2: TLabel
            AlignWithMargins = True
            Left = 493
            Top = 153
            Width = 137
            Height = 81
            Align = alClient
            AutoSize = False
            Color = clCream
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            Transparent = False
            ExplicitLeft = 600
            ExplicitTop = 172
            ExplicitWidth = 136
            ExplicitHeight = 23
          end
          object pnlField: TPanel
            Left = 633
            Top = 150
            Width = 80
            Height = 84
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 20
            ExplicitTop = 153
            ExplicitHeight = 85
            DesignSize = (
              80
              84)
            object edtFieldName: TLabeledEdit
              Left = 3
              Top = 18
              Width = 68
              Height = 21
              Anchors = [akLeft, akRight]
              EditLabel.Width = 57
              EditLabel.Height = 13
              EditLabel.Caption = 'FieldName:'
              TabOrder = 0
              Text = 'Test'
            end
            object edtValue: TLabeledEdit
              Left = 3
              Top = 57
              Width = 68
              Height = 21
              Anchors = [akLeft, akRight]
              EditLabel.Width = 56
              EditLabel.Height = 13
              EditLabel.Caption = 'FieldValue:'
              TabOrder = 1
              Text = '8'
              ExplicitTop = 58
            end
          end
        end
      end
    end
    object btnTestAssign: TButton
      Left = 3
      Top = 3
      Width = 57
      Height = 25
      Action = actTestAssign
      TabOrder = 1
    end
    object btnTestAssignTo: TButton
      Left = 3
      Top = 34
      Width = 57
      Height = 25
      Action = actTestAssignTo
      TabOrder = 2
    end
    object btnTestAssignTo1: TButton
      Left = 3
      Top = 65
      Width = 57
      Height = 25
      Action = actToStrings
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object btnTestData: TButton
      Left = 3
      Top = 94
      Width = 57
      Height = 25
      Action = actTestData
      TabOrder = 4
    end
    object btnCustomTest: TButton
      Left = 3
      Top = 125
      Width = 57
      Height = 25
      Action = actCustomTest
      Caption = 'Custom'
      TabOrder = 5
    end
  end
  object aclMain: TActionList
    Left = 16
    Top = 320
    object actTestAssignTo: TAction
      Category = 'Test'
      Caption = 'AssignTo'
    end
    object actTestAssign: TAction
      Category = 'Test'
      Caption = 'Assign'
      OnExecute = actTestAssignExecute
    end
    object actToStrings: TAction
      Caption = 'ToStrings'
      Hint = 'Puts the content in a TStrings instance as key/value pairs.'
      OnExecute = actToStringsExecute
    end
    object actTestData: TAction
      Category = 'Test'
      Caption = 'TestData'#13#10
      OnExecute = actTestDataExecute
    end
    object actFRecord1Clear: TAction
      Caption = 'Clear'
      Hint = 'Clears the fieldlist of FRecord1'
      OnExecute = actFRecord1ClearExecute
    end
    object actFRecord2Clear: TAction
      Caption = 'Clear'
      Hint = 'Clears the fieldlist of FRecord2'
      OnExecute = actFRecord2ClearExecute
    end
    object actFDynamicRecord1Clear: TAction
      Caption = 'Clear'
      Hint = 'Clears the fieldlist of FDynamicRecord1.'
      OnExecute = actFDynamicRecord1ClearExecute
    end
    object actFDynamicRecord2Clear: TAction
      Caption = 'Clear'
      Hint = 'Clears the fieldlist of FDynamicRecord2.'
      OnExecute = actFDynamicRecord2ClearExecute
    end
    object actAssignFieldValueToFRecord1: TAction
      Caption = 'Assign Field'
      Hint = 'Assign a field value to FRecord1.'
      OnExecute = actAssignFieldValueToFRecord1Execute
    end
    object actAssignFRecord1ToFDynamicRecord1: TAction
      Caption = 'Assign'
      Hint = 'Assign a field value to FDynamicRecord1.'
      OnExecute = actAssignFRecord1ToFDynamicRecord1Execute
    end
    object actAssignFRecord1ToFRecord2: TAction
      Caption = 'Assign'
      Hint = 'Assign FRecord1 to FRecord2.'
      OnExecute = actAssignFRecord1ToFRecord2Execute
    end
    object actAssignFieldValueToFRecord2: TAction
      Caption = 'Assign Field'
      Hint = 'Assign a value to FRecord2.'
      OnExecute = actAssignFieldValueToFRecord2Execute
    end
    object actAssignFDynamicRecord1ToFDynamicRecord2: TAction
      Caption = 'Assign'
      OnExecute = actAssignFDynamicRecord1ToFDynamicRecord2Execute
    end
    object actAssignFieldValueToDynamicRecord2: TAction
      Caption = 'Assign Field'
      Hint = 'Assign a field value to FDynamicRecord2.'
      OnExecute = actAssignFieldValueToDynamicRecord2Execute
    end
    object actCustomTest: TAction
      Caption = 'Custom test'
      OnExecute = actCustomTestExecute
    end
    object actAssignFieldValueToDynamicRecord1: TAction
      Caption = 'Assign Field'
      Hint = 'Assign a field value to FDynamicRecord1.'
      OnExecute = actAssignFieldValueToDynamicRecord1Execute
    end
    object actAssignFRecord2ToFRecord1: TAction
      Caption = 'Assign'
      OnExecute = actAssignFRecord2ToFRecord1Execute
    end
    object actAssignFDynamicRecord2ToFRecord1: TAction
      Caption = 'Assign'
      OnExecute = actAssignFDynamicRecord2ToFRecord1Execute
    end
    object actAssignFDynamicRecord2ToFDynamicRecord1: TAction
      Caption = 'Assign'
      OnExecute = actAssignFDynamicRecord2ToFDynamicRecord1Execute
    end
    object actAssignFDynamicRecord2ToFRecord2: TAction
      Caption = 'Assign'
      OnExecute = actAssignFDynamicRecord2ToFRecord2Execute
    end
    object actAssignFDynamicRecord1ToFRecord1: TAction
      Caption = 'Assign'
      OnExecute = actAssignFDynamicRecord1ToFRecord1Execute
    end
    object actAssignFDynamicRecord1ToFRecord2: TAction
      Caption = 'Assign'
      OnExecute = actAssignFDynamicRecord1ToFRecord2Execute
    end
    object actAssignFRecord1ToFDynamicRecord2: TAction
      Caption = 'Assign'
      OnExecute = actAssignFRecord1ToFDynamicRecord2Execute
    end
    object actAssignFRecord2ToFDynamicRecord1: TAction
      Caption = 'Assign'
      OnExecute = actAssignFRecord2ToFDynamicRecord1Execute
    end
    object actAssignFRecord2ToFDynamicRecord2: TAction
      Caption = 'Assign'
      OnExecute = actAssignFRecord2ToFDynamicRecord2Execute
    end
  end
  object dscTest: TDataSource
    OnDataChange = dscTestDataChange
    Left = 104
    Top = 320
  end
end
