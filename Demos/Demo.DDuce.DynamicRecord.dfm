object frmDynamicRecords: TfrmDynamicRecords
  Left = 0
  Top = 0
  ClientHeight = 673
  ClientWidth = 966
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object spl1: TSplitter
    Left = 0
    Top = 274
    Width = 966
    Height = 6
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 275
  end
  object pnlBottom: TGridPanel
    Left = 0
    Top = 280
    Width = 966
    Height = 393
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
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
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
      Width = 356
      Height = 387
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object pnlRecordInspectorHeader: TPanel
        Left = 0
        Top = 0
        Width = 356
        Height = 19
        Align = alTop
        BevelKind = bkFlat
        BevelOuter = bvNone
        Caption = 'TRecord content'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentFont = False
        TabOrder = 0
      end
    end
    object pnlBottomRight: TPanel
      Left = 362
      Top = 0
      Width = 604
      Height = 393
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object pnlRightBottomHeader: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 598
        Height = 19
        Align = alTop
        BevelKind = bkFlat
        BevelOuter = bvNone
        Caption = 'Conversion methods'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentFont = False
        TabOrder = 0
      end
      object pnlTRecordRepresentations: TGridPanel
        Left = 0
        Top = 25
        Width = 604
        Height = 368
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
          Width = 598
          Height = 86
          Align = alClient
          Caption = 'AsCommaText'
          TabOrder = 0
          object mmoAsCommaText: TMemo
            AlignWithMargins = True
            Left = 5
            Top = 18
            Width = 588
            Height = 66
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
          Top = 95
          Width = 598
          Height = 85
          Align = alClient
          Caption = 'AsDelimitedText'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
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
            EditLabel.Width = 45
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
            EditLabel.Width = 58
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
            Width = 413
            Height = 65
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
          Top = 186
          Width = 598
          Height = 85
          Align = alClient
          Caption = 'ToStrings'
          TabOrder = 2
          object mmoToStrings: TMemo
            AlignWithMargins = True
            Left = 5
            Top = 18
            Width = 588
            Height = 65
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
          Top = 277
          Width = 598
          Height = 88
          Align = alClient
          Caption = 'ToString'
          TabOrder = 3
          DesignSize = (
            598
            88)
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
            Width = 497
            Height = 68
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
    Width = 966
    Height = 274
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      966
      274)
    object pgcMain: TPageControl
      Left = 97
      Top = 8
      Width = 869
      Height = 266
      ActivePage = tsContactObject
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      object tsContactObject: TTabSheet
        Caption = 'TContact object'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblContact: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 855
          Height = 235
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
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object grdTest: TDBGrid
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 855
          Height = 232
          Align = alClient
          DataSource = dscTest
          Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
          TabOrder = 0
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Tahoma'
          TitleFont.Style = []
        end
      end
      object tsTestClass: TTabSheet
        Caption = 'TTestClass'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblTestClass: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 855
          Height = 235
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
          ExplicitTop = 0
          ExplicitWidth = 245
          ExplicitHeight = 364
        end
      end
      object tsTestRecord: TTabSheet
        Caption = 'TTestRecord'
        ImageIndex = 3
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblTestRecord: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 855
          Height = 235
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
      object tsTRecord: TTabSheet
        Caption = 'TRecord'
        ImageIndex = 4
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblTestTRecord: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 855
          Height = 235
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
          ExplicitTop = 0
          ExplicitWidth = 245
          ExplicitHeight = 364
        end
      end
      object tsAssignments: TTabSheet
        Caption = 'Assignments'
        ImageIndex = 6
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pnlAssignments: TGridPanel
          Left = 0
          Top = 0
          Width = 861
          Height = 238
          Align = alClient
          BevelOuter = bvNone
          Color = clWhite
          ColumnCollection = <
            item
              Value = 16.848372591888310000
            end
            item
              Value = 16.706459051760200000
            end
            item
              Value = 16.613473967846160000
            end
            item
              Value = 16.573890671562420000
            end
            item
              Value = 16.591137050276250000
            end
            item
              Value = 16.666666666666670000
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
          DesignSize = (
            861
            238)
          object lbl00: TLabel
            Left = 71
            Top = 9
            Width = 3
            Height = 13
            Anchors = []
            ExplicitLeft = 22
          end
          object lbl01: TLabel
            AlignWithMargins = True
            Left = 148
            Top = 3
            Width = 137
            Height = 28
            Align = alClient
            Alignment = taCenter
            AutoSize = False
            Caption = 'FRecord1'
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
            Left = 291
            Top = 3
            Width = 137
            Height = 28
            Align = alClient
            Alignment = taCenter
            AutoSize = False
            Caption = 'FRecord2'
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
            Left = 434
            Top = 3
            Width = 136
            Height = 28
            Align = alClient
            Alignment = taCenter
            AutoSize = False
            Caption = 'FDynamicRecord1'
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
            Left = 576
            Top = 3
            Width = 136
            Height = 28
            Align = alClient
            Alignment = taCenter
            AutoSize = False
            Caption = 'FDynamicRecord2'
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
            Width = 145
            Height = 30
            Align = alClient
            Caption = 'FRecord2 :='
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            Layout = tlCenter
            ExplicitWidth = 66
            ExplicitHeight = 13
          end
          object lbl30: TLabel
            Left = 0
            Top = 91
            Width = 145
            Height = 30
            Align = alClient
            Caption = 'FDynamicRecord1 :='
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            Layout = tlCenter
            ExplicitWidth = 108
            ExplicitHeight = 13
          end
          object lbl40: TLabel
            Left = 0
            Top = 121
            Width = 145
            Height = 29
            Align = alClient
            Caption = 'FDynamicRecord2 :='
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            Layout = tlCenter
            ExplicitWidth = 108
            ExplicitHeight = 13
          end
          object lbl15: TLabel
            Left = 0
            Top = 31
            Width = 145
            Height = 30
            Align = alClient
            Caption = 'FRecord1 :='
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
            ExplicitWidth = 66
            ExplicitHeight = 13
          end
          object btnClearFRecord1: TButton
            AlignWithMargins = True
            Left = 148
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
          end
          object btnFRecord2Clear1: TButton
            AlignWithMargins = True
            Left = 291
            Top = 64
            Width = 137
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
          end
          object btnFDynamicRecord1Clear1: TButton
            AlignWithMargins = True
            Left = 434
            Top = 94
            Width = 136
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
          end
          object btnFDynamicRecord2Clear: TButton
            AlignWithMargins = True
            Left = 576
            Top = 124
            Width = 136
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
          end
          object btnAssignFRecord2ToFRecord1: TButton
            AlignWithMargins = True
            Left = 291
            Top = 34
            Width = 137
            Height = 24
            Action = actAssignFRecord2ToFRecord1
            Align = alClient
            TabOrder = 4
          end
          object btnAssignFDynamicRecord1ToFRecord1: TButton
            AlignWithMargins = True
            Left = 434
            Top = 34
            Width = 136
            Height = 24
            Action = actAssignFDynamicRecord1ToFRecord1
            Align = alClient
            TabOrder = 5
          end
          object btnAssignFDynamicRecord2ToFRecord1: TButton
            AlignWithMargins = True
            Left = 576
            Top = 34
            Width = 136
            Height = 24
            Action = actAssignFDynamicRecord2ToFRecord1
            Align = alClient
            TabOrder = 6
          end
          object btnAssignFRecord1ToFRecord4: TButton
            AlignWithMargins = True
            Left = 148
            Top = 64
            Width = 137
            Height = 24
            Action = actAssignFRecord1ToFRecord2
            Align = alClient
            TabOrder = 7
          end
          object btnAssignFDynamicRecord1ToFRecord2: TButton
            AlignWithMargins = True
            Left = 434
            Top = 64
            Width = 136
            Height = 24
            Action = actAssignFDynamicRecord1ToFRecord2
            Align = alClient
            TabOrder = 8
          end
          object btnAssignFDynamicRecord2ToFRecord2: TButton
            AlignWithMargins = True
            Left = 576
            Top = 64
            Width = 136
            Height = 24
            Action = actAssignFDynamicRecord2ToFRecord2
            Align = alClient
            TabOrder = 9
          end
          object btnAssignFRecord1ToFDynamicRecord1: TButton
            AlignWithMargins = True
            Left = 148
            Top = 94
            Width = 137
            Height = 24
            Action = actAssignFRecord1ToFDynamicRecord1
            Align = alClient
            TabOrder = 10
          end
          object btnAssignFRecord2ToFDynamicRecord1: TButton
            AlignWithMargins = True
            Left = 291
            Top = 94
            Width = 137
            Height = 24
            Action = actAssignFRecord2ToFDynamicRecord1
            Align = alClient
            TabOrder = 11
          end
          object btnAssignFDynamicRecord2ToFDynamicRecord1: TButton
            AlignWithMargins = True
            Left = 576
            Top = 94
            Width = 136
            Height = 24
            Action = actAssignFDynamicRecord2ToFDynamicRecord1
            Align = alClient
            TabOrder = 12
          end
          object btnAssignFRecord1ToFDynamicRecord2: TButton
            AlignWithMargins = True
            Left = 148
            Top = 124
            Width = 137
            Height = 23
            Action = actAssignFRecord1ToFDynamicRecord2
            Align = alClient
            TabOrder = 13
          end
          object btnAssignFRecord2ToFDynamicRecord2: TButton
            AlignWithMargins = True
            Left = 291
            Top = 124
            Width = 137
            Height = 23
            Action = actAssignFRecord2ToFDynamicRecord2
            Align = alClient
            TabOrder = 14
          end
          object btnAssignFDynamicRecord1ToFDynamicRecord2: TButton
            AlignWithMargins = True
            Left = 434
            Top = 124
            Width = 136
            Height = 23
            Action = actAssignFDynamicRecord1ToFDynamicRecord2
            Align = alClient
            TabOrder = 15
          end
          object btnAssignFieldValueToFRecord1: TButton
            AlignWithMargins = True
            Left = 718
            Top = 34
            Width = 140
            Height = 24
            Action = actAssignFieldValueToFRecord1
            Align = alClient
            TabOrder = 16
          end
          object btnAssignFieldValueToFRecord2: TButton
            AlignWithMargins = True
            Left = 718
            Top = 64
            Width = 140
            Height = 24
            Action = actAssignFieldValueToFRecord2
            Align = alClient
            TabOrder = 17
          end
          object btnAssignFieldValueToDynamicRecord1: TButton
            AlignWithMargins = True
            Left = 718
            Top = 94
            Width = 140
            Height = 24
            Action = actAssignFieldValueToDynamicRecord1
            Align = alClient
            TabOrder = 18
          end
          object btnAssignFieldValueToDynamicRecord2: TButton
            AlignWithMargins = True
            Left = 718
            Top = 124
            Width = 140
            Height = 23
            Action = actAssignFieldValueToDynamicRecord2
            Align = alClient
            TabOrder = 19
          end
          object lblFRecord1: TLabel
            AlignWithMargins = True
            Left = 148
            Top = 153
            Width = 137
            Height = 85
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
            Left = 291
            Top = 153
            Width = 137
            Height = 85
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
            Left = 434
            Top = 153
            Width = 136
            Height = 85
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
            Left = 576
            Top = 153
            Width = 136
            Height = 85
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
            ExplicitHeight = 23
          end
          object pnlField: TPanel
            Left = 715
            Top = 150
            Width = 146
            Height = 88
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 20
            DesignSize = (
              146
              88)
            object edtFieldName: TLabeledEdit
              Left = 3
              Top = 19
              Width = 134
              Height = 21
              Anchors = [akLeft, akRight]
              EditLabel.Width = 53
              EditLabel.Height = 13
              EditLabel.Caption = 'FieldName:'
              TabOrder = 0
              Text = 'Test'
            end
            object edtValue: TLabeledEdit
              Left = 3
              Top = 60
              Width = 134
              Height = 21
              Anchors = [akLeft, akRight]
              EditLabel.Width = 52
              EditLabel.Height = 13
              EditLabel.Caption = 'FieldValue:'
              TabOrder = 1
              Text = '8'
            end
          end
        end
      end
    end
    object btnTestAssign: TButton
      Left = 8
      Top = 8
      Width = 83
      Height = 25
      Action = actTestAssign
      TabOrder = 1
    end
    object btnTestAssignTo: TButton
      Left = 8
      Top = 39
      Width = 83
      Height = 25
      Action = actTestAssignTo
      TabOrder = 2
    end
    object btnTestAssignTo1: TButton
      Left = 8
      Top = 70
      Width = 83
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
      Left = 8
      Top = 99
      Width = 83
      Height = 25
      Action = actTestData
      TabOrder = 4
    end
    object btnCustomTest: TButton
      Left = 8
      Top = 130
      Width = 83
      Height = 25
      Action = actCustomTest
      TabOrder = 5
    end
  end
  object aclMain: TActionList
    Left = 24
    Top = 160
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
  object dsTest: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 56
    Top = 320
  end
  object dscTest: TDataSource
    DataSet = dsTest
    OnDataChange = dscTestDataChange
    Left = 104
    Top = 320
  end
end
