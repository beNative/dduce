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
        Color = clAppWorkSpace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
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
        Color = clAppWorkSpace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
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
    PersistDataPacket.Data = {
      171D00009619E0BD01000000180000000D003700000003000000630106437573
      744E6F080004000000000007436F6D70616E7901004900000001000557494454
      48020002001E000541646472310100490000000100055749445448020002001E
      000541646472320100490000000100055749445448020002001E000443697479
      0100490000000100055749445448020002000F00055374617465010049000000
      0100055749445448020002001400035A69700100490000000100055749445448
      020002000A0007436F756E747279010049000000010005574944544802000200
      14000550686F6E650100490000000100055749445448020002000F0003464158
      0100490000000100055749445448020002000F00075461785261746508000400
      0000000007436F6E746163740100490000000100055749445448020002001400
      0F4C617374496E766F6963654461746508000800000000000100044C43494404
      0001000904000000000000000000000000149340114B61756169204469766520
      53686F70706513342D3937362053756761726C6F616620487779095375697465
      203130330B4B61706161204B617561690248490A39343736362D313233340255
      530C3830382D3535352D303236390C3830382D3535352D303237380000000000
      0021400C4572696361204E6F726D616E000CDBC6B59DCC420040140000000000
      00003C934006556E6973636F0C504F20426F78205A2D3534370846726565706F
      727407426168616D61730C3830392D3535352D333931350C3830392D3535352D
      3439353800000000000000000F47656F72676520576561746865727300D497E9
      F09CCC42004014000000000000001C95400B53696768742044697665720E3120
      4E657074756E65204C616E650B4B61746F20506170686F73064379707275730C
      3335372D362D3837363730380C3335372D362D38373039343300000000000000
      000F5068796C6C69732053706F6F6E657200181638A49CCC4200401400000000
      0000002895401D4361796D616E2044697665727320576F726C6420556E6C696D
      697465640A504F20426F78203534310C4772616E64204361796D616E13427269
      74697368205765737420496E646965730C3031312D352D3639373034340C3031
      312D352D36393730363400000000000000000A4A6F65204261696C657900E0F5
      09A892CC420040000000000000000030954018546F6D20536177796572204469
      76696E672043656E747265153633322D312054686972642046727964656E686F
      6A0D43687269737469616E737465640953742E2043726F697805303038323011
      55532056697267696E2049736C616E64730C3530342D3739382D333032320C35
      30342D3739382D3737373200000000000000000C43687269732054686F6D6173
      00F01A992993CC420000000000000000000090954015426C7565204A61636B20
      417175612043656E7465721632332D3733382050616464696E67746F6E204C61
      6E65095375697465203331300757616970616875024849053939373736025553
      0C3430312D3630392D373632330C3430312D3630392D39343033000000000000
      00000E45726E6573742042617272617474008078B9DA9CCC4200400000000000
      000000A095400F5649502044697665727320436C75620B3332204D61696E2053
      742E0D43687269737469616E737465640953742E2043726F6978053032383030
      1155532056697267696E2049736C616E64730C3830392D3435332D353937360C
      3830392D3435332D3539333200000000000000001352757373656C6C20436872
      6973746F7068657200DC0E19B59DCC42004000000000000000009897400E4F63
      65616E2050617261646973650B504F20426F7820383734350B4B61696C75612D
      4B6F6E610248490539343735360255530C3830382D3535352D383233310C3830
      382D3535352D3834353000000000000000000C5061756C20476172646E657200
      5882F0DA9CCC4200401400000000000000A497401446616E7461737469717565
      204171756174696361145A33322039393920233132412D373720412E412E0642
      6F676F746108436F6C756D6269610C3035372D312D3737333433340C3035372D
      312D37373334323100000000000000000A537573616E20576F6E670064071BB7
      9BCC42004000000000000000003C9840124D61726D6F74204469766572732043
      6C75620D38373220517565656E2053742E094B69746368656E6572074F6E7461
      72696F0747334E203245310643616E6164610C3431362D3639382D303339390C
      3432362D3639382D3033393900000000000000000B4A6F796365204D61727368
      0008486FE78CCC42004000000000000000006098401054686520446570746820
      43686172676515313532343320556E6465727761746572204677792E084D6172
      6174686F6E02464C0533353030330255530C3830302D3535352D333739380C38
      30302D3535352D3033353300000000000000000F53616D205769746865727370
      6F6F6E0030E2A4B49ACC42004000000000000000006C98400B426C7565205370
      6F727473153230332031327468204176652E20426F7820373436094769726962
      616C6469024F520539313138370255530C3631302D3737322D363730340C3631
      302D3737322D3638393800000000000000000D54686572657361204B756E6563
      00F80031FC8FCC4200400000000000000000609940104D616B61692053435542
      4120436C75620B504F20426F7820383533340B4B61696C75612D4B6F6E610248
      490539343735360255530C3331372D3634392D393039380C3331372D3634392D
      3637383700000000000000000B446F6E6E612053696175730028DE34749DCC42
      00400000000000000000B499400B416374696F6E20436C75620D504F20426F78
      20353435312D460853617261736F746102464C0533323237340255530C383133
      2D3837302D303233390C3831332D3837302D303238320000000000000000104D
      69636861656C20537075726C696E67009812D5F798CC42004010000000000000
      00CC9940144A616D616963612053435542412043656E74726509504F20426F78
      203638064E656772696C074A616D616963610B5765737420496E646965730C30
      31312D332D3639373034330C3031312D332D3639373034330000000000000000
      0E426172626172612048617276657900380E71BF94CC42004000000000000000
      00409A400E49736C616E642046696E64657273153631333320312F332053746F
      6E65204176656E75650E53742053696D6F6E732049736C650247410533323532
      310255530C3731332D3432332D353637350C3731332D3432332D353637360000
      0000000000000E4465736D6F6E64204F727465676100901CDD669DCC42004014
      00000000000000009F4012416476656E7475726520556E6465727365610A504F
      20426F78203734340B42656C697A6520436974790642656C697A650C3031312D
      33342D30393035340C3031312D33342D303930363400000000000000000F476C
      6F72696120476F6E7A616C657300980FD9D49CCC42004000000000000000008C
      A04010426C75652053706F72747320436C7562163633333635204E657A205065
      72636520537472656574054C6172676F02464C0533343638340255530C363132
      2D3839372D303334320C3631322D3839372D3033343800000000000000000E48
      617272792042617468626F6E65006864839D8CCC4200400000000000000000AE
      A040154672616E6B27732044697665727320537570706C791331343535204E6F
      72746820343474682053742E06457567656E65024F520539303432370255530C
      3530332D3535352D323737380C3530332D3535352D3237363900000000000000
      000D4C6C6F79642046656C6C6F777300C82EC61A89CC42004000000000000000
      00D8A0401244617679204A6F6E657327204C6F636B65721432343620536F7574
      68203136746820506C6163650956616E636F75766572024243074B3856203950
      310643616E6164610C3830332D3530392D303131320C3830332D3530392D3035
      353300000000000000000C54616E7961205761676E65720058886ACA91CC4200
      401400000000000000E6A0400C53435542412048656176656E0D504F20426F78
      20512D38383734064E617373617507426168616D61730C3031312D33322D3039
      3438350C3031312D33322D3039343835000000000000000010526F6265727420
      4D696368656C696E6400B0DAC69E8ECC4200401400000000000000EAA0401853
      68616E6772692D4C612053706F7274732043656E7465720D504F20426F782044
      2D353439350846726565706F727407426168616D61730C3031312D33322D3038
      3537340C3031312D33322D343439333800000000000000000E4672616E6B2050
      616E69616775610074867D3289CC420040100000000000000016A24015446976
      657273206F6620436F7266752C20496E632E114D61726D6F73657420506C6163
      652035340F4179696F73204D6174746861696F7305436F726675064772656563
      650C33302D3636312D38383336340C33302D3636312D30353934330000000000
      0000000D436861726C6573204C6F70657A004028065C9BCC4200400000000000
      00000064A240104B69726B20456E7465727072697365730C3432204171756120
      4C616E6507486F7573746F6E0254580537373037390255530C3731332D353536
      2D363433370C3731332D3535362D3130373300000000000000000D5275646F6C
      706820436C61757300545FD51A9ACC42004000000000000000003EA740114765
      6F726765204265616E202620436F2E13233733204B696E672053616C6D6F6E20
      576179064C75676F6666024E430532393037380255530C3830332D3433382D32
      3737310C3830332D3433382D3330303300000000000000000A42696C6C205779
      65727300386112AC9ACC420040000000000000000050A7401950726F66657373
      696F6E616C204469766572732C204C74642E1034373334204D656C696E646120
      53742E06486F6F76657202414C0533323134350255530C3230352D3535352D38
      3333330C3230352D3535352D3430353400000000000000000F536869726C6579
      204D617468657273001C4490F89ACC4200400000000000000000C2A740144469
      76657273206F6620426C75652D677265656E1036333420436F6D706C65782041
      76652E0650656C68616D02414C0533323134350255530C3230352D3535352D37
      3138340C3230352D3535352D3630353900000000000000000A4E616E63792042
      65616E00988CDA4E9ACC4200400000000000000000C4A74011476F6C6420436F
      61737420537570706C79133232332D4220486F7573746F6E20506C616365064D
      6F62696C6502414C0533303639360255530C3230352D3535352D323634300C32
      30352D3535352D3430393400000000000000000C456C61696E652046616C6C73
      00A0F704A69BCC4200400000000000000000D6A7401553616E205061626C6F20
      446976652043656E74657211313730312D44204E2042726F61647761790B5361
      6E7461204D617269610243410539353434330255530C3832332D3034342D3239
      31300C3832332D3034342D323939300000000000000000105061747269636961
      204F27427269656E0044EFE7BD9BCC4200400000000000000000D8A74015556E
      64657277617465722053706F72747320436F2E123335312D412053617261736F
      74612053742E0853616E204A6F73650243410539323139350255530C3430382D
      3836372D303539340C3430382D3836372D3030393400000000000000000C4461
      76652057616C6C696E6700F0E9F7659CCC4200400000000000000000DAA74015
      416D65726963616E20534355424120537570706C7914313733392041746C616E
      746963204176656E7565064C6F6D6974610243410539313737300255530C3231
      332D3635342D303039320C3231332D3635342D30303935000000000000000010
      4C796E6E2043696E6369726970696E69002CAD736C9DCC420040000000000000
      0000DCA74013436174616D6172616E204469766520436C756216426F78203236
      3420506C65617375726520506F696E740F436174616C696E612049736C616E64
      0243410539303734300255530C3231332D3232332D303934310C3231332D3232
      332D3233323400000000000000000D4E69636F6C65204475706F6E7400B06C4B
      A79CCC4200400000000000000000DEA7400E446976657227732047726F74746F
      14323436303120556E6976657273616C204C616E6506446F776E657902434105
      39343232300255530C3231332D3433322D303039330C3231332D3433322D3438
      323100000000000000000A5065746572204F77656E00148C75089DCC42004014
      000000000000009EA8400F4669736865726D616E2773204579650B504F20426F
      7820373534320C4772616E64204361796D616E13427269746973682057657374
      20496E646965730C3830392D3535352D343638300C3830392D3535352D343638
      3900000000000000000C42657468616E204C657769730030BCCCD99BCC420040
      0400000000000000ACA84013416374696F6E20446976657220537570706C7910
      426C7565205370617220426F782023330A53742E2054686F6D61730530303832
      301155532056697267696E2049736C616E64730C32322D34342D353030323131
      0C32322D34342D35303035393600000000000000000E4D617269616E6E65204D
      696C6573004C85DB7598CC42004014000000000000003EAC40134D6172696E61
      2053435542412043656E74657216504F20426F78203832343338205A756C7520
      3738333107436172616361730956656E657A75656C610B35382D33332D363632
      32320B35382D33332D363630343900000000000000000E5374657068656E2042
      7279616E7400180C805C97CC420040000000000000000020AF4014426C756520
      476C6173732048617070696E657373123633343520572E2053686F7265204C61
      6E650C53616E7461204D6F6E6963610243410539303431300255530C3231332D
      3535352D313938340C3231332D3535352D313939350000000000000000104368
      72697374696E65205461796C6F7200ACFD03169DCC4200400000000000000000
      D8B04010446976657273206F662056656E6963650E32323020456C6D20537472
      6565740656656E69636502464C0533393232340255530C3831332D3434332D32
      3335360C3831332D3434332D3938343200000000000000000C53696D6F6E6520
      477265656E00B823820A99CC4200400000000000000000B3B1400F4F6E2D5461
      7267657420534355424115372D3733373633204E616E616B61776120526F6164
      0857696E6E69706567084D616E69746F6261074A3252203554330643616E6164
      610C3431362D3434352D303938380C3431362D3434352D303232330000000000
      0000000D4272616D205068696C6C69707300C00E81CC99CC4200401000000000
      0000002CB240114A616D616963612053756E2C20496E632E0A504F20426F7820
      3634330B52756E6177617920426179074A616D616963610B5765737420496E64
      6965730C3830392D3535352D323734360C3830392D3535352D30393239000000
      00000000000D4A6F6E617468616E20576573740054C4A73A9DCC420040100000
      00000000004CB24012556E64657277617465722046616E746173790A504F2042
      6F7820383432094F63686F2052696F73074A616D616963610B5765737420496E
      646965730C3830392D3535352D323231340C3830392D3535352D323233340000
      0000000000000F4772616E742041696E73776F72746800E8BD1E199CCC420040
      14000000000000000CB440155072696E636573732049736C616E642053435542
      4111504F20426F78203332205761697965766F0754617665756E690446696A69
      0A3637392D3331313932330A3637392D33313132303300000000000000000D41
      6E6E65204D617269616368690034564F018BCC42004004000000000000001FB4
      401B43656E7472616C20556E646572776174657220537570706C6965730A504F
      20426F78203733370C4A6F68616E6E6573627572670432303432135265707562
      6C696320536F2E204166726963610D32372D31312D343433323435380D32372D
      31312D3434333332353900000000000000000E4D61726961204576656E746F73
      6800AC7945E59CCC42004014000000000000002BB4401453616661726920556E
      64657220746865205365610B504F20426F7820373435360C4772616E64204361
      796D616E1342726974697368205765737420496E646965730C3830392D343039
      2D343233330C3830392D3430392D33303032000000000000000009416E6E6120
      5261636B00A8F851A398CC42004000000000000000002DB440154C6172727927
      7320446976696E67205363686F6F6C1433353632204E57204272756365205374
      72656574094D696C7761756B6965024F520539363237370255530C3530332D34
      30332D373737370C3530332D3430332D3030353900000000000000000E497361
      62656C6C65204E656563650074F660229BCC420040140000000000000008B540
      0E546F726120546F726120546F72610D504F20426F7820482D34353733064E61
      7373617507426168616D61730C3830392D3839382D303034330C3830392D3839
      382D3938363400000000000000000B4B6576696E20526964657200989BFEBD9D
      CC420040000000000000000024B5400F566173686F6E2056656E747572657311
      373433204B6579686F6C6520436F75727408486F6E6F6C756C75024849053932
      3835360255530C3533322D3039392D303432330C3533322D3039392D36363534
      00000000000000000B537573616E20536D69746800583E61E399CC4200401400
      00000000000038B5400F4469766572732D666F722D486972650D472E4F2E2050
      20426F7820393104537576610446696A690A3637392D3830343537360A363739
      2D30353933343500000000000000000A4A6F652048617474657200203117FF99
      CC42004000000000000000008BB540104F6365616E20416476656E7475726573
      10504F20426F7820343636204B69686569044D61756902484905393537333602
      55530C3737362D3836382D393333340C3737362D3836382D3935353300000000
      000000000A5061756C205374696C6C00B05D11779DCC42004004000000000000
      0047B84018556E646572776174657220534355424120436F6D70616E790C504F
      20426F7820536E20393108536F6D6572736574045358424E074265726D756461
      0C3830392D3535352D313232350C3830392D3535352D32343435000000000000
      0000104D69636861656C2047726F73736D616E0068BC22B19ACC420040000000
      0000000000A8B8400D41717561746963204472616D6112393231204576657267
      6C61646573205761790554616D706102464C0533303634330255530C3631332D
      3434322D373635340C3631332D3434322D3736373800000000000000000C4769
      6C6C69616E204F77656E007008D62F88CC420040040000000000000074B94012
      54686520446976696E6720436F6D70616E790B504F20426F7820383533350A53
      742E2054686F6D61730530303832301155532056697267696E2049736C616E64
      730B32322D34342D35303039380B32322D34342D303938373800000000000000
      000B427269616E204D696C657300288AD3DC99CC4200400400000000000000B6
      B940184E6F7277657374276572205343554241204C696D697465640B504F2042
      6F782036383334055061676574045053425A074265726D7564610C3737382D31
      32332D303734350C3737382D3132332D3937303500000000000000000C416E67
      656C61204A6F6E657300E8CBF6B89DCC42004000000000000000009CBA401757
      6174657273706F75742053435542412043656E7465721237383635204E452042
      61726265722043742E08506F72746C616E64024F520539393237310255530C35
      30332D3635342D323433340C3530332D3635342D393938360000000000000000
      0E5269636861726420486F7573657200048A36919CCC42004010000000000000
      8038C340184E657074756E6527732054726964656E7420537570706C790A504F
      20426F7820313239064E656772696C074A616D616963610B5765737420496E64
      6965730C3737382D3839372D333534360C3737382D3839372D36363433000000
      00000000000D4C6F75697365204672616E6B7300446FBD72A0CC42}
    Active = True
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
