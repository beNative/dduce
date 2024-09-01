object frmDynamicRecords: TfrmDynamicRecords
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  ClientHeight = 914
  ClientWidth = 1185
  Color = clWindow
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  PixelsPerInch = 144
  TextHeight = 23
  object splHorizontal: TSplitter
    Left = 0
    Top = 363
    Width = 1185
    Height = 9
    Cursor = crVSplit
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    MinSize = 45
    ResizeStyle = rsLine
    ExplicitTop = 411
  end
  object pnlBottom: TGridPanel
    Left = 0
    Top = 372
    Width = 1185
    Height = 542
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
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
      Left = 5
      Top = 5
      Width = 434
      Height = 532
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 484
      object pnlRecordInspectorHeader: TPanel
        Left = 0
        Top = 0
        Width = 434
        Height = 29
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alTop
        BevelOuter = bvNone
        Caption = 'DynamicRecord content'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentFont = False
        TabOrder = 0
      end
    end
    object pnlBottomRight: TPanel
      Left = 444
      Top = 0
      Width = 741
      Height = 542
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitHeight = 494
      object pnlRightBottomHeader: TPanel
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 731
        Height = 28
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Conversion methods'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentFont = False
        TabOrder = 0
      end
      object pnlTRecordRepresentations: TGridPanel
        Left = 0
        Top = 38
        Width = 741
        Height = 504
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
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
        ExplicitHeight = 456
        object grpAsCommaText: TGroupBox
          AlignWithMargins = True
          Left = 5
          Top = 5
          Width = 731
          Height = 117
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          Caption = 'AsCommaText'
          TabOrder = 0
          ExplicitHeight = 105
          object mmoAsCommaText: TMemo
            AlignWithMargins = True
            Left = 7
            Top = 30
            Width = 717
            Height = 85
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
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
            Font.Height = -17
            Font.Name = 'Consolas'
            Font.Style = []
            ParentDoubleBuffered = False
            ParentFont = False
            ScrollBars = ssVertical
            TabOrder = 0
            ExplicitHeight = 73
          end
        end
        object grpAsDelimitedText: TGroupBox
          AlignWithMargins = True
          Left = 5
          Top = 132
          Width = 731
          Height = 115
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          Caption = 'AsDelimitedText'
          TabOrder = 1
          ExplicitTop = 120
          ExplicitHeight = 103
          object chkQuoteValues: TCheckBox
            Left = 17
            Top = 33
            Width = 124
            Height = 26
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Quote values'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnClick = chkQuoteValuesClick
          end
          object edtDelimiter: TLabeledEdit
            Left = 92
            Top = 71
            Width = 96
            Height = 29
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            EditLabel.Width = 73
            EditLabel.Height = 29
            EditLabel.Margins.Left = 5
            EditLabel.Margins.Top = 5
            EditLabel.Margins.Right = 5
            EditLabel.Margins.Bottom = 5
            EditLabel.Caption = 'Delimiter:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'Tahoma'
            Font.Style = []
            LabelPosition = lpLeft
            ParentFont = False
            TabOrder = 1
            Text = ';'
            OnChange = edtDelimiterChange
          end
          object edtQuoteChar: TLabeledEdit
            Left = 236
            Top = 30
            Width = 25
            Height = 31
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            EditLabel.Width = 90
            EditLabel.Height = 31
            EditLabel.Margins.Left = 5
            EditLabel.Margins.Top = 5
            EditLabel.Margins.Right = 5
            EditLabel.Margins.Bottom = 5
            EditLabel.Caption = 'Quote char:'
            LabelPosition = lpLeft
            MaxLength = 1
            TabOrder = 2
            Text = #39
            OnChange = edtQuoteCharChange
          end
          object mmoAsDelimitedText: TMemo
            AlignWithMargins = True
            Left = 270
            Top = 30
            Width = 454
            Height = 83
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
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
            Font.Height = -17
            Font.Name = 'Consolas'
            Font.Style = []
            ParentDoubleBuffered = False
            ParentFont = False
            ScrollBars = ssVertical
            TabOrder = 3
            ExplicitHeight = 71
          end
        end
        object grpToStrings: TGroupBox
          AlignWithMargins = True
          Left = 5
          Top = 257
          Width = 731
          Height = 116
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          Caption = 'ToStrings'
          TabOrder = 2
          ExplicitTop = 233
          ExplicitHeight = 104
          object mmoToStrings: TMemo
            AlignWithMargins = True
            Left = 7
            Top = 30
            Width = 717
            Height = 84
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
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
            Font.Height = -17
            Font.Name = 'Consolas'
            Font.Style = []
            ParentDoubleBuffered = False
            ParentFont = False
            ScrollBars = ssVertical
            TabOrder = 0
            ExplicitHeight = 72
          end
        end
        object grpToString: TGroupBox
          AlignWithMargins = True
          Left = 5
          Top = 383
          Width = 731
          Height = 116
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          Caption = 'ToString'
          TabOrder = 3
          ExplicitTop = 347
          ExplicitHeight = 104
          DesignSize = (
            731
            116)
          object chkAlignValues: TCheckBox
            Left = 17
            Top = 36
            Width = 118
            Height = 26
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Align values'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = chkAlignValuesClick
          end
          object mmoToString: TMemo
            AlignWithMargins = True
            Left = 144
            Top = 27
            Width = 580
            Height = 86
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
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
            Font.Height = -17
            Font.Name = 'Consolas'
            Font.Style = []
            ParentDoubleBuffered = False
            ParentFont = False
            ScrollBars = ssVertical
            TabOrder = 1
            ExplicitHeight = 74
          end
        end
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1185
    Height = 363
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitHeight = 411
    DesignSize = (
      1185
      363)
    object pgcMain: TPageControl
      Left = 95
      Top = 0
      Width = 1081
      Height = 353
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ActivePage = tsContactObject
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      ExplicitHeight = 329
      object tsContactObject: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'TContact object'
        object lblContact: TLabel
          Left = 0
          Top = 0
          Width = 1073
          Height = 315
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Align = alClient
          AutoSize = False
          Color = clInfoBk
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -17
          Font.Name = 'Consolas'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          Transparent = False
          ExplicitWidth = 1070
          ExplicitHeight = 351
        end
      end
      object tsDataSet: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'DataSet'
        ImageIndex = 1
        object grdTest: TDBGrid
          Left = 0
          Top = 0
          Width = 1073
          Height = 315
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          DataSource = dscTest
          Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
          TabOrder = 0
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -17
          TitleFont.Name = 'Segoe UI'
          TitleFont.Style = []
        end
      end
      object tsTestClass: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'TTestClass'
        ImageIndex = 2
        object lblTestClass: TLabel
          Left = 0
          Top = 0
          Width = 1073
          Height = 315
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Align = alClient
          AutoSize = False
          Color = clInfoBk
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -17
          Font.Name = 'Consolas'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          Transparent = False
          ExplicitWidth = 1070
          ExplicitHeight = 351
        end
      end
      object tsTestRecord: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'TTestRecord'
        ImageIndex = 3
        object lblTestRecord: TLabel
          Left = 0
          Top = 0
          Width = 1073
          Height = 315
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Align = alClient
          AutoSize = False
          Color = clInfoBk
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -17
          Font.Name = 'Consolas'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          Transparent = False
          ExplicitWidth = 1070
          ExplicitHeight = 351
        end
      end
      object tsDynamicRecord: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'DynamicRecord'
        ImageIndex = 4
        object lblTestTRecord: TLabel
          Left = 0
          Top = 0
          Width = 1073
          Height = 315
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Align = alClient
          AutoSize = False
          Color = clInfoBk
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -17
          Font.Name = 'Consolas'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          Transparent = False
          ExplicitWidth = 1070
          ExplicitHeight = 351
        end
      end
      object tsAssignments: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Assignments'
        ImageIndex = 6
        object pnlAssignments: TGridPanel
          Left = 0
          Top = 0
          Width = 1073
          Height = 315
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          BevelOuter = bvNone
          Color = clWhite
          ColumnCollection = <
            item
              SizeStyle = ssAbsolute
              Value = 90.000000000000000000
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
              Value = 120.000000000000000000
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
          ExplicitHeight = 359
          DesignSize = (
            1073
            315)
          object lbl00: TLabel
            Left = 42
            Top = 9
            Width = 5
            Height = 23
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Anchors = []
            ExplicitTop = 14
          end
          object lbl01: TLabel
            AlignWithMargins = True
            Left = 95
            Top = 5
            Width = 206
            Height = 37
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Align = alClient
            Alignment = taCenter
            AutoSize = False
            Caption = 'FRec1: DynamicRecord;'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            Transparent = False
            Layout = tlCenter
            ExplicitWidth = 205
            ExplicitHeight = 42
          end
          object lbl02: TLabel
            AlignWithMargins = True
            Left = 311
            Top = 5
            Width = 206
            Height = 37
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Align = alClient
            Alignment = taCenter
            AutoSize = False
            Caption = 'FRec2: DynamicRecord;'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            Transparent = False
            Layout = tlCenter
            ExplicitLeft = 309
            ExplicitWidth = 207
            ExplicitHeight = 42
          end
          object lbl03: TLabel
            AlignWithMargins = True
            Left = 527
            Top = 5
            Width = 205
            Height = 37
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Align = alClient
            Alignment = taCenter
            AutoSize = False
            Caption = 'FIntf1: IDynamicRecord;'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            Transparent = False
            Layout = tlCenter
            ExplicitLeft = 525
            ExplicitWidth = 206
            ExplicitHeight = 42
          end
          object lbl04: TLabel
            AlignWithMargins = True
            Left = 742
            Top = 5
            Width = 206
            Height = 37
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Align = alClient
            Alignment = taCenter
            AutoSize = False
            Caption = 'FIntf2: IDynamicRecord;'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            Transparent = False
            Layout = tlCenter
            ExplicitLeft = 740
            ExplicitWidth = 205
            ExplicitHeight = 42
          end
          object lbl20: TLabel
            Left = 0
            Top = 83
            Width = 90
            Height = 40
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Align = alClient
            Caption = 'FRec2 :='
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            Layout = tlCenter
            ExplicitTop = 93
            ExplicitWidth = 72
            ExplicitHeight = 20
          end
          object lbl30: TLabel
            Left = 0
            Top = 123
            Width = 90
            Height = 40
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Align = alClient
            Caption = 'FIntf1 :='
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            Layout = tlCenter
            ExplicitTop = 139
            ExplicitWidth = 81
            ExplicitHeight = 20
          end
          object lbl40: TLabel
            Left = 0
            Top = 163
            Width = 90
            Height = 39
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Align = alClient
            Caption = 'FIntf2 :='
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            Layout = tlCenter
            ExplicitTop = 184
            ExplicitWidth = 81
            ExplicitHeight = 20
          end
          object lbl15: TLabel
            Left = 0
            Top = 42
            Width = 90
            Height = 41
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Align = alClient
            Caption = 'FRec1 :='
            Color = clBlack
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'Consolas'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            Transparent = True
            Layout = tlCenter
            ExplicitTop = 47
            ExplicitWidth = 72
            ExplicitHeight = 20
          end
          object btnClearFRecord1: TButton
            AlignWithMargins = True
            Left = 95
            Top = 47
            Width = 206
            Height = 31
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actFRecord1Clear
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -17
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            ExplicitTop = 53
            ExplicitHeight = 36
          end
          object btnFRecord2Clear1: TButton
            AlignWithMargins = True
            Left = 311
            Top = 88
            Width = 206
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actFRecord2Clear
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            ExplicitTop = 99
            ExplicitHeight = 36
          end
          object btnFDynamicRecord1Clear1: TButton
            AlignWithMargins = True
            Left = 527
            Top = 128
            Width = 205
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actFDynamicRecord1Clear
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 2
            ExplicitTop = 145
            ExplicitHeight = 36
          end
          object btnFDynamicRecord2Clear: TButton
            AlignWithMargins = True
            Left = 742
            Top = 168
            Width = 206
            Height = 29
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actFDynamicRecord2Clear
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 3
            ExplicitTop = 191
            ExplicitHeight = 34
          end
          object btnAssignFRecord2ToFRecord1: TButton
            AlignWithMargins = True
            Left = 311
            Top = 47
            Width = 206
            Height = 31
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actAssignFRecord2ToFRecord1
            Align = alClient
            TabOrder = 4
            ExplicitTop = 53
            ExplicitHeight = 36
          end
          object btnAssignFDynamicRecord1ToFRecord1: TButton
            AlignWithMargins = True
            Left = 527
            Top = 47
            Width = 205
            Height = 31
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actAssignFDynamicRecord1ToFRecord1
            Align = alClient
            TabOrder = 5
            ExplicitTop = 53
            ExplicitHeight = 36
          end
          object btnAssignFDynamicRecord2ToFRecord1: TButton
            AlignWithMargins = True
            Left = 742
            Top = 47
            Width = 206
            Height = 31
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actAssignFDynamicRecord2ToFRecord1
            Align = alClient
            TabOrder = 6
            ExplicitTop = 53
            ExplicitHeight = 36
          end
          object btnAssignFRecord1ToFRecord4: TButton
            AlignWithMargins = True
            Left = 95
            Top = 88
            Width = 206
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actAssignFRecord1ToFRecord2
            Align = alClient
            TabOrder = 7
            ExplicitTop = 99
            ExplicitHeight = 36
          end
          object btnAssignFDynamicRecord1ToFRecord2: TButton
            AlignWithMargins = True
            Left = 527
            Top = 88
            Width = 205
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actAssignFDynamicRecord1ToFRecord2
            Align = alClient
            TabOrder = 8
            ExplicitTop = 99
            ExplicitHeight = 36
          end
          object btnAssignFDynamicRecord2ToFRecord2: TButton
            AlignWithMargins = True
            Left = 742
            Top = 88
            Width = 206
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actAssignFDynamicRecord2ToFRecord2
            Align = alClient
            TabOrder = 9
            ExplicitTop = 99
            ExplicitHeight = 36
          end
          object btnAssignFRecord1ToFDynamicRecord1: TButton
            AlignWithMargins = True
            Left = 95
            Top = 128
            Width = 206
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actAssignFRecord1ToFDynamicRecord1
            Align = alClient
            TabOrder = 10
            ExplicitTop = 145
            ExplicitHeight = 36
          end
          object btnAssignFRecord2ToFDynamicRecord1: TButton
            AlignWithMargins = True
            Left = 311
            Top = 128
            Width = 206
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actAssignFRecord2ToFDynamicRecord1
            Align = alClient
            TabOrder = 11
            ExplicitTop = 145
            ExplicitHeight = 36
          end
          object btnAssignFDynamicRecord2ToFDynamicRecord1: TButton
            AlignWithMargins = True
            Left = 742
            Top = 128
            Width = 206
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actAssignFDynamicRecord2ToFDynamicRecord1
            Align = alClient
            TabOrder = 12
            ExplicitTop = 145
            ExplicitHeight = 36
          end
          object btnAssignFRecord1ToFDynamicRecord2: TButton
            AlignWithMargins = True
            Left = 95
            Top = 168
            Width = 206
            Height = 29
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actAssignFRecord1ToFDynamicRecord2
            Align = alClient
            TabOrder = 13
            ExplicitTop = 191
            ExplicitHeight = 34
          end
          object btnAssignFRecord2ToFDynamicRecord2: TButton
            AlignWithMargins = True
            Left = 311
            Top = 168
            Width = 206
            Height = 29
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actAssignFRecord2ToFDynamicRecord2
            Align = alClient
            TabOrder = 14
            ExplicitTop = 191
            ExplicitHeight = 34
          end
          object btnAssignFDynamicRecord1ToFDynamicRecord2: TButton
            AlignWithMargins = True
            Left = 527
            Top = 168
            Width = 205
            Height = 29
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actAssignFDynamicRecord1ToFDynamicRecord2
            Align = alClient
            TabOrder = 15
            ExplicitTop = 191
            ExplicitHeight = 34
          end
          object btnAssignFieldValueToFRecord1: TButton
            AlignWithMargins = True
            Left = 958
            Top = 47
            Width = 110
            Height = 31
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actAssignFieldValueToFRecord1
            Align = alClient
            TabOrder = 16
            ExplicitTop = 53
            ExplicitHeight = 36
          end
          object btnAssignFieldValueToFRecord2: TButton
            AlignWithMargins = True
            Left = 958
            Top = 88
            Width = 110
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actAssignFieldValueToFRecord2
            Align = alClient
            TabOrder = 17
            ExplicitTop = 99
            ExplicitHeight = 36
          end
          object btnAssignFieldValueToDynamicRecord1: TButton
            AlignWithMargins = True
            Left = 958
            Top = 128
            Width = 110
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actAssignFieldValueToDynamicRecord1
            Align = alClient
            TabOrder = 18
            ExplicitTop = 145
            ExplicitHeight = 36
          end
          object btnAssignFieldValueToDynamicRecord2: TButton
            AlignWithMargins = True
            Left = 958
            Top = 168
            Width = 110
            Height = 29
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Action = actAssignFieldValueToDynamicRecord2
            Align = alClient
            TabOrder = 19
            ExplicitTop = 191
            ExplicitHeight = 34
          end
          object lblFRecord1: TLabel
            AlignWithMargins = True
            Left = 95
            Top = 207
            Width = 206
            Height = 108
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Align = alClient
            AutoSize = False
            Color = clCream
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'Consolas'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            Transparent = False
            ExplicitTop = 230
            ExplicitWidth = 205
            ExplicitHeight = 121
          end
          object lblFRecord2: TLabel
            AlignWithMargins = True
            Left = 311
            Top = 207
            Width = 206
            Height = 108
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Align = alClient
            AutoSize = False
            Color = clCream
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'Consolas'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            Transparent = False
            ExplicitLeft = 309
            ExplicitTop = 230
            ExplicitWidth = 207
            ExplicitHeight = 121
          end
          object lblFDynamicRecord1: TLabel
            AlignWithMargins = True
            Left = 527
            Top = 207
            Width = 205
            Height = 108
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Align = alClient
            AutoSize = False
            Color = clCream
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'Consolas'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            Transparent = False
            ExplicitLeft = 525
            ExplicitTop = 230
            ExplicitWidth = 206
            ExplicitHeight = 121
          end
          object lblFDynamicRecord2: TLabel
            AlignWithMargins = True
            Left = 742
            Top = 207
            Width = 206
            Height = 108
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Align = alClient
            AutoSize = False
            Color = clCream
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'Consolas'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            Transparent = False
            ExplicitLeft = 740
            ExplicitTop = 230
            ExplicitWidth = 205
            ExplicitHeight = 121
          end
          object pnlField: TPanel
            Left = 953
            Top = 202
            Width = 120
            Height = 113
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 20
            ExplicitTop = 230
            ExplicitHeight = 129
            DesignSize = (
              120
              113)
            object edtFieldName: TLabeledEdit
              Left = 5
              Top = 23
              Width = 102
              Height = 31
              Margins.Left = 5
              Margins.Top = 5
              Margins.Right = 5
              Margins.Bottom = 5
              Anchors = [akLeft, akRight]
              EditLabel.Width = 85
              EditLabel.Height = 23
              EditLabel.Margins.Left = 5
              EditLabel.Margins.Top = 5
              EditLabel.Margins.Right = 5
              EditLabel.Margins.Bottom = 5
              EditLabel.Caption = 'FieldName:'
              TabOrder = 0
              Text = 'Test'
              ExplicitTop = 28
            end
            object edtValue: TLabeledEdit
              Left = 5
              Top = 76
              Width = 102
              Height = 31
              Margins.Left = 5
              Margins.Top = 5
              Margins.Right = 5
              Margins.Bottom = 5
              Anchors = [akLeft, akRight]
              EditLabel.Width = 81
              EditLabel.Height = 23
              EditLabel.Margins.Left = 5
              EditLabel.Margins.Top = 5
              EditLabel.Margins.Right = 5
              EditLabel.Margins.Bottom = 5
              EditLabel.Caption = 'FieldValue:'
              TabOrder = 1
              Text = '8'
              ExplicitTop = 89
            end
          end
        end
      end
    end
    object btnTestAssign: TButton
      Left = 5
      Top = 5
      Width = 85
      Height = 37
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actTestAssign
      TabOrder = 1
    end
    object btnTestAssignTo: TButton
      Left = 5
      Top = 51
      Width = 85
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actTestAssignTo
      TabOrder = 2
    end
    object btnTestAssignTo1: TButton
      Left = 5
      Top = 98
      Width = 85
      Height = 37
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actToStrings
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object btnTestData: TButton
      Left = 5
      Top = 141
      Width = 85
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actTestData
      TabOrder = 4
    end
    object btnCustomTest: TButton
      Left = 5
      Top = 188
      Width = 85
      Height = 37
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
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
