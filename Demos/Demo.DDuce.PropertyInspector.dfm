object frmPropertyInspector: TfrmPropertyInspector
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'PropertyInspector Demo'
  ClientHeight = 879
  ClientWidth = 1254
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  PixelsPerInch = 144
  TextHeight = 23
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 1254
    Height = 879
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object splSplitter: TSplitter
      Left = 470
      Top = 0
      Width = 9
      Height = 851
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      MinSize = 45
      ResizeStyle = rsLine
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 470
      Height = 851
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object cbxControls: TComboBox
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 460
        Height = 31
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 0
        Align = alTop
        Style = csDropDownList
        DropDownCount = 20
        TabOrder = 0
        OnChange = cbxControlsChange
      end
    end
    object pnlRight: TPanel
      Left = 479
      Top = 0
      Width = 775
      Height = 851
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        775
        851)
      object lblLabel: TLabel
        Left = 9
        Top = 5
        Width = 749
        Height = 93
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'This form demonstrates the TPropertyInspector control. '#13#10'You can' +
          ' adjust properties of any control shown on this form.'
        EllipsisPosition = epWordEllipsis
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 750
      end
      object btnButton: TButton
        Left = 338
        Top = 99
        Width = 163
        Height = 38
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'TButton'
        ImageIndex = 0
        ImageMargins.Left = 5
        ImageMargins.Top = 5
        ImageMargins.Right = 5
        ImageMargins.Bottom = 5
        TabOrder = 1
      end
      object chkCheckBox: TCheckBox
        Left = 9
        Top = 198
        Width = 376
        Height = 26
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akTop, akRight]
        Caption = 'TCheckBox'
        State = cbGrayed
        TabOrder = 2
      end
      object edtEdit: TEdit
        Left = 12
        Top = 102
        Width = 161
        Height = 31
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabOrder = 0
        Text = 'TEdit'
      end
      object bgMain: TButtonGroup
        Left = 9
        Top = 233
        Width = 749
        Height = 609
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelInner = bvNone
        BevelKind = bkFlat
        BorderStyle = bsNone
        ButtonHeight = 36
        ButtonWidth = 36
        ButtonOptions = [gboAllowReorder, gboShowCaptions]
        Items = <>
        TabOrder = 3
      end
      object trbTrackBar: TTrackBar
        Left = 9
        Top = 150
        Width = 749
        Height = 39
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Anchors = [akLeft, akTop, akRight]
        PositionToolTip = ptTop
        ShowSelRange = False
        TabOrder = 4
        ThumbLength = 30
      end
      object edtButtonedEdit: TButtonedEdit
        Left = 182
        Top = 102
        Width = 147
        Height = 31
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        RightButton.Hint = 'Hint'
        RightButton.HotImageIndex = 114
        RightButton.ImageIndex = 115
        RightButton.PressedImageIndex = 116
        RightButton.Visible = True
        TabOrder = 5
        Text = 'TButtonedEdit'
      end
    end
    object sbrStatusBar: TStatusBar
      Left = 0
      Top = 851
      Width = 1254
      Height = 28
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Panels = <>
      ParentFont = True
      ParentShowHint = False
      ShowHint = True
      UseSystemFont = False
    end
  end
end
