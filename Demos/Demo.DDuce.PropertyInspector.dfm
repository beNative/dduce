object frmPropertyInspector: TfrmPropertyInspector
  Left = 0
  Top = 0
  Caption = 'PropertyInspector Demo'
  ClientHeight = 586
  ClientWidth = 830
  Color = clBtnFace
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
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 830
    Height = 586
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object splSplitter: TSplitter
      Left = 313
      Top = 0
      Width = 6
      Height = 567
      ResizeStyle = rsUpdate
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 313
      Height = 567
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object cbxControls: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 307
        Height = 21
        Margins.Bottom = 0
        Align = alTop
        Style = csDropDownList
        DropDownCount = 20
        TabOrder = 0
        OnChange = cbxControlsChange
      end
    end
    object pnlRight: TPanel
      Left = 319
      Top = 0
      Width = 511
      Height = 567
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        511
        567)
      object lblLabel: TLabel
        Left = 6
        Top = 3
        Width = 500
        Height = 62
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'This form demonstrates the TPropertyInspector control. '#13#10'You can' +
          ' adjust properties of any control shown on this form.'
        EllipsisPosition = epWordEllipsis
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 245
      end
      object btnButton: TButton
        Left = 225
        Top = 66
        Width = 109
        Height = 25
        Caption = 'TButton'
        ImageIndex = 0
        ImageMargins.Left = 3
        ImageMargins.Top = 3
        ImageMargins.Right = 3
        ImageMargins.Bottom = 3
        TabOrder = 1
      end
      object chkCheckBox: TCheckBox
        Left = 6
        Top = 132
        Width = 251
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'TCheckBox'
        State = cbGrayed
        TabOrder = 2
      end
      object edtEdit: TEdit
        Left = 8
        Top = 68
        Width = 107
        Height = 21
        TabOrder = 0
        Text = 'TEdit'
      end
      object bgMain: TButtonGroup
        Left = 6
        Top = 155
        Width = 500
        Height = 406
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelInner = bvNone
        BevelKind = bkFlat
        BorderStyle = bsNone
        ButtonOptions = [gboAllowReorder, gboShowCaptions]
        Items = <>
        TabOrder = 3
      end
      object trbTrackBar: TTrackBar
        Left = 6
        Top = 100
        Width = 500
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        PositionToolTip = ptTop
        ShowSelRange = False
        TabOrder = 4
      end
      object edtButtonedEdit: TButtonedEdit
        Left = 121
        Top = 68
        Width = 98
        Height = 21
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
      Top = 567
      Width = 830
      Height = 19
      Panels = <>
      ParentFont = True
      ParentShowHint = False
      ShowHint = True
      UseSystemFont = False
    end
  end
end
