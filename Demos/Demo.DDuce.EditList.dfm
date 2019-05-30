object frmEditList: TfrmEditList
  Left = 0
  Top = 0
  Caption = 'TEditList module demo'
  ClientHeight = 411
  ClientWidth = 635
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 321
    Top = 0
    Width = 2
    Height = 411
    Color = clScrollBar
    ParentColor = False
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 321
    Height = 411
    Align = alLeft
    BevelEdges = []
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
  end
  object pnlRight: TPanel
    Left = 323
    Top = 0
    Width = 312
    Height = 411
    Align = alClient
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 326
    ExplicitWidth = 309
    DesignSize = (
      312
      411)
    object lblSelected: TLabel
      Left = 6
      Top = 42
      Width = 294
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Selected:'
      ExplicitWidth = 291
    end
    object mmoData: TMemo
      Left = 0
      Top = 58
      Width = 312
      Height = 353
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Consolas'
      Font.Style = []
      ParentColor = True
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
      ExplicitLeft = 1
      ExplicitWidth = 309
    end
  end
  object chkMultiSelect: TCheckBox
    Left = 332
    Top = 8
    Width = 97
    Height = 17
    Caption = 'MultiSelect'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = chkMultiSelectClick
  end
end
