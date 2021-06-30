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
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 321
    Top = 0
    Width = 6
    Height = 411
    Color = clBtnFace
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
    Left = 327
    Top = 0
    Width = 308
    Height = 411
    Align = alClient
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 323
    ExplicitWidth = 312
    object mmoData: TMemo
      Left = 0
      Top = 31
      Width = 308
      Height = 380
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
      ExplicitWidth = 312
    end
  end
  object chkMultiSelect: TCheckBox
    Left = 332
    Top = 8
    Width = 97
    Height = 17
    Caption = 'MultiSelect'
    TabOrder = 2
    OnClick = chkMultiSelectClick
  end
end
