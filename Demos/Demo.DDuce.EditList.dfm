object frmEditList: TfrmEditList
  Left = 114
  Top = 114
  Caption = 'TEditList module demo'
  ClientHeight = 411
  ClientWidth = 647
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 13
  object splVertical: TSplitter
    Left = 327
    Top = 0
    Width = 6
    Height = 411
    Color = clBtnFace
    ParentColor = False
    ResizeStyle = rsLine
    ExplicitLeft = 321
  end
  object pnlLeft: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 321
    Height = 405
    Align = alLeft
    BevelEdges = []
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitHeight = 394
  end
  object pnlRight: TPanel
    Left = 333
    Top = 0
    Width = 314
    Height = 411
    Align = alClient
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 327
    ExplicitHeight = 394
    object mmoData: TMemo
      Left = 0
      Top = 31
      Width = 314
      Height = 380
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
      ExplicitHeight = 363
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
