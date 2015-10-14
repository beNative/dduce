object frmInspector: TfrmInspector
  Left = 0
  Top = 0
  Caption = 'Inspector demo'
  ClientHeight = 605
  ClientWidth = 660
  Color = clBtnFace
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
    Width = 6
    Height = 605
    ExplicitLeft = 198
    ExplicitHeight = 450
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 321
    Height = 605
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 450
  end
  object pnlRight: TPanel
    Left = 327
    Top = 0
    Width = 333
    Height = 605
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 185
    ExplicitWidth = 450
    ExplicitHeight = 450
  end
  object aclMain: TActionList
    Left = 312
    Top = 232
  end
  object imlMain: TImageList
    Left = 368
    Top = 224
  end
end
