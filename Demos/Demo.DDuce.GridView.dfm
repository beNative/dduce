object frmGridView: TfrmGridView
  Left = 0
  Top = 0
  Caption = 'GridView demo'
  ClientHeight = 579
  ClientWidth = 781
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
    Left = 289
    Top = 0
    Width = 6
    Height = 579
    ExplicitLeft = 233
    ExplicitTop = -113
    ExplicitHeight = 450
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 289
    Height = 579
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlRight: TPanel
    Left = 295
    Top = 0
    Width = 486
    Height = 579
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
  object imlMain: TImageList
    Left = 368
    Top = 224
  end
  object aclMain: TActionList
    Left = 312
    Top = 232
  end
end
