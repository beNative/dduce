object frmEditor: TfrmEditor
  Left = 0
  Top = 0
  ClientHeight = 568
  ClientWidth = 1004
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 377
    Top = 0
    Width = 5
    Height = 549
    ExplicitLeft = 8
    ExplicitTop = 8
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 549
    Width = 1004
    Height = 19
    Panels = <>
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 377
    Height = 549
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pnlRight: TPanel
    Left = 382
    Top = 0
    Width = 622
    Height = 549
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
  end
end
