object frmEditor: TfrmEditor
  Left = 0
  Top = 0
  ClientHeight = 568
  ClientWidth = 1004
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
  object splVertical: TSplitter
    Left = 377
    Top = 0
    Width = 6
    Height = 549
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
    Left = 383
    Top = 0
    Width = 621
    Height = 549
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 379
    ExplicitWidth = 625
  end
end
