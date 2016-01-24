object frmEditor: TfrmEditor
  Left = 0
  Top = 0
  ClientHeight = 568
  ClientWidth = 796
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
  object sbrMain: TStatusBar
    Left = 0
    Top = 549
    Width = 796
    Height = 19
    Panels = <>
    ExplicitLeft = 408
    ExplicitTop = 296
    ExplicitWidth = 0
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 353
    Height = 549
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pnlRight: TPanel
    Left = 353
    Top = 0
    Width = 443
    Height = 549
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 8
    ExplicitTop = 8
    ExplicitWidth = 353
    object splVertical: TSplitter
      Left = 0
      Top = 0
      Width = 5
      Height = 549
      ExplicitLeft = -1
      ExplicitTop = 1
      ExplicitHeight = 547
    end
  end
end
