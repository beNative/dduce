object frmGridView: TfrmGridView
  Left = 0
  Top = 0
  Caption = 'GridView demo'
  ClientHeight = 665
  ClientWidth = 1008
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
    Left = 345
    Top = 0
    Width = 6
    Height = 665
    ResizeStyle = rsLine
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 345
    Height = 665
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlRight: TPanel
    Left = 351
    Top = 0
    Width = 657
    Height = 665
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
  object imlMain: TImageList
    Left = 88
    Top = 24
  end
  object aclMain: TActionList
    Left = 24
    Top = 24
    object actAutoSizeCols: TAction
      Caption = 'Autosize colums'
      OnExecute = actAutoSizeColsExecute
    end
  end
  object ppmGridView: TPopupMenu
    Left = 583
    Top = 120
    object mniAutoSizeCols: TMenuItem
      Action = actAutoSizeCols
    end
  end
end
