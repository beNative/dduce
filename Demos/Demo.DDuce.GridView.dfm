object frmGridView: TfrmGridView
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'GridView demo'
  ClientHeight = 998
  ClientWidth = 1521
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 144
  TextHeight = 23
  object splVertical: TSplitter
    Left = 518
    Top = 0
    Width = 9
    Height = 998
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    MinSize = 45
    ResizeStyle = rsLine
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 518
    Height = 998
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlRight: TPanel
    Left = 527
    Top = 0
    Width = 994
    Height = 998
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
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
