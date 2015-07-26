object frmVirtualDBGrid: TfrmVirtualDBGrid
  Left = 0
  Top = 0
  ClientHeight = 704
  ClientWidth = 1016
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object splHorizontal: TSplitter
    Left = 0
    Top = 22
    Width = 1016
    Height = 8
    Cursor = crVSplit
    Align = alTop
    Visible = False
    ExplicitLeft = 8
    ExplicitTop = 79
    ExplicitWidth = 721
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 685
    Width = 1016
    Height = 19
    Panels = <>
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1016
    Height = 22
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object navMain: TDBNavigator
      Left = 0
      Top = 0
      Width = 1016
      Height = 22
      DataSource = dscMain
      Align = alClient
      Flat = True
      TabOrder = 0
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 30
    Width = 1016
    Height = 655
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object splVertical: TSplitter
      Left = 732
      Top = 0
      Width = 8
      Height = 655
      Align = alRight
      ExplicitLeft = 574
      ExplicitHeight = 474
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 732
      Height = 655
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
    end
    object pnlRight: TPanel
      Left = 740
      Top = 0
      Width = 276
      Height = 655
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  object imlMain: TImageList
    Left = 16
    Top = 320
  end
  object aclMain: TActionList
    Images = imlMain
    Left = 72
    Top = 320
    object actInspectComponent: TAction
      Caption = 'Inspect component'
      ShortCut = 16496
    end
  end
  object dscMain: TDataSource
    DataSet = dmData.cdsMain
    Left = 312
    Top = 176
  end
end
