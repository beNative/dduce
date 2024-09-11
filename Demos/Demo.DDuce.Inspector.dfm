object frmInspector: TfrmInspector
  Left = 266
  Top = 266
  Caption = 'Inspector demo'
  ClientHeight = 605
  ClientWidth = 996
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 13
  object splVertical: TSplitter
    Left = 321
    Top = 0
    Width = 6
    Height = 605
    ResizeStyle = rsLine
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 321
    Height = 605
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlRight: TPanel
    Left = 327
    Top = 0
    Width = 669
    Height = 605
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object splHorizontal: TSplitter
      Left = 0
      Top = 482
      Width = 669
      Height = 6
      Cursor = crVSplit
      Align = alBottom
      ResizeStyle = rsLine
    end
    object pnlInspector: TPanel
      Left = 0
      Top = 488
      Width = 669
      Height = 117
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
    end
    object grdMain: TDBGrid
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 662
      Height = 476
      Align = alClient
      DataSource = dscMain
      Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
      ReadOnly = True
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Segoe UI'
      TitleFont.Style = []
    end
  end
  object dscMain: TDataSource
    OnDataChange = dscMainDataChange
    Left = 16
    Top = 8
  end
  object pmMain: TPopupMenu
    Left = 120
    Top = 12
    object mniHideEmptyFields: TMenuItem
      Action = actHideEmptyFields
      AutoCheck = True
    end
  end
  object aclMain: TActionList
    Left = 216
    Top = 12
    object actHideEmptyFields: TAction
      AutoCheck = True
      Caption = 'Hide empty fields'
      Checked = True
    end
  end
end
