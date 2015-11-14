object frmInspector: TfrmInspector
  Left = 0
  Top = 0
  Caption = 'Inspector demo'
  ClientHeight = 605
  ClientWidth = 984
  Color = clBtnFace
  DoubleBuffered = True
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
  end
  object pnlRight: TPanel
    Left = 327
    Top = 0
    Width = 657
    Height = 605
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object splHorizontal: TSplitter
      Left = 0
      Top = 481
      Width = 657
      Height = 7
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 208
      ExplicitWidth = 333
    end
    object pnlInspector: TPanel
      Left = 0
      Top = 488
      Width = 657
      Height = 117
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
    end
    object grdMain: TDBGrid
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 651
      Height = 475
      Align = alClient
      DataSource = dscMain
      Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
      ReadOnly = True
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
  end
  object dscMain: TDataSource
    OnDataChange = dscMainDataChange
    Left = 16
    Top = 8
  end
  object pmMain: TPopupMenu
    Left = 64
    Top = 8
    object mniHideEmptyFields: TMenuItem
      Action = actHideEmptyFields
      AutoCheck = True
    end
  end
  object aclMain: TActionList
    Left = 120
    Top = 8
    object actHideEmptyFields: TAction
      AutoCheck = True
      Caption = 'Hide empty fields'
      Checked = True
    end
  end
end
