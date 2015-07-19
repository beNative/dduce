object frmMainMenu: TfrmMainMenu
  Left = 0
  Top = 0
  ActiveControl = edtFilter
  Caption = 'DDuce demos'
  ClientHeight = 333
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object btnExecute: TBitBtn
    AlignWithMargins = True
    Left = 3
    Top = 286
    Width = 363
    Height = 25
    Action = actExecute
    Align = alBottom
    Caption = '&Execute'
    Default = True
    ParentDoubleBuffered = True
    Style = bsNew
    TabOrder = 0
  end
  object pnlVST: TPanel
    Left = 0
    Top = 29
    Width = 369
    Height = 254
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object vstDemos: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 369
      Height = 254
      Align = alClient
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.MainColumn = -1
      TabOrder = 0
      Columns = <>
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 369
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object edtFilter: TEdit
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 363
      Height = 23
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      TextHint = 'Filter'
      OnChange = edtFilterChange
      OnKeyDown = edtFilterKeyDown
      OnKeyUp = edtFilterKeyUp
      ExplicitHeight = 21
    end
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 314
    Width = 369
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object aclMain: TActionList
    Left = 72
    Top = 192
    object actExecute: TAction
      Caption = '&Execute'
      OnExecute = actExecuteExecute
    end
    object actClose: TAction
      Caption = 'Close'
    end
    object actFocusFilter: TAction
      Caption = 'Focus search filter'
      ShortCut = 113
      OnExecute = actFocusFilterExecute
    end
  end
end
