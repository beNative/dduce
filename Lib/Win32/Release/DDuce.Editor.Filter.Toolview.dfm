inherited frmFilter: TfrmFilter
  Left = 579
  Top = 257
  ClientHeight = 454
  ClientWidth = 526
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  ExplicitWidth = 542
  ExplicitHeight = 493
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 526
    Height = 431
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 530
    ExplicitHeight = 432
    object pnlHeader: TPanel
      Left = 0
      Top = 0
      Width = 530
      Height = 21
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 0
      object edtFilter: TEdit
        Left = 0
        Top = 0
        Width = 530
        Height = 21
        Align = alTop
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 0
        OnChange = edtFilterChange
        OnKeyDown = edtFilterKeyDown
        OnKeyUp = edtFilterKeyUp
      end
    end
    object pnlView: TPanel
      Left = 0
      Top = 21
      Width = 530
      Height = 411
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 431
    Width = 526
    Height = 23
    Panels = <>
    ExplicitTop = 432
    ExplicitWidth = 530
  end
  object aclMain: TActionList
    Left = 158
    Top = 124
    object actFocusFilterText: TAction
      Caption = 'Set focus to filter text'
      ShortCut = 113
      OnExecute = actFocusFilterTextExecute
    end
  end
end
