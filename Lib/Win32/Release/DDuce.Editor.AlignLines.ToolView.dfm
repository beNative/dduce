inherited frmAlignLines: TfrmAlignLines
  Left = 76
  Top = 76
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Align selection'
  ClientHeight = 729
  ClientWidth = 366
  Color = clBtnFace
  Constraints.MinWidth = 170
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 23
  object sbrMain: TScrollBox
    Left = 0
    Top = 0
    Width = 366
    Height = 704
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 0
    DesignSize = (
      366
      704)
    object rgpSortDirection: TRadioGroup
      Left = 5
      Top = 164
      Width = 352
      Height = 54
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Sort direction:'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Ascending'
        'Descending')
      TabOrder = 1
      OnClick = rgpSortDirectionClick
    end
    object rgpAlignAt: TRadioGroup
      Left = 5
      Top = 222
      Width = 352
      Height = 54
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Align at:'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Leftmost token'
        'Rightmost token')
      TabOrder = 0
      Visible = False
      OnClick = rgpAlignAtClick
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 704
    Width = 366
    Height = 25
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      366
      25)
    object btnOK: TButton
      Left = 240
      Top = 0
      Width = 120
      Height = 25
      Action = actExecute
      Anchors = [akRight, akBottom]
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object aclMain: TActionList
    Left = 152
    Top = 472
    object actExecute: TAction
      Caption = 'Execute'
      OnExecute = actExecuteExecute
    end
  end
end
