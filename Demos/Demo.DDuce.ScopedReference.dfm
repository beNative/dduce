object frmScopedReferences: TfrmScopedReferences
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Scoped references'
  ClientHeight = 140
  ClientWidth = 232
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblCode: TLabel
    Left = 16
    Top = 8
    Width = 138
    Height = 39
    Caption = 'O: Scoped<TObject>;'#13#10'P: Scoped<TPersistent>;'#13#10'L: Scoped<TList>;'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
  end
  object btnShowClassNames: TButton
    Left = 8
    Top = 50
    Width = 217
    Height = 25
    Action = actShowClassNames
    TabOrder = 0
  end
  object pnlScopedButton: TPanel
    AlignWithMargins = True
    Left = 8
    Top = 108
    Width = 216
    Height = 24
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
  end
  object btnCreateScopedButton: TButton
    Left = 8
    Top = 79
    Width = 217
    Height = 25
    Action = actCreateScopedButton
    TabOrder = 2
  end
  object aclMain: TActionList
    Left = 168
    Top = 8
    object actShowClassNames: TAction
      Caption = 'Show classnames of embedded objects'
      OnExecute = actShowClassNamesExecute
    end
    object actCreateScopedButton: TAction
      Caption = 'Create a scoped button'
      OnExecute = actCreateScopedButtonExecute
    end
  end
end
