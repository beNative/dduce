object frmScopedReferences: TfrmScopedReferences
  Left = 0
  Top = 0
  Caption = 'Scoped references'
  ClientHeight = 82
  ClientWidth = 234
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
  object aclMain: TActionList
    Left = 168
    Top = 8
    object actShowClassNames: TAction
      Caption = 'Show classnames of embedded objects'
      OnExecute = actShowClassNamesExecute
    end
  end
end
