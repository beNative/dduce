object frmDialogs: TfrmDialogs
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Dialogs'
  ClientHeight = 104
  ClientWidth = 253
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 144
  TextHeight = 23
  object btnAboutDialog: TButton
    Left = 12
    Top = 12
    Width = 225
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Action = actAboutDialog
    TabOrder = 0
  end
  object btnRTTEye: TButton
    Left = 12
    Top = 59
    Width = 225
    Height = 37
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Action = actRTTEye
    TabOrder = 1
  end
  object aclMain: TActionList
    Left = 16
    Top = 16
    object actAboutDialog: TAction
      Caption = 'About dialog'
      OnExecute = actAboutDialogExecute
    end
    object actRTTEye: TAction
      Caption = 'RTTEye'
      OnExecute = actRTTEyeExecute
    end
  end
end
