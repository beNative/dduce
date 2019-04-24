object frmDialogs: TfrmDialogs
  Left = 0
  Top = 0
  Caption = 'Dialogs'
  ClientHeight = 69
  ClientWidth = 169
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnAboutDialog: TButton
    Left = 8
    Top = 8
    Width = 150
    Height = 25
    Action = actAboutDialog
    TabOrder = 0
  end
  object btnRTTEye: TButton
    Left = 8
    Top = 39
    Width = 150
    Height = 25
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
