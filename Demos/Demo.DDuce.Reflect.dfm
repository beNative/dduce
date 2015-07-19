object frmReflect: TfrmReflect
  Left = 0
  Top = 0
  Caption = 'Reflect demo'
  ClientHeight = 508
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblReflected: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 627
    Height = 13
    Align = alTop
    Caption = 'Reflected properties of this form:'
    FocusControl = mmoMain
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitWidth = 187
  end
  object mmoMain: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 19
    Width = 627
    Height = 486
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
end
