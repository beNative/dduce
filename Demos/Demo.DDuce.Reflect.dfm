object frmReflect: TfrmReflect
  Left = 0
  Top = 0
  Caption = 'Reflect demo'
  ClientHeight = 762
  ClientWidth = 495
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
  object mmoMain: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 26
    Width = 489
    Height = 733
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
  object pnlReflected: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 489
    Height = 20
    Margins.Bottom = 0
    Align = alTop
    BevelKind = bkFlat
    BevelOuter = bvNone
    Caption = 'Reflected properties of this form:'
    Color = clSilver
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 1
  end
end
