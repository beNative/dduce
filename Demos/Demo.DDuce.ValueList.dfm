object frmValueListDemo: TfrmValueListDemo
  Left = 0
  Top = 0
  ClientHeight = 540
  ClientWidth = 661
  Color = clBtnFace
  DoubleBuffered = True
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 321
    Top = 0
    Width = 6
    Height = 540
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 321
    Height = 540
    Align = alLeft
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlRight: TPanel
    Left = 327
    Top = 0
    Width = 334
    Height = 540
    Align = alClient
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 1
  end
end
