object frmXMLTree: TfrmXMLTree
  Left = 0
  Top = 0
  ClientHeight = 548
  ClientWidth = 1097
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1097
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btnExpand: TButton
      Left = 8
      Top = 4
      Width = 150
      Height = 25
      Action = actExpand
      TabOrder = 0
    end
    object btnCollapse: TButton
      Left = 164
      Top = 4
      Width = 150
      Height = 25
      Action = actCollapse
      TabOrder = 1
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 32
    Width = 1097
    Height = 516
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlMain'
    TabOrder = 1
    object spl1: TSplitter
      Left = 361
      Top = 0
      Width = 7
      Height = 516
      ExplicitLeft = 306
      ExplicitTop = 1
      ExplicitHeight = 441
    end
    object pnlXMLTree: TPanel
      Left = 0
      Top = 0
      Width = 361
      Height = 516
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
    end
    object pnlEditor: TPanel
      Left = 368
      Top = 0
      Width = 729
      Height = 516
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object mmoXML: TMemo
        Left = 0
        Top = 0
        Width = 431
        Height = 516
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnChange = mmoXMLChange
      end
      object pnlInspector: TPanel
        Left = 431
        Top = 0
        Width = 298
        Height = 516
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
      end
    end
  end
  object aclMain: TActionList
    Left = 320
    Top = 256
    object actExpand: TAction
      Caption = 'Expand'
      OnExecute = actExpandExecute
    end
    object actCollapse: TAction
      Caption = 'Collapse'
      OnExecute = actCollapseExecute
    end
  end
end
