object frmXMLTree: TfrmXMLTree
  Left = 0
  Top = 0
  ClientHeight = 507
  ClientWidth = 924
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
    Width = 924
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 652
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
    object btnInspectComponent: TButton
      Left = 320
      Top = 4
      Width = 150
      Height = 25
      Action = actInspectComponent
      TabOrder = 2
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 32
    Width = 924
    Height = 475
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlMain'
    TabOrder = 1
    ExplicitLeft = 8
    ExplicitTop = 38
    ExplicitWidth = 561
    ExplicitHeight = 443
    object spl1: TSplitter
      Left = 361
      Top = 0
      Width = 7
      Height = 475
      ExplicitLeft = 306
      ExplicitTop = 1
      ExplicitHeight = 441
    end
    object pnlXMLTree: TPanel
      Left = 0
      Top = 0
      Width = 361
      Height = 475
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
    end
    object pnlEditor: TPanel
      Left = 368
      Top = 0
      Width = 556
      Height = 475
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 376
      ExplicitWidth = 548
      object mmoXML: TMemo
        Left = 0
        Top = 0
        Width = 556
        Height = 475
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnChange = mmoXMLChange
        ExplicitLeft = 184
        ExplicitTop = 192
        ExplicitWidth = 185
        ExplicitHeight = 89
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
    object actInspectComponent: TAction
      Caption = 'Inspect component'
      OnExecute = actInspectComponentExecute
    end
  end
end
