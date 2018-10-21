object frmVTNode: TfrmVTNode
  Left = 0
  Top = 0
  ClientHeight = 642
  ClientWidth = 1171
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTree: TPanel
    Left = 433
    Top = 0
    Width = 433
    Height = 642
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 65
    ExplicitTop = -24
  end
  object pnlMain: TPanel
    Left = 866
    Top = 0
    Width = 305
    Height = 642
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 488
    ExplicitWidth = 340
    object btnDeleteNode: TButton
      Left = 6
      Top = 8
      Width = 140
      Height = 25
      Action = actDeleteNode
      TabOrder = 0
    end
    object btnAddChild: TButton
      Left = 6
      Top = 39
      Width = 139
      Height = 25
      Action = actAddChild
      TabOrder = 1
    end
    object btnSetNodeText: TButton
      Left = 6
      Top = 70
      Width = 139
      Height = 25
      Action = actSetNodeText
      TabOrder = 2
    end
    object btn3: TButton
      Left = 6
      Top = 101
      Width = 139
      Height = 25
      Caption = 'btn1'
      TabOrder = 3
    end
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 433
    Height = 642
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 8
  end
  object aclMain: TActionList
    Left = 208
    Top = 208
    object actDeleteNode: TAction
      Caption = 'Delete node'
      OnExecute = actDeleteNodeExecute
    end
    object actAddChild: TAction
      Caption = 'Add child node'
      OnExecute = actAddChildExecute
    end
    object actSetNodeText: TAction
      Caption = 'Set node text'
      OnExecute = actSetNodeTextExecute
    end
  end
end
