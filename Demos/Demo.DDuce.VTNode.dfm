object frmVTNode: TfrmVTNode
  Left = 0
  Top = 0
  ClientHeight = 489
  ClientWidth = 764
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
    Left = 353
    Top = 0
    Width = 254
    Height = 489
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 264
    ExplicitHeight = 642
  end
  object pnlMain: TPanel
    Left = 607
    Top = 0
    Width = 157
    Height = 489
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 866
    ExplicitWidth = 215
    ExplicitHeight = 642
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
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 353
    Height = 489
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitHeight = 642
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
