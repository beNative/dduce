object frmVTNode: TfrmVTNode
  Left = 0
  Top = 0
  ClientHeight = 489
  ClientWidth = 764
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
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
  end
  object pnlMain: TPanel
    Left = 607
    Top = 0
    Width = 157
    Height = 489
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object btnDeleteNode: TButton
      Left = 6
      Top = 225
      Width = 140
      Height = 25
      Action = actDeleteNode
      TabOrder = 7
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
    object btnBuildTree: TButton
      Left = 6
      Top = 8
      Width = 139
      Height = 25
      Action = actBuildTree
      TabOrder = 0
    end
    object btnFullExpand: TButton
      Left = 6
      Top = 101
      Width = 139
      Height = 25
      Action = actFullExpand
      TabOrder = 3
    end
    object btnFullCollapse: TButton
      Left = 6
      Top = 132
      Width = 139
      Height = 25
      Action = actFullCollapse
      TabOrder = 4
    end
    object btnMoveUp: TButton
      Left = 6
      Top = 163
      Width = 139
      Height = 25
      Action = actMoveUp
      TabOrder = 5
    end
    object btnMoveDown: TButton
      Left = 6
      Top = 194
      Width = 139
      Height = 25
      Action = actMoveDown
      TabOrder = 6
    end
  end
  object pnlObjectInspector: TPanel
    Left = 0
    Top = 0
    Width = 353
    Height = 489
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
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
    object actBuildTree: TAction
      Caption = 'Build tree'
      OnExecute = actBuildTreeExecute
    end
    object actFullExpand: TAction
      Caption = 'Full expand'
      OnExecute = actFullExpandExecute
    end
    object actFullCollapse: TAction
      Caption = 'Full collapse'
      OnExecute = actFullCollapseExecute
    end
    object actMoveUp: TAction
      Caption = 'Move up'
      OnExecute = actMoveUpExecute
    end
    object actMoveDown: TAction
      Caption = 'Move down'
      OnExecute = actMoveDownExecute
    end
  end
end
