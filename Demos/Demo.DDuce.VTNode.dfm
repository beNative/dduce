object frmVTNode: TfrmVTNode
  Left = 0
  Top = 0
  ClientHeight = 798
  ClientWidth = 791
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
    Width = 281
    Height = 798
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlMain: TPanel
    Left = 634
    Top = 0
    Width = 157
    Height = 798
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object btnDeleteNode: TButton
      Left = 6
      Top = 118
      Width = 140
      Height = 25
      Action = actDeleteNode
      TabOrder = 3
    end
    object btnAddChild: TButton
      Left = 6
      Top = 87
      Width = 139
      Height = 25
      Action = actAddChildNode
      TabOrder = 2
    end
    object btnSetNodeText: TButton
      Left = 6
      Top = 48
      Width = 139
      Height = 25
      Action = actSetNodeText
      TabOrder = 1
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
      Top = 398
      Width = 139
      Height = 25
      Action = actFullExpandTree
      TabOrder = 10
    end
    object btnFullCollapse: TButton
      Left = 6
      Top = 428
      Width = 139
      Height = 25
      Action = actFullCollapseTree
      TabOrder = 11
    end
    object btnMoveUp: TButton
      Left = 6
      Top = 163
      Width = 139
      Height = 25
      Action = actMoveUpNode
      TabOrder = 4
    end
    object btnMoveDown: TButton
      Left = 6
      Top = 194
      Width = 139
      Height = 25
      Action = actMoveDownNode
      TabOrder = 5
    end
    object btnCollapseNode: TButton
      Left = 6
      Top = 272
      Width = 139
      Height = 25
      Action = actCollapseNode
      TabOrder = 7
    end
    object btnExpandNode: TButton
      Left = 6
      Top = 241
      Width = 139
      Height = 25
      Action = actExpandNode
      TabOrder = 6
    end
    object btnFullCollapseNode: TButton
      Left = 6
      Top = 350
      Width = 139
      Height = 25
      Action = actFullCollapseNode
      TabOrder = 9
    end
    object btnFullExpandNode: TButton
      Left = 6
      Top = 319
      Width = 139
      Height = 25
      Action = actFullExpandNode
      TabOrder = 8
    end
    object btnFocusFirstChild: TButton
      Left = 6
      Top = 484
      Width = 139
      Height = 25
      Action = actSelectFirstChild
      TabOrder = 12
    end
    object btnSelectLastChild: TButton
      Left = 6
      Top = 515
      Width = 139
      Height = 25
      Action = actSelectLastChild
      TabOrder = 13
    end
    object btnSelectNextSibling: TButton
      Left = 6
      Top = 546
      Width = 139
      Height = 25
      Action = actSelectNextSibling
      TabOrder = 14
    end
    object btnSelectPreviousSibling: TButton
      Left = 6
      Top = 577
      Width = 139
      Height = 25
      Action = actSelectPreviousSibling
      TabOrder = 15
    end
  end
  object pnlObjectInspector: TPanel
    Left = 0
    Top = 0
    Width = 353
    Height = 798
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
    object actAddChildNode: TAction
      Caption = 'Add child node'
      OnExecute = actAddChildNodeExecute
    end
    object actSetNodeText: TAction
      Caption = 'Set node text'
      OnExecute = actSetNodeTextExecute
    end
    object actBuildTree: TAction
      Caption = 'Build tree'
      OnExecute = actBuildTreeExecute
    end
    object actFullExpandTree: TAction
      Caption = 'Fully expand tree'
      OnExecute = actFullExpandTreeExecute
    end
    object actFullCollapseTree: TAction
      Caption = 'Fully collapse tree'
      OnExecute = actFullCollapseTreeExecute
    end
    object actMoveUpNode: TAction
      Caption = 'Move node up'
      OnExecute = actMoveUpNodeExecute
    end
    object actMoveDownNode: TAction
      Caption = 'Move node down'
      OnExecute = actMoveDownNodeExecute
    end
    object actExpandNode: TAction
      Caption = 'Expand node'
      OnExecute = actExpandNodeExecute
    end
    object actCollapseNode: TAction
      Caption = 'Collapse node'
      OnExecute = actCollapseNodeExecute
    end
    object actFullExpandNode: TAction
      Caption = 'Fully expand node'
      OnExecute = actFullExpandNodeExecute
    end
    object actFullCollapseNode: TAction
      Caption = 'Fully collapse node'
      OnExecute = actFullCollapseNodeExecute
    end
    object actSelectFirstChild: TAction
      Caption = 'Select first child'
      OnExecute = actSelectFirstChildExecute
    end
    object actSelectLastChild: TAction
      Caption = 'Select last child'
      OnExecute = actSelectLastChildExecute
    end
    object actSelectNextSibling: TAction
      Caption = 'Select next sibling'
      OnExecute = actSelectNextSiblingExecute
    end
    object actSelectPreviousSibling: TAction
      Caption = 'Select previous sibling'
      OnExecute = actSelectPreviousSiblingExecute
    end
  end
end
