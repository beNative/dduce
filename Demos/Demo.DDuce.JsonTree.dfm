object frmJsonTree: TfrmJsonTree
  Left = 228
  Top = 228
  ClientHeight = 933
  ClientWidth = 1452
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1452
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    OnClick = pnlTopClick
    ExplicitWidth = 1446
    object btnExpand: TButton
      Left = 8
      Top = 3
      Width = 150
      Height = 25
      Action = actExpand
      Images = imlMain
      TabOrder = 0
    end
    object btnCollapse: TButton
      Left = 164
      Top = 3
      Width = 150
      Height = 25
      Action = actCollapse
      Images = imlMain
      TabOrder = 1
    end
    object btnParseDocument: TButton
      Left = 320
      Top = 3
      Width = 150
      Height = 25
      Action = actParseDocument
      Caption = 'Parse document'
      Images = imlMain
      TabOrder = 2
    end
    object btnCreateJsonDocument: TButton
      Left = 476
      Top = 3
      Width = 157
      Height = 25
      Action = actCreateJsonDocument
      Images = imlMain
      TabOrder = 3
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 32
    Width = 1452
    Height = 901
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlMain'
    TabOrder = 1
    ExplicitWidth = 1446
    ExplicitHeight = 884
    object splVertical: TSplitter
      Left = 481
      Top = 0
      Width = 6
      Height = 901
      ResizeStyle = rsLine
    end
    object pnlTree: TPanel
      Left = 0
      Top = 0
      Width = 481
      Height = 901
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 884
    end
    object pnlEditor: TPanel
      Left = 487
      Top = 0
      Width = 965
      Height = 901
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 959
      ExplicitHeight = 884
      object pnlObjectInspector: TPanel
        Left = 571
        Top = 0
        Width = 394
        Height = 901
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = 565
        ExplicitHeight = 884
      end
    end
  end
  object aclMain: TActionList
    Images = imlMain
    Left = 92
    Top = 112
    object actExpand: TAction
      Caption = 'Expand'
      ImageIndex = 0
      ImageName = 'diff-added-16'
      OnExecute = actExpandExecute
    end
    object actCollapse: TAction
      Caption = 'Collapse'
      ImageIndex = 1
      ImageName = 'diff-removed-16'
      OnExecute = actCollapseExecute
    end
    object actParseDocument: TAction
      Caption = 'actParseDocument'
      ImageIndex = 2
      ImageName = 'zap-16'
      OnExecute = actParseDocumentExecute
    end
    object actCreateJsonDocument: TAction
      Caption = 'Create JSON document'
      OnExecute = actCreateJsonDocumentExecute
    end
    object actCopy: TAction
      Caption = 'Copy'
      OnExecute = actCopyExecute
    end
  end
  object ppmTree: TPopupMenu
    Images = imlMain
    Left = 84
    Top = 364
    object mniCopy: TMenuItem
      Action = actCopy
    end
  end
  object imlMain: TVirtualImageList
    Images = <
      item
        CollectionIndex = 94
        CollectionName = 'diff-added-16'
        Name = 'diff-added-16'
      end
      item
        CollectionIndex = 95
        CollectionName = 'diff-removed-16'
        Name = 'diff-removed-16'
      end
      item
        CollectionIndex = 406
        CollectionName = 'zap-16'
        Name = 'zap-16'
      end>
    ImageCollection = dmData.imcMain
    Left = 96
    Top = 228
  end
end
