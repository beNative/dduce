object frmXMLTree: TfrmXMLTree
  Left = 0
  Top = 0
  ClientHeight = 1035
  ClientWidth = 1719
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1719
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 1713
    object btnExpand: TButton
      Left = 8
      Top = 4
      Width = 150
      Height = 25
      Action = actExpand
      Images = imlMain
      TabOrder = 0
    end
    object btnCollapse: TButton
      Left = 164
      Top = 4
      Width = 150
      Height = 25
      Action = actCollapse
      Images = imlMain
      TabOrder = 1
    end
    object btnParseDocument: TButton
      Left = 325
      Top = 4
      Width = 150
      Height = 25
      Action = actParseDocument
      Images = imlMain
      TabOrder = 2
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 32
    Width = 1719
    Height = 1003
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlMain'
    TabOrder = 1
    ExplicitWidth = 1713
    ExplicitHeight = 986
    object splVertical: TSplitter
      Left = 479
      Top = 0
      Width = 6
      Height = 1003
      ResizeStyle = rsLine
      ExplicitLeft = 361
      ExplicitHeight = 516
    end
    object pnlTree: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 473
      Height = 997
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitHeight = 986
    end
    object pnlEditor: TPanel
      AlignWithMargins = True
      Left = 488
      Top = 3
      Width = 1228
      Height = 997
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 479
      ExplicitTop = 0
      ExplicitWidth = 1234
      ExplicitHeight = 986
      object pnlObjectInspector: TPanel
        Left = 733
        Top = 0
        Width = 495
        Height = 997
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = 739
        ExplicitHeight = 986
      end
    end
  end
  object aclMain: TActionList
    Images = imlMain
    Left = 320
    Top = 256
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
      Caption = 'Parse document'
      ImageIndex = 2
      ImageName = 'zap-16'
      OnExecute = actParseDocumentExecute
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
    Left = 468
    Top = 564
  end
end
