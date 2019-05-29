object frmVirtualTrees: TfrmVirtualTrees
  Left = 0
  Top = 0
  ClientHeight = 731
  ClientWidth = 984
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pgcMain: TPageControl
    Left = 0
    Top = 0
    Width = 984
    Height = 731
    ActivePage = tsSettings
    Align = alClient
    TabOrder = 0
    object tsSettings: TTabSheet
      Caption = 'Options'
      ImageIndex = 6
      object pnlOptions: TGridPanel
        Left = 0
        Top = 0
        Width = 976
        Height = 703
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 33.333333333333340000
          end
          item
            Value = 33.333333333333340000
          end
          item
            Value = 33.333333333333340000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = pnlVST
            Row = 0
          end
          item
            Column = 1
            Control = pnlVSTTree
            Row = 0
          end
          item
            Column = 2
            Control = pnlVSTGrid
            Row = 0
          end
          item
            Column = 0
            Control = pnlVSTList
            Row = 1
          end
          item
            Column = 1
            Control = pnlVSTTreeList
            Row = 1
          end
          item
            Column = 2
            Control = pnlVSTTreeGrid
            Row = 1
          end>
        RowCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        TabOrder = 0
        object pnlVST: TPanel
          Left = 0
          Top = 0
          Width = 325
          Height = 351
          Align = alClient
          BevelEdges = []
          BevelOuter = bvNone
          Caption = 'pnlVST'
          ParentColor = True
          TabOrder = 0
          object pnlVSTHeader: TPanel
            Left = 0
            Top = 0
            Width = 325
            Height = 20
            Align = alTop
            BevelEdges = []
            BevelKind = bkFlat
            BevelOuter = bvNone
            Caption = 'VST'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentBackground = False
            ParentFont = False
            TabOrder = 0
            ExplicitLeft = 3
            ExplicitTop = 3
            ExplicitWidth = 319
          end
        end
        object pnlVSTTree: TPanel
          Left = 325
          Top = 0
          Width = 325
          Height = 351
          Align = alClient
          BevelEdges = []
          BevelOuter = bvNone
          Caption = 'pnlVSTTree'
          ParentColor = True
          TabOrder = 1
          object pnlVSTTreeHeader: TPanel
            Left = 0
            Top = 0
            Width = 325
            Height = 20
            Align = alTop
            BevelEdges = []
            BevelKind = bkFlat
            BevelOuter = bvNone
            Caption = 'VSTTree'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentBackground = False
            ParentFont = False
            TabOrder = 0
            ExplicitLeft = 3
            ExplicitTop = 3
            ExplicitWidth = 319
          end
        end
        object pnlVSTGrid: TPanel
          Left = 650
          Top = 0
          Width = 326
          Height = 351
          Align = alClient
          BevelEdges = []
          BevelOuter = bvNone
          Caption = 'pnlVSTGrid'
          ParentColor = True
          TabOrder = 2
          object pnlVSTGridHeader: TPanel
            Left = 0
            Top = 0
            Width = 326
            Height = 20
            Align = alTop
            BevelEdges = []
            BevelKind = bkFlat
            BevelOuter = bvNone
            Caption = 'VSTGrid'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentBackground = False
            ParentFont = False
            TabOrder = 0
            ExplicitLeft = 3
            ExplicitTop = 3
            ExplicitWidth = 320
          end
        end
        object pnlVSTList: TPanel
          Left = 0
          Top = 351
          Width = 325
          Height = 352
          Align = alClient
          BevelEdges = []
          BevelOuter = bvNone
          Caption = 'pnlVSTList'
          ParentColor = True
          TabOrder = 3
          ExplicitLeft = -6
          ExplicitTop = 357
          object pnlVSTListHeader: TPanel
            Left = 0
            Top = 0
            Width = 325
            Height = 20
            Align = alTop
            BevelEdges = []
            BevelKind = bkFlat
            BevelOuter = bvNone
            Caption = 'VSTList'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentBackground = False
            ParentFont = False
            TabOrder = 0
            ExplicitLeft = 3
            ExplicitTop = 3
            ExplicitWidth = 319
          end
        end
        object pnlVSTTreeList: TPanel
          Left = 325
          Top = 351
          Width = 325
          Height = 352
          Align = alClient
          BevelEdges = []
          BevelOuter = bvNone
          Caption = 'pnlVSTTreeList'
          ParentColor = True
          TabOrder = 4
          object pnlVSTTreeListHeader: TPanel
            Left = 0
            Top = 0
            Width = 325
            Height = 20
            Align = alTop
            BevelEdges = []
            BevelKind = bkFlat
            BevelOuter = bvNone
            Caption = 'VSTTreeList'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentBackground = False
            ParentFont = False
            TabOrder = 0
            ExplicitLeft = 3
            ExplicitTop = 3
            ExplicitWidth = 319
          end
        end
        object pnlVSTTreeGrid: TPanel
          Left = 650
          Top = 351
          Width = 326
          Height = 352
          Align = alClient
          BevelEdges = []
          BevelOuter = bvNone
          Caption = 'pnlVSTTreeGrid'
          ParentColor = True
          TabOrder = 5
          object pnlVSTTreeGridHeader: TPanel
            Left = 0
            Top = 0
            Width = 326
            Height = 20
            Align = alTop
            BevelEdges = []
            BevelKind = bkFlat
            BevelOuter = bvNone
            Caption = 'VSTTreeGrid'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentBackground = False
            ParentFont = False
            TabOrder = 0
            ExplicitLeft = 3
            ExplicitTop = 3
            ExplicitWidth = 320
          end
        end
      end
    end
    object tsVST: TTabSheet
      Caption = 'VST'
      ImageIndex = 5
    end
    object tsVSTTree: TTabSheet
      Caption = 'Tree'
    end
    object tsVSTGrid: TTabSheet
      Caption = 'Grid'
      ImageIndex = 1
    end
    object tsVSTList: TTabSheet
      Caption = 'List'
      ImageIndex = 2
    end
    object tsVSTTreeList: TTabSheet
      Caption = 'TreeList'
      ImageIndex = 3
    end
    object tsVSTTreeGrid: TTabSheet
      Caption = 'TreeGrid'
      ImageIndex = 4
    end
  end
end
