object frmVirtualTrees: TfrmVirtualTrees
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  ClientHeight = 1097
  ClientWidth = 1485
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  PixelsPerInch = 144
  TextHeight = 23
  object pgcMain: TPageControl
    Left = 0
    Top = 0
    Width = 1485
    Height = 1097
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ActivePage = tsSettings
    Align = alClient
    TabOrder = 0
    object tsSettings: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Options'
      ImageIndex = 6
      object pnlOptions: TGridPanel
        Left = 0
        Top = 0
        Width = 1477
        Height = 1059
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
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
            Value = 33.333333333333310000
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
          Width = 492
          Height = 530
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          BevelEdges = []
          BevelOuter = bvNone
          Caption = 'pnlVST'
          ParentColor = True
          TabOrder = 0
          object pnlVSTHeader: TPanel
            Left = 0
            Top = 0
            Width = 492
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alTop
            BevelEdges = []
            BevelKind = bkFlat
            BevelOuter = bvNone
            Caption = 'VST'
            Color = clWindow
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -17
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentBackground = False
            ParentFont = False
            TabOrder = 0
          end
        end
        object pnlVSTTree: TPanel
          Left = 492
          Top = 0
          Width = 493
          Height = 530
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          BevelEdges = []
          BevelOuter = bvNone
          Caption = 'pnlVSTTree'
          ParentColor = True
          TabOrder = 1
          object pnlVSTTreeHeader: TPanel
            Left = 0
            Top = 0
            Width = 493
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alTop
            BevelEdges = []
            BevelKind = bkFlat
            BevelOuter = bvNone
            Caption = 'VSTTree'
            Color = clWindow
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -17
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentBackground = False
            ParentFont = False
            TabOrder = 0
          end
        end
        object pnlVSTGrid: TPanel
          Left = 985
          Top = 0
          Width = 492
          Height = 530
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          BevelEdges = []
          BevelOuter = bvNone
          Caption = 'pnlVSTGrid'
          ParentColor = True
          TabOrder = 2
          object pnlVSTGridHeader: TPanel
            Left = 0
            Top = 0
            Width = 492
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alTop
            BevelEdges = []
            BevelKind = bkFlat
            BevelOuter = bvNone
            Caption = 'VSTGrid'
            Color = clWindow
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -17
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentBackground = False
            ParentFont = False
            TabOrder = 0
          end
        end
        object pnlVSTList: TPanel
          Left = 0
          Top = 530
          Width = 492
          Height = 529
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          BevelEdges = []
          BevelOuter = bvNone
          Caption = 'pnlVSTList'
          ParentColor = True
          TabOrder = 3
          object pnlVSTListHeader: TPanel
            Left = 0
            Top = 0
            Width = 492
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alTop
            BevelEdges = []
            BevelKind = bkFlat
            BevelOuter = bvNone
            Caption = 'VSTList'
            Color = clWindow
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -17
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentBackground = False
            ParentFont = False
            TabOrder = 0
          end
        end
        object pnlVSTTreeList: TPanel
          Left = 492
          Top = 530
          Width = 493
          Height = 529
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          BevelEdges = []
          BevelOuter = bvNone
          Caption = 'pnlVSTTreeList'
          ParentColor = True
          TabOrder = 4
          object pnlVSTTreeListHeader: TPanel
            Left = 0
            Top = 0
            Width = 493
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alTop
            BevelEdges = []
            BevelKind = bkFlat
            BevelOuter = bvNone
            Caption = 'VSTTreeList'
            Color = clWindow
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -17
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentBackground = False
            ParentFont = False
            TabOrder = 0
          end
        end
        object pnlVSTTreeGrid: TPanel
          Left = 985
          Top = 530
          Width = 492
          Height = 529
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          BevelEdges = []
          BevelOuter = bvNone
          Caption = 'pnlVSTTreeGrid'
          ParentColor = True
          TabOrder = 5
          object pnlVSTTreeGridHeader: TPanel
            Left = 0
            Top = 0
            Width = 492
            Height = 30
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alTop
            BevelEdges = []
            BevelKind = bkFlat
            BevelOuter = bvNone
            Caption = 'VSTTreeGrid'
            Color = clWindow
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -17
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentBackground = False
            ParentFont = False
            TabOrder = 0
          end
        end
      end
    end
    object tsVST: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'VST'
      ImageIndex = 5
    end
    object tsVSTTree: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Tree'
    end
    object tsVSTGrid: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Grid'
      ImageIndex = 1
    end
    object tsVSTList: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'List'
      ImageIndex = 2
    end
    object tsVSTTreeList: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'TreeList'
      ImageIndex = 3
    end
    object tsVSTTreeGrid: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'TreeGrid'
      ImageIndex = 4
    end
  end
end
