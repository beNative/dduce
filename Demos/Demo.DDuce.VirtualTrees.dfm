object frmVirtualTrees: TfrmVirtualTrees
  Left = 0
  Top = 0
  ClientHeight = 731
  ClientWidth = 984
  Color = clBtnFace
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
        ExplicitLeft = 400
        ExplicitTop = 328
        ExplicitWidth = 185
        ExplicitHeight = 41
        object pnlVST: TPanel
          Left = 0
          Top = 0
          Width = 324
          Height = 350
          Align = alClient
          BevelOuter = bvNone
          Caption = 'pnlVST'
          ParentColor = True
          TabOrder = 0
          ExplicitLeft = -5
          ExplicitTop = -5
          object pnlVSTHeader: TPanel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 318
            Height = 20
            Align = alTop
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
          end
        end
        object pnlVSTTree: TPanel
          Left = 324
          Top = 0
          Width = 324
          Height = 350
          Align = alClient
          BevelOuter = bvNone
          Caption = 'pnlVSTTree'
          ParentColor = True
          TabOrder = 1
          ExplicitLeft = 400
          ExplicitTop = 328
          ExplicitWidth = 185
          ExplicitHeight = 41
          object pnlVSTTreeHeader: TPanel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 318
            Height = 20
            Align = alTop
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
          end
        end
        object pnlVSTGrid: TPanel
          Left = 648
          Top = 0
          Width = 326
          Height = 350
          Align = alClient
          BevelOuter = bvNone
          Caption = 'pnlVSTGrid'
          ParentColor = True
          TabOrder = 2
          ExplicitLeft = 400
          ExplicitTop = 328
          ExplicitWidth = 185
          ExplicitHeight = 41
          object pnlVSTGridHeader: TPanel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 320
            Height = 20
            Align = alTop
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
          end
        end
        object pnlVSTList: TPanel
          Left = 0
          Top = 350
          Width = 324
          Height = 351
          Align = alClient
          BevelOuter = bvNone
          Caption = 'pnlVSTList'
          ParentColor = True
          TabOrder = 3
          ExplicitLeft = 400
          ExplicitTop = 328
          ExplicitWidth = 185
          ExplicitHeight = 41
          object pnlVSTListHeader: TPanel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 318
            Height = 20
            Align = alTop
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
          end
        end
        object pnlVSTTreeList: TPanel
          Left = 324
          Top = 350
          Width = 324
          Height = 351
          Align = alClient
          BevelOuter = bvNone
          Caption = 'pnlVSTTreeList'
          ParentColor = True
          TabOrder = 4
          ExplicitLeft = 400
          ExplicitTop = 328
          ExplicitWidth = 185
          ExplicitHeight = 41
          object pnlVSTTreeListHeader: TPanel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 318
            Height = 20
            Align = alTop
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
          end
        end
        object pnlVSTTreeGrid: TPanel
          Left = 648
          Top = 350
          Width = 326
          Height = 351
          Align = alClient
          BevelOuter = bvNone
          Caption = 'pnlVSTTreeGrid'
          ParentColor = True
          TabOrder = 5
          ExplicitLeft = 400
          ExplicitTop = 328
          ExplicitWidth = 185
          ExplicitHeight = 41
          object pnlVSTTreeGridHeader: TPanel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 320
            Height = 20
            Align = alTop
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
          end
        end
      end
    end
    object tsVST: TTabSheet
      Caption = 'VST'
      ImageIndex = 5
      ExplicitWidth = 1024
    end
    object tsVSTTree: TTabSheet
      Caption = 'Tree'
      ExplicitWidth = 844
      ExplicitHeight = 383
    end
    object tsVSTGrid: TTabSheet
      Caption = 'Grid'
      ImageIndex = 1
      ExplicitLeft = 8
      ExplicitTop = 28
    end
    object tsVSTList: TTabSheet
      Caption = 'List'
      ImageIndex = 2
      ExplicitWidth = 844
      ExplicitHeight = 383
    end
    object tsVSTTreeList: TTabSheet
      Caption = 'TreeList'
      ImageIndex = 3
      ExplicitLeft = 8
      ExplicitTop = 28
    end
    object tsVSTTreeGrid: TTabSheet
      Caption = 'TreeGrid'
      ImageIndex = 4
      ExplicitWidth = 844
      ExplicitHeight = 383
    end
  end
end
