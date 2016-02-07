inherited frmAlignLines: TfrmAlignLines
  Left = -521
  Height = 729
  Top = 253
  Width = 360
  Caption = 'Align selection'
  ChildSizing.TopBottomSpacing = 4
  ChildSizing.HorizontalSpacing = 4
  ChildSizing.VerticalSpacing = 4
  ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
  ChildSizing.ShrinkHorizontal = crsHomogenousChildResize
  ChildSizing.Layout = cclLeftToRightThenTopToBottom
  ChildSizing.ControlsPerLine = 1
  ClientHeight = 729
  ClientWidth = 360
  Constraints.MinWidth = 170
  OnResize = FormResize
  OnShow = FormShow
  object sbrMain: TScrollBox[0]
    Left = 4
    Height = 688
    Top = 4
    Width = 352
    HorzScrollBar.Page = 352
    VertScrollBar.Page = 688
    Align = alClient
    BorderSpacing.Bottom = 4
    BorderSpacing.Around = 4
    BorderSpacing.CellAlignHorizontal = ccaCenter
    BorderSpacing.CellAlignVertical = ccaCenter
    BorderStyle = bsNone
    ChildSizing.TopBottomSpacing = 4
    ChildSizing.HorizontalSpacing = 4
    ChildSizing.VerticalSpacing = 4
    ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
    ChildSizing.EnlargeVertical = crsHomogenousChildResize
    ChildSizing.ShrinkHorizontal = crsHomogenousChildResize
    ChildSizing.ShrinkVertical = crsHomogenousChildResize
    ChildSizing.Layout = cclLeftToRightThenTopToBottom
    ChildSizing.ControlsPerLine = 1
    ClientHeight = 688
    ClientWidth = 352
    TabOrder = 0
    object gbxOptions: TCheckGroup
      Left = 0
      Height = 100
      Top = 4
      Width = 352
      AutoFill = True
      Caption = 'Options'
      ChildSizing.LeftRightSpacing = 20
      ChildSizing.TopBottomSpacing = 6
      ChildSizing.VerticalSpacing = 4
      ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
      ChildSizing.EnlargeVertical = crsHomogenousChildResize
      ChildSizing.ShrinkHorizontal = crsScaleChilds
      ChildSizing.ShrinkVertical = crsScaleChilds
      ChildSizing.Layout = cclLeftToRightThenTopToBottom
      ChildSizing.ControlsPerLine = 1
      ClientHeight = 82
      ClientWidth = 348
      Items.Strings = (
        'Remove whitespace from text before aligning'
        'Align in paragraphs'
        'Sort after aligning'
      )
      OnItemClick = gbxOptionsItemClick
      TabOrder = 0
      Data = {
        03000000020202
      }
    end
    object rgpSortDirection: TRadioGroup
      Left = 0
      Height = 54
      Top = 108
      Width = 352
      AutoFill = True
      Caption = 'Sort direction:'
      ChildSizing.LeftRightSpacing = 20
      ChildSizing.TopBottomSpacing = 6
      ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
      ChildSizing.EnlargeVertical = crsHomogenousChildResize
      ChildSizing.ShrinkHorizontal = crsScaleChilds
      ChildSizing.ShrinkVertical = crsScaleChilds
      ChildSizing.Layout = cclLeftToRightThenTopToBottom
      ChildSizing.ControlsPerLine = 2
      ClientHeight = 36
      ClientWidth = 348
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Ascending'
        'Descending'
      )
      OnClick = rgpSortDirectionClick
      TabOrder = 1
    end
    object gbxInsertSpace: TCheckGroup
      Left = 0
      Height = 54
      Top = 166
      Width = 352
      AutoFill = True
      Caption = 'Keep at least one space'
      ChildSizing.LeftRightSpacing = 20
      ChildSizing.TopBottomSpacing = 6
      ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
      ChildSizing.EnlargeVertical = crsHomogenousChildResize
      ChildSizing.ShrinkHorizontal = crsScaleChilds
      ChildSizing.ShrinkVertical = crsScaleChilds
      ChildSizing.Layout = cclLeftToRightThenTopToBottom
      ChildSizing.ControlsPerLine = 2
      ClientHeight = 36
      ClientWidth = 348
      Columns = 2
      Items.Strings = (
        'Before token'
        'After token'
      )
      OnItemClick = gbxInsertSpaceItemClick
      TabOrder = 2
      Data = {
        020000000202
      }
    end
    object rgpAlignAt: TRadioGroup
      Left = 0
      Height = 54
      Top = 224
      Width = 352
      AutoFill = True
      Caption = 'Align at:'
      ChildSizing.LeftRightSpacing = 20
      ChildSizing.TopBottomSpacing = 6
      ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
      ChildSizing.EnlargeVertical = crsHomogenousChildResize
      ChildSizing.ShrinkHorizontal = crsScaleChilds
      ChildSizing.ShrinkVertical = crsScaleChilds
      ChildSizing.Layout = cclLeftToRightThenTopToBottom
      ChildSizing.ControlsPerLine = 2
      ClientHeight = 36
      ClientWidth = 348
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Leftmost token'
        'Rightmost token'
      )
      OnClick = rgpAlignAtClick
      TabOrder = 3
      Visible = False
    end
    object pnlTokens: TOMultiPanel
      Left = 0
      Height = 402
      Top = 282
      Width = 352
      PanelCollection = <      
        item
          Control = gbxTokenList
          Position = 0.5
          Visible = True
          Index = 0
        end      
        item
          Control = gbxTokensFound
          Position = 1
          Visible = True
          Index = 1
        end>
      MinPosition = 0.02
      Constraints.MinHeight = 400
      TabOrder = 4
      object gbxTokenList: TGroupBox
        Left = 0
        Height = 402
        Top = 0
        Width = 176
        Align = alClient
        Caption = 'Tokens to align to:'
        ClientHeight = 384
        ClientWidth = 172
        TabOrder = 0
        object mmoTokens: TMemo
          Left = 4
          Height = 376
          Top = 4
          Width = 164
          Align = alClient
          BorderSpacing.Around = 4
          Font.Name = 'Consolas'
          OnChange = mmoTokensChange
          ParentFont = False
          ScrollBars = ssAutoBoth
          TabOrder = 0
        end
      end
      object gbxTokensFound: TGroupBox
        Left = 179
        Height = 402
        Top = 0
        Width = 173
        Align = alClient
        Caption = 'Tokens found in selection:'
        ChildSizing.ControlsPerLine = 2
        ClientHeight = 384
        ClientWidth = 169
        TabOrder = 1
        object pnlVST: TPanel
          Left = 4
          Height = 376
          Top = 4
          Width = 161
          Align = alClient
          BorderSpacing.Around = 4
          BevelOuter = bvNone
          ChildSizing.ControlsPerLine = 2
          TabOrder = 0
        end
      end
    end
  end
  object pnlBottom: TPanel[1]
    Left = 0
    Height = 25
    Top = 700
    Width = 360
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    ClientHeight = 25
    ClientWidth = 360
    TabOrder = 1
    object btnOK: TButton
      Left = 233
      Height = 25
      Top = -1
      Width = 120
      Action = actExecute
      Anchors = [akRight, akBottom]
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object aclMain: TActionList[2]
    left = 152
    top = 472
    object actExecute: TAction
      Caption = 'Execute'
      OnExecute = actExecuteExecute
    end
  end
end
