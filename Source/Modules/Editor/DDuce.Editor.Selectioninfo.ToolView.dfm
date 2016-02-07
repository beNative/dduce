object frmSelectionInfo: TfrmSelectionInfo
  Left = -652
  Top = 203
  ClientHeight = 615
  ClientWidth = 358
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    358
    615)
  PixelsPerInch = 96
  TextHeight = 13
  object lblStoredBlockBegin: TLabel
    Left = 8
    Top = 8
    Width = 86
    Height = 13
    Caption = 'StoredBlockBegin:'
    Color = clBtnFace
    ParentColor = False
  end
  object lblStoredBlockEnd: TLabel
    Left = 8
    Top = 36
    Width = 78
    Height = 13
    Caption = 'StoredBlockEnd:'
    Color = clBtnFace
    ParentColor = False
  end
  object lblStoredBlockBeginValue: TLabel
    Left = 156
    Top = 8
    Width = 118
    Height = 13
    Caption = 'lblStoredBlockBeginValue'
    Color = clBtnFace
    ParentColor = False
  end
  object lblStoredBlockEndValue: TLabel
    Left = 156
    Top = 36
    Width = 110
    Height = 13
    Caption = 'lblStoredBlockEndValue'
    Color = clBtnFace
    ParentColor = False
  end
  object lblStoredBlockLines: TLabel
    Left = 10
    Top = 304
    Width = 114
    Height = 13
    Caption = 'lblStoredBlockLines'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object lblStoredBlockSelectionMode: TLabel
    Left = 8
    Top = 80
    Width = 129
    Height = 13
    Caption = 'StoredBlockSelectionMode:'
    Color = clBtnFace
    ParentColor = False
  end
  object lblStoredBlockSelectionModeValue: TLabel
    Left = 156
    Top = 80
    Width = 161
    Height = 13
    Caption = 'lblStoredBlockSelectionModeValue'
    Color = clBtnFace
    ParentColor = False
  end
  object lblBlockBegin: TLabel
    Left = 8
    Top = 104
    Width = 54
    Height = 13
    Caption = 'BlockBegin:'
    Color = clBtnFace
    ParentColor = False
  end
  object lblBlockEnd: TLabel
    Left = 8
    Top = 132
    Width = 46
    Height = 13
    Caption = 'BlockEnd:'
    Color = clBtnFace
    ParentColor = False
  end
  object lblBlockBeginValue: TLabel
    Left = 156
    Top = 104
    Width = 86
    Height = 13
    Caption = 'lblBlockBeginValue'
    Color = clBtnFace
    ParentColor = False
  end
  object lblBlockEndValue: TLabel
    Left = 156
    Top = 132
    Width = 78
    Height = 13
    Caption = 'lblBlockEndValue'
    Color = clBtnFace
    ParentColor = False
  end
  object lblCaretXY: TLabel
    Left = 10
    Top = 160
    Width = 43
    Height = 13
    Caption = 'CaretXY:'
    Color = clBtnFace
    ParentColor = False
  end
  object lblCaretXYValue: TLabel
    Left = 156
    Top = 160
    Width = 75
    Height = 13
    Caption = 'lblCaretXYValue'
    Color = clBtnFace
    ParentColor = False
  end
  object lblLogicalCaretXY: TLabel
    Left = 10
    Top = 184
    Width = 75
    Height = 13
    Caption = 'LogicalCaretXY:'
    Color = clBtnFace
    ParentColor = False
  end
  object lblLogicalCaretXYValue: TLabel
    Left = 156
    Top = 184
    Width = 107
    Height = 13
    Caption = 'lblLogicalCaretXYValue'
    Color = clBtnFace
    ParentColor = False
  end
  object lblStoredCaretXY: TLabel
    Left = 8
    Top = 56
    Width = 75
    Height = 13
    Caption = 'StoredCaretXY:'
    Color = clBtnFace
    ParentColor = False
  end
  object lblStoredCaretXYValue: TLabel
    Left = 156
    Top = 56
    Width = 107
    Height = 13
    Caption = 'lblStoredCaretXYValue'
    Color = clBtnFace
    ParentColor = False
  end
  object lblLineCount: TLabel
    Left = 10
    Top = 208
    Width = 52
    Height = 13
    Caption = 'LineCount:'
    Color = clBtnFace
    ParentColor = False
  end
  object lblLineCountValue: TLabel
    Left = 156
    Top = 208
    Width = 84
    Height = 13
    Caption = 'lblLineCountValue'
    Color = clBtnFace
    ParentColor = False
  end
  object btnStore: TButton
    Left = 8
    Top = 232
    Width = 100
    Height = 25
    Caption = 'Store'
    TabOrder = 0
    OnClick = btnStoreClick
  end
  object btnRestore: TButton
    Left = 8
    Top = 264
    Width = 100
    Height = 25
    Caption = 'Restore'
    TabOrder = 1
    OnClick = btnRestoreClick
  end
  object chkLockUpdates: TCheckBox
    Left = 120
    Top = 235
    Width = 88
    Height = 19
    Caption = 'LockUpdates'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object chkExcludeEmptyLines: TCheckBox
    Left = 224
    Top = 235
    Width = 121
    Height = 19
    Caption = 'ExcludeEmptyLines'
    TabOrder = 3
  end
  object mmoBlock: TMemo
    Left = 16
    Top = 408
    Width = 327
    Height = 194
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
    OnChange = mmoBlockChange
  end
end
