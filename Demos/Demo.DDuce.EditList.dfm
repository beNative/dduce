object frmEditList: TfrmEditList
  Left = 0
  Top = 0
  ClientHeight = 411
  ClientWidth = 734
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 321
    Top = 0
    Width = 5
    Height = 411
    ExplicitTop = -129
    ExplicitHeight = 540
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 321
    Height = 411
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlRight: TPanel
    Left = 326
    Top = 0
    Width = 408
    Height = 411
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      408
      411)
    object edtName: TLabeledEdit
      Left = 42
      Top = 64
      Width = 359
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 31
      EditLabel.Height = 13
      EditLabel.Caption = 'Name:'
      LabelPosition = lpLeft
      TabOrder = 0
      Text = 'SomeName'
    end
    object edtValue: TLabeledEdit
      Left = 42
      Top = 91
      Width = 359
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 30
      EditLabel.Height = 13
      EditLabel.Caption = 'Value:'
      LabelPosition = lpLeft
      TabOrder = 1
      Text = '48'
    end
    object mmoData: TMemo
      Left = 42
      Top = 128
      Width = 359
      Height = 113
      Anchors = [akLeft, akTop, akRight]
      BevelInner = bvNone
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Consolas'
      Font.Style = []
      ParentColor = True
      ParentFont = False
      ReadOnly = True
      TabOrder = 2
    end
  end
  object chkMultiSelect: TCheckBox
    Left = 368
    Top = 32
    Width = 97
    Height = 17
    Caption = 'MultiSelect'
    TabOrder = 2
    OnClick = chkMultiSelectClick
  end
end
