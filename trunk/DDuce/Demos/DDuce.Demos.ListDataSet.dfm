object frmListDataSet: TfrmListDataSet
  Left = 0
  Top = 0
  ClientHeight = 683
  ClientWidth = 1096
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poScreenCenter
  ShowHint = True
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1096
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    object lblRecordCount: TLabel
      Left = 3
      Top = 7
      Width = 47
      Height = 13
      Caption = 'Listcount:'
    end
    object edtRecordCount: TEdit
      Left = 56
      Top = 2
      Width = 68
      Height = 21
      Alignment = taRightJustify
      TabOrder = 0
      Text = '100'
    end
    object btnExecute: TButton
      Left = 135
      Top = 0
      Width = 153
      Height = 25
      Action = actFillList
      Default = True
      TabOrder = 1
    end
  end
  object pnlClient: TPanel
    Left = 0
    Top = 33
    Width = 1096
    Height = 631
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object splVertical: TSplitter
      Left = 521
      Top = 0
      Width = 7
      Height = 631
      ExplicitLeft = 528
      ExplicitTop = 6
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 521
      Height = 631
      Align = alLeft
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      object pnlLeftHeader: TPanel
        Left = 0
        Top = 0
        Width = 521
        Height = 50
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        FullRepaint = False
        TabOrder = 0
        DesignSize = (
          521
          50)
        object pnlDataAware: TPanel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 515
          Height = 17
          Align = alTop
          BevelKind = bkFlat
          BevelOuter = bvNone
          Caption = 
            'TDBGridView -> TDataSource -> TListDataSet<TContact> -> TObjectL' +
            'ist<TContact>'
          FullRepaint = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
        end
        object navDataSet: TDBNavigator
          Left = 315
          Top = 26
          Width = 200
          Height = 18
          DataSource = dscMain
          Anchors = [akTop, akRight]
          Flat = True
          TabOrder = 1
        end
        object btnExecute2: TButton
          Left = 3
          Top = 25
          Width = 65
          Height = 25
          Action = actConnectDataSet
          TabOrder = 2
        end
        object btnExecute1: TButton
          Left = 74
          Top = 25
          Width = 65
          Height = 25
          Action = actDisconnectDataSet
          TabOrder = 3
        end
      end
      object pnlLeftFooter: TPanel
        Left = 0
        Top = 477
        Width = 521
        Height = 154
        Align = alBottom
        BevelOuter = bvNone
        FullRepaint = False
        TabOrder = 1
        object lblFirstname: TLabel
          Left = 5
          Top = 26
          Width = 51
          Height = 13
          Caption = 'Firstname:'
          FocusControl = edtDBFirstname
        end
        object lblLastname: TLabel
          Left = 133
          Top = 26
          Width = 50
          Height = 13
          Caption = 'Lastname:'
          FocusControl = edtDBLastname
        end
        object lblEmail: TLabel
          Left = 260
          Top = 26
          Width = 28
          Height = 13
          Caption = 'Email:'
          FocusControl = edtDBEmail
        end
        object lblCompanyName: TLabel
          Left = 5
          Top = 69
          Width = 75
          Height = 13
          Caption = 'Companyname:'
          FocusControl = edtDBCompanyName
        end
        object lblAddress: TLabel
          Left = 132
          Top = 69
          Width = 43
          Height = 13
          Caption = 'Address:'
          FocusControl = edtAddress
        end
        object lblCountry: TLabel
          Left = 132
          Top = 113
          Width = 43
          Height = 13
          Caption = 'Country:'
          FocusControl = edtDBCountry
        end
        object lblNumber: TLabel
          Left = 5
          Top = 112
          Width = 41
          Height = 13
          Caption = 'Number:'
          FocusControl = edtDBNumber
        end
        object edtDBFirstname: TDBEdit
          Left = 5
          Top = 42
          Width = 121
          Height = 21
          DataField = 'Firstname'
          DataSource = dscMain
          TabOrder = 0
        end
        object edtDBLastname: TDBEdit
          Left = 133
          Top = 42
          Width = 121
          Height = 21
          DataField = 'Lastname'
          DataSource = dscMain
          TabOrder = 1
        end
        object edtDBEmail: TDBEdit
          Left = 260
          Top = 42
          Width = 121
          Height = 21
          DataField = 'Email'
          DataSource = dscMain
          TabOrder = 2
        end
        object edtDBCompanyName: TDBEdit
          Left = 5
          Top = 85
          Width = 121
          Height = 21
          DataField = 'CompanyName'
          DataSource = dscMain
          TabOrder = 3
        end
        object edtDBAddress: TDBEdit
          Left = 132
          Top = 86
          Width = 229
          Height = 21
          DataField = 'Address'
          DataSource = dscMain
          TabOrder = 4
        end
        object edtDBNumber: TDBEdit
          Left = 5
          Top = 127
          Width = 121
          Height = 21
          DataField = 'Number'
          DataSource = dscMain
          TabOrder = 5
        end
        object edtDBCountry: TDBEdit
          Left = 132
          Top = 127
          Width = 121
          Height = 21
          DataField = 'Country'
          DataSource = dscMain
          TabOrder = 6
        end
        object pnlDataAwareControls: TPanel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 515
          Height = 17
          Align = alTop
          BevelKind = bkFlat
          BevelOuter = bvNone
          Caption = 'Standard VCL data-aware controls'
          FullRepaint = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 7
        end
      end
    end
    object pnlRight: TPanel
      Left = 528
      Top = 0
      Width = 568
      Height = 631
      Align = alClient
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 1
      object pnlRightHeader: TPanel
        Left = 0
        Top = 0
        Width = 568
        Height = 50
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        FullRepaint = False
        TabOrder = 0
        object pnlPresenter: TPanel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 562
          Height = 17
          Align = alTop
          BevelKind = bkFlat
          BevelOuter = bvNone
          Caption = 
            'TVirtualStringTree -> TTreeViewPresenter -> TObjectList<TContact' +
            '>'
          FullRepaint = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
        end
        object btnConnectPresenter: TButton
          Left = 6
          Top = 25
          Width = 65
          Height = 25
          Action = actConnectPresenter
          TabOrder = 1
        end
        object btnDisconnectPresenter: TButton
          Left = 77
          Top = 25
          Width = 65
          Height = 25
          Action = actDisconnectPresenter
          TabOrder = 2
        end
      end
      object pnlRightFooter: TPanel
        Left = 0
        Top = 477
        Width = 568
        Height = 154
        Align = alBottom
        BevelOuter = bvNone
        FullRepaint = False
        TabOrder = 1
        object edtFirstname: TLabeledEdit
          Left = 8
          Top = 42
          Width = 121
          Height = 21
          BevelInner = bvNone
          BevelOuter = bvNone
          EditLabel.Width = 51
          EditLabel.Height = 13
          EditLabel.Caption = 'Firstname:'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
        end
        object edtLastname: TLabeledEdit
          Left = 135
          Top = 42
          Width = 121
          Height = 21
          EditLabel.Width = 50
          EditLabel.Height = 13
          EditLabel.Caption = 'Lastname:'
          TabOrder = 1
        end
        object edtEmail: TLabeledEdit
          Left = 262
          Top = 42
          Width = 121
          Height = 21
          EditLabel.Width = 28
          EditLabel.Height = 13
          EditLabel.Caption = 'Email:'
          TabOrder = 2
        end
        object edtCompanyName: TLabeledEdit
          Left = 8
          Top = 82
          Width = 121
          Height = 21
          EditLabel.Width = 76
          EditLabel.Height = 13
          EditLabel.Caption = 'CompanyName:'
          TabOrder = 3
        end
        object edtAddress: TLabeledEdit
          Left = 135
          Top = 82
          Width = 233
          Height = 21
          EditLabel.Width = 43
          EditLabel.Height = 13
          EditLabel.Caption = 'Address:'
          TabOrder = 4
        end
        object edtNumber: TLabeledEdit
          Left = 6
          Top = 131
          Width = 121
          Height = 21
          EditLabel.Width = 41
          EditLabel.Height = 13
          EditLabel.Caption = 'Number:'
          TabOrder = 5
        end
        object edtCountry: TLabeledEdit
          Left = 133
          Top = 130
          Width = 121
          Height = 21
          EditLabel.Width = 43
          EditLabel.Height = 13
          EditLabel.Caption = 'Country:'
          TabOrder = 6
        end
        object pnlVCLControls: TPanel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 562
          Height = 17
          Align = alTop
          BevelKind = bkFlat
          BevelOuter = bvNone
          Caption = 
            'Standard VCL TEdit controls connected to TContact properties usi' +
            'ng bindings.'
          FullRepaint = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 7
        end
      end
    end
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 664
    Width = 1096
    Height = 19
    Panels = <>
  end
  object dscMain: TDataSource
    OnUpdateData = dscMainUpdateData
    Left = 352
    Top = 136
  end
  object aclMain: TActionList
    Left = 312
    Top = 136
    object actFillList: TAction
      Caption = 'Fill list with contacts'
      Hint = 'Fills a TObjectList<TContact> instance with TContact objects.'
      OnExecute = actFillListExecute
    end
    object actDisconnectDataSet: TAction
      Caption = 'Disconnect'
      OnExecute = actDisconnectDataSetExecute
    end
    object actConnectDataSet: TAction
      Caption = 'Connect'
      OnExecute = actConnectDataSetExecute
    end
    object actInspectComponents: TAction
      Caption = 'Inspect'
      OnExecute = actInspectComponentsExecute
    end
    object actConnectPresenter: TAction
      Caption = 'Connect'
      OnExecute = actConnectPresenterExecute
    end
    object actDisconnectPresenter: TAction
      Caption = 'Disconnect'
      OnExecute = actDisconnectPresenterExecute
    end
  end
end
