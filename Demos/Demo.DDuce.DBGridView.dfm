object frmDBGridView: TfrmDBGridView
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'TDBGridView testbench'
  ClientHeight = 978
  ClientWidth = 1310
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 144
  TextHeight = 23
  object splMain: TSplitter
    Left = 0
    Top = 506
    Width = 1310
    Height = 9
    Cursor = crVSplit
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    MinSize = 45
    ResizeStyle = rsLine
  end
  object pnlLog: TPanel
    Left = 0
    Top = 515
    Width = 1310
    Height = 463
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
  end
  object pgcMain: TPageControl
    Left = 0
    Top = 51
    Width = 1310
    Height = 455
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ActivePage = tsDBGridView
    Align = alClient
    TabOrder = 2
    object tsDBGridView: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'DBGridView'
      ImageIndex = 1
    end
    object tsDBGridViewEvents: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'DBGridView Events'
      ImageIndex = 2
      object pnlDBGridViewEvents: TPanel
        Left = 0
        Top = 0
        Width = 1302
        Height = 417
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object lbxDBGridViewEvents: TCheckListBox
          AlignWithMargins = True
          Left = 5
          Top = 5
          Width = 1292
          Height = 407
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Columns = 4
          ItemHeight = 26
          Items.Strings = (
            'OnCellAcceptCursor'
            'OnCellClick'
            'OnCellTips'
            'OnChange'
            'OnChangeColumns'
            'OnChangeEditing'
            'OnChangeEditMode'
            'OnChangeFixed'
            'OnChangeRows'
            'OnChanging'
            'OnCheckClick'
            'OnClearMultiSelect'
            'OnClick'
            'OnColumnAutoSize'
            'OnColumnResize'
            'OnColumnResizing'
            'OnDataActiveChanged'
            'OnDataChanged'
            'OnDataDeleteRecord'
            'OnDataEditError'
            'OnDataInsertRecord'
            'OnDataLayoutChanged'
            'OnDataUpdateError'
            'OnDataUpdateField'
            'OnDblClick'
            'OnDragDrop'
            'OnDragOver'
            'OnDraw'
            'OnDrawCell'
            'OnDrawHeader'
            'OnEditAcceptKey'
            'OnEditButtonPress'
            'OnEditCanceled'
            'OnEditCanModify'
            'OnEditCanShow'
            'OnEditChange'
            'OnEditCloseUp'
            'OnEditSelectNext'
            'OnEndDrag'
            'OnEnter'
            'OnExit'
            'OnGetCellColors'
            'OnGetCellHintRect'
            'OnGetCellImage'
            'OnGetCellImageIndent'
            'OnGetCellReadOnly'
            'OnGetCellText'
            'OnGetCellTextIndent'
            'OnGetCheckAlignment'
            'OnGetCheckImage'
            'OnGetCheckIndent'
            'OnGetCheckKind'
            'OnGetCheckState'
            'OnGetEditList'
            'OnGetEditListBounds'
            'OnGetEditMask'
            'OnGetEditStyle'
            'OnGetEditText'
            'OnGetHeaderColors'
            'OnGetHeaderImage'
            'OnGetIndicatorImage'
            'OnGetSortDirection'
            'OnGetSortImage'
            'OnGetTipsRect'
            'OnGetTipsText'
            'OnHeaderClick'
            'OnHeaderClicking'
            'OnKeyDown'
            'OnKeyPress'
            'OnKeyUp'
            'OnMouseDown'
            'OnMouseMove'
            'OnMouseUp'
            'OnMouseWheelDown'
            'OnMouseWheelUp'
            'OnResize'
            'OnRowMultiSelect'
            'OnSetEditText'
            'OnStartDrag')
          TabOrder = 0
        end
      end
    end
    object tsDataSourceEvents: TTabSheet
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'DataSource Events'
      ImageIndex = 2
      object pnlDataSourceEvents: TPanel
        Left = 0
        Top = 0
        Width = 1302
        Height = 417
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object lbxDataSourceEvents: TCheckListBox
          AlignWithMargins = True
          Left = 5
          Top = 5
          Width = 1292
          Height = 407
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Columns = 4
          ItemHeight = 26
          Items.Strings = (
            'OnDataChange'
            'OnStateChange'
            'OnUpdateData')
          TabOrder = 0
        end
      end
    end
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 1310
    Height = 51
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      1310
      51)
    object chkActive: TCheckBox
      Left = 1145
      Top = 12
      Width = 145
      Height = 26
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akTop, akRight]
      Caption = 'DataSet active'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkActiveClick
    end
    object btnAutoSizeDisplayWidths: TButton
      Left = 6
      Top = 5
      Width = 225
      Height = 37
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actAutoSizeColumns
      TabOrder = 1
    end
    object btnClearLog: TButton
      Left = 240
      Top = 5
      Width = 225
      Height = 37
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actClearLog
      TabOrder = 2
    end
    object chkMultiselect: TCheckBox
      Left = 990
      Top = 12
      Width = 146
      Height = 26
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akTop, akRight]
      Caption = 'Multiselect'
      TabOrder = 3
      OnClick = chkMultiselectClick
    end
    object btnInspectComponent: TButton
      Left = 474
      Top = 5
      Width = 260
      Height = 37
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actInspectComponent
      TabOrder = 4
    end
    object chkConnectEvents: TCheckBox
      Left = 819
      Top = 12
      Width = 146
      Height = 26
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akTop, akRight]
      Caption = 'Connect events'
      TabOrder = 5
      OnClick = chkConnectEventsClick
    end
  end
  object dscMain: TDataSource
    OnStateChange = dscMainStateChange
    OnDataChange = dscMainDataChange
    OnUpdateData = dscMainUpdateData
    Left = 120
    Top = 544
  end
  object aclMain: TActionList
    Left = 216
    Top = 544
    object actInspectComponent: TAction
      Caption = 'Inspect component (CTRL-F1)'
      ShortCut = 16496
      Visible = False
    end
    object actAutoSizeColumns: TAction
      Caption = 'Autosize columns'
      OnExecute = actAutoSizeColumnsExecute
    end
    object actClearLog: TAction
      Caption = 'Clear log'
      OnExecute = actClearLogExecute
    end
  end
  object imcMain: TImageCollection
    Images = <
      item
        Name = 'Item1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002E34944415478DA6D935B48937118C67F9FCEA9CD9C399D6DE171666E
              532BCB56A81DA88B444830BA4C8248820ED47574B88A8E775E44055D18DD8886
              4659627430A3311A666C5ACD524807F33447DFE6E60E7DFB24CDEC81F7E2FBF3
              3ECFFFF9BFDFF30A3109FC0387C381D7EB251008C8DFA9A9A964646460369BFF
              6D45F85BC0E974323131417676367ABD9E70382C9F2B140AC6C7C7999A9A92CF
              4D26D36A01ABD5CAFCFC3C2525253CF3F462173FF329E0949B36A79AA8549573
              306B1FDFBEB96447168B6559607070909999190A0A0A38FFE332BE34118D4683
              52A9949B82C120D3D3D3A8C5346EE55D6474744C76595151B128D0DDDD2DD932
              72F6EB05C2399022DD1027FD8DB898288AA44C257223EF122E978BFAFA7A04BB
              DD1E0B85827C58F84887F002D5DAB5F8FDFE15E438D1B6BF87479EC7B40CDDE3
              8850C79688591AEC3A849E9E9E5851512157BEDC6052E763C2ED96DF185A08C9
              C4803FC0FBDD4F1104411EE67EDB612A13CD9CCB69969E3D8BD0DEDE2E091471
              ECFB69B4F97A9E94B56278BC1D31244204EC7B5FCAC47899BAAB25872AB212D5
              B468AFE1F17810DADADA628585851C1B39C5951D1730FA0D68B55A0CCFB73350
              FD6A895CDABB8BA8324A8A2299F5110D2D39D7999C9C44E8EAEA8AE5E6E67175
              E4361FD31D3CD9D04A5252D212315E9B5EEF24B22622CF2339AAE440B4865359
              27F0F9BC08FDFDFD31AFD7C7408283BBE203E6F063AF5CB65DF2CEC2827A6169
              A019B32ACEA49FA4D45F8C4EA75DFC8DADAD0F292BDBCA59E77986D25D042361
              BE5AAC145BAB0869424B64D57432E5A291DB1B6F323C3C4853D3D14581BEBE3E
              7EFE744B59AFA4D9761CA76298795D8468DA2231E19764DD9D80396CE4CEB6FB
              52E4EDE4E6EAA9ADAD5D8E7267672773737EAAAA76D335DACE3BF71BB9E2A8D1
              ED91EB50412336DB5BD46A150D0D0DAB97299EC8A1A1610C867229995B5684C9
              E91C6064E433466329757575FFDFC63FE8E8E8606C6C4CDE8F38323333C9CFCF
              A7B1B171D53AFF0653B046F71C3FCB210000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item2'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002E24944415478DA6D53DB4BD361187E7EB20D6DC716AE1D733A94E6D6
              48619A7904C1122F046F2B2F0AB4EB2E82344484A02EA6FD0F76518882DE0823
              45A722329B3173D3DCCCC50EE6710777D0E6D6B76F134B7BE1E3C7EFE37D9FEF
              79DFE7799934095C8AB5B535048341C4E371FA5F5050009148049D4E773915CC
              DF000E87037EBF1F85858590CBE5482693F49EC562C1E7F3617F7F9FDE979797
              5F05585A5A422291405959190E2C1644AC56C45756B20C2A2AC0331A21AEABC5
              E6A68B32AAAEAEBE00B0DBED383C3C845AADC6CF8101080201C865329A988958
              2C469945140A287A7BB0BDEDA12C0D06431660727292D0D2C2D5D70755340601
              8F470B2ACD660A606B6981542A45301442402484B2A7172E970B6D6D6D606C36
              5BFAF4F404C9D555703E7E824828442412A1A771719102CCD6D480CFE7534607
              4747C0E347F85D5A4A067B1D8CD96C4E979414E3FBDB7750EFEDE1D7CE0E2DCA
              00544D4F231C0E63BEB616A9B3337A2F572A81AA2A489E7793B68FC08C8E8E12
              80126C7575A194F4E5F77A714C5E8946A3D09116D86C36569A9A90979B3A4F20
              405A2281647010BBBBBB60464646D2C5C5C5707777439D9F0FDFC6067D2D23A0
              7666065C2E17CB4401560E804D721244CA9B2613F6086366626222AD52DDC28F
              A121DC5826D21105CE239CFB0AFE324E92C3C1E98387103F7D46DA0B82595858
              48078361B0DD5B88BD37414A244B9E9C2046920D4E2738A46059A3C1B51C4080
              0C92FFB207C74A0564324956C6E1E10FD0EB2BB0D9FF1AA22F564889A142A914
              F46E372DFA4600322C02641EE17BF7A1EEEDC7FABA1D9D9D4FB200737373F07A
              03C4EB95B0BF7A81BCC579A8498130F76A889C6D72523575D0BF3111CBDBA052
              C9515F5F7F61E5F1F171844231188D0D08CC7E866F660A7ECB5456BA8666289A
              9A216B6C86D56A8150C8457B7BFBD565CA38D2E95C8746738738F3EE3F5BE770
              7C85DBBD0AADF6365A5B5BFFBF8DE7313636068FC743F7231362B118454545E8
              E8E8B8B2CE7F00BFC54CF7B48B989D0000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item3'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000003084944415478DA6D935D685B6518C77F270989271F6BD6AD99392C36
              6DA5DA742BB6527AA155F1A258AA76F4C62BE7C52E36AD88378A2828280E8732
              1415C42B61CA84D242EB45A5301CD65ED4B1A8D166C5A52ED99A867CB43B09CD
              774E8E6FDF20D3D5E7E6F03E3CEFEFFD9FFFF33C8A2982BB626D6D0D5DD72997
              CBF2ACAA2A5EAF97FEFEFEBB4B51FE0D8846A36C6D6DD1D1D181A669341A0D99
              B7D96C24934972B99CCC8742A1FD80D5D5552A950ABDBDBD5CFA394F2456251A
              3764512868E5788F9D27873D5CBF1E938A464646EE002291083B3B3B048341DE
              F932474DF1E2F31DC26EB7B387AF56AB6432DBDCA3E479FB9497783C21550E0C
              0CB4008B8B8B42561F6F7E9EC67140C3E15085E40C0B1FB7A43EFB6A14BFBF83
              52A948B39C9690582CC6C4C4044A381C366BB52A57FFB47029E2C6E3F190CF97
              28168B2C7D3124016367C2B85C2E9C4E07854281A71E2E110A9484B107519696
              96CCEEEE2ECE7D95A46AEB249DD6E5A57ABDCEE6E62686D194261A46CB0F4DF3
              F1C05183D3275CE2B76FA3CCCECE0A40372F7FB44D508092C91C7A7E97A2907B
              F13D0D455178EEAD4DF1B54880C7ED44B59B7C70C6217CC9A0CCCCCC985D5D5D
              4C7FB88D4F3BCAC68D0C46B3896934B8F8EE61E9F889D76EA1586D12E0B0DB50
              AD15CEBDE8209BCDA22C2C2C9881C07D9CFF264D42F791D36BB2B0D9A83277F6
              306EB79BB157E2586C0E99B7D06030B8CBA9A755E1878EB2B2B262EA7A813F6E
              AA7CFB838945F553AB1B18B512DF9F3F22CD1B3DFD1756BB53021AC5142F8C59
              B9FF882E3AE36BB5F1C285AF39766C90D73FB941B6E2C3EABC977AA5C0E5CF5A
              1E3C3E9D148003D4765368EE2CEFBF14607D3DC2C993CFB700CBCBCBC2F19498
              F521A6CFFEC6ADDB4E5C8782D8D536F96AAD9CA7B81D2770B0C4A76F0C88910F
              1308688C8E8EDE19E5F9F979D9FFE1E1C7F8EEF24D56C2297EFA2525018F0EFA
              7964C8CF334F04B872E547DADA5C4C4E4EEE5FA6BD89BC766D9D9E9EE362321F
              FACFD645A3BFB2B1F13B7D7D0F323E3EFEFFDBF84FCCCDCD914824E47EEC457B
              7B3B9D9D9D4C4D4DED5BE7BF01C3305AF786C5BFFE0000000049454E44AE4260
              82}
          end>
      end
      item
        Name = 'Item4'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002DE4944415478DA6D53DF4B5361187E8E734BC9DC5AB8D899CB4DD94C
              5DB389D3586E0582255E082278517951A05D7711A42122047531ED7FB08BC45F
              288130356CCB912D664C377F6DEA623F42CD36AD6DDA6C7DE7CC666A0FBC1CBE
              8FEF7DCEF3BEEFF35209029C80D3E944281442341A65CF999999100804282929
              39F914D4BF042E970B814000393939A0691AF1789CBD4F4F4F87DFEFC7D6D616
              7B5F5C5C7C9A60666606B1580C4AA512DFCC66ECDA6C88CECE26156834C8D26A
              21ACBA8E951537ABA8B2B2F288C0E170607B7B1B32990C5FBABA901D0C82168B
              D9870C229108AB6C572281A4BD0DEBEB5E56A55AAD4E128C8D8D115945707774
              40FA3302E5C000D69A9A8ED52AEFEB83ABBE1E41011FB96DED70BBDDA8ABAB03
              65B7DB13FBFB7B88CFCD81F7BA0FDA89895492BDA686FD96994CA9BBF7060370
              F70E7E2914A4B1E741994CA6447EBE1CCBCF5F40B6B909E7FC3C1A093BD38F70
              38CCCA3F3838405A5A1ADE9697432295021515103D6C25657F073538384808F2
              B1DAD20205A92BE0F3E12BE9B87A7C1C1C0E075C2E173C1E0F16D2440E519095
              9D8D84480451773736363640F5F7F727E472393CADAD906564C0BFB484DFE48F
              8AA9A95432131F4B4BD912B8E44D8C8CF2A2D1884DA2981A1D1D4D48A597B0D6
              D3830B9FC8E8C804C42792191F30612F2C449C9CF76FDD86F0FE03ECEC84404D
              4F4F2742A11D703DAB88BC34423E3C9C4A9E27B219E8882A8680C1A44A85738F
              DBF0235702B158941C636FEF2BA8541AAC743EC5DE072B74562B9649327DD8F9
              00896B1E0FDE10939DD15541D6DE89C545079A9BEF25092C160B7CBE20F17A19
              1C4F1E216CB5404351E01F128449CC12C3F2757AA89E1989E5ED904A69E8F5FA
              232B8F8C8C90B145A0D51A107C3701FFD42402E649968036544372B31AE21BD5
              B0D9CCE0F3CFA29E98EAD432318E5C58584441C115E2CCABC79CE8727D86C733
              87A2A2CBA8ADADFDFF36FEC5D0D010BC5E2FBB1F0C844221F2F2F2D0D0D0706A
              9DFF00E61E40F7B6281B5F0000000049454E44AE426082}
          end>
      end>
    Left = 120
    Top = 659
  end
  object imlMain: TVirtualImageList
    AutoFill = True
    DisabledGrayscale = True
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Item1'
        Name = 'Item1'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Item2'
        Name = 'Item2'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Item3'
        Name = 'Item3'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Item4'
        Name = 'Item4'
      end>
    ImageCollection = imcMain
    Left = 228
    Top = 659
  end
end
