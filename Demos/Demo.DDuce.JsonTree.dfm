object frmJsonTree: TfrmJsonTree
  Left = 0
  Top = 0
  ClientHeight = 933
  ClientWidth = 1440
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1440
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    OnClick = pnlTopClick
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
      TabOrder = 3
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 32
    Width = 1440
    Height = 901
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlMain'
    TabOrder = 1
    object splVertical: TSplitter
      Left = 481
      Top = 0
      Width = 6
      Height = 901
      ResizeStyle = rsLine
      ExplicitLeft = 361
      ExplicitHeight = 516
    end
    object pnlTree: TPanel
      Left = 0
      Top = 0
      Width = 481
      Height = 901
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
    end
    object pnlEditor: TPanel
      Left = 487
      Top = 0
      Width = 953
      Height = 901
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object mmoJson: TMemo
        Left = 0
        Top = 0
        Width = 560
        Height = 901
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        Lines.Strings = (
          '{'
          #9'"DebugMode": false,'
          #9'"EmitLogMessages": true,'
          #9'"FormSettings": {'
          #9#9'"Left": 1036,'
          #9#9'"Top": 414,'
          #9#9'"Width": 1023,'
          #9#9'"Height": 499,'
          #9#9'"FormStyle": 0,'
          #9#9'"WindowState": 0'
          #9'},'
          #9'"MessageListSettings": {'
          #9#9'"AutoScrollMessages": true,'
          #9#9'"AutoFilterMessages": true,'
          #9#9'"ColumnHeadersVisible": true,'
          #9#9'"DynamicAutoSizeColumns": true,'
          #9#9'"MessageDetailsVisible": true,'
          #9#9'"SmartTimeStamps": true,'
          #9#9'"HorizontalPanelPositions": ['
          #9#9#9'0.232743867655448,'
          #9#9#9'0.836328125,'
          #9#9#9'1'
          #9#9'],'
          #9#9'"LeftVerticalPanelPositions": ['
          #9#9#9'0.204040404040404,'
          #9#9#9'1'
          #9#9']'
          #9'},'
          #9'"WinodsSettings": {'
          #9#9'"Enabled": true'
          #9'},'
          #9'"WinipcSettings": {'
          #9#9'"Enabled": false,'
          #9#9'"PollingInterval": 100'
          #9'},'
          #9'"ZmqSettings": {'
          #9#9'"Enabled": false,'
          #9#9'"PollingInterval": 100,'
          #9#9'"PollingTimeout": 10,'
          #9#9'"Endpoints": "New=tcp://SBVCTITPOA8:5555\r\n"'
          #9'},'
          #9'"FileSystemSettings": {'
          #9#9'"Enabled": false,'
          #9#9'"PathNames": ""'
          #9'},'
          #9'"ComPortSettings": {'
          #9#9'"ComPorts": "GPS=com30:115200\r\n"'
          #9'},'
          #9'"DisplayValuesSettings": {'
          #9#9'"Id": {'
          #9#9#9'"BackgroundColor": 16777215,'
          #9#9#9'"FontName": "Segoe UI",'
          #9#9#9'"FontColor": 8421504,'
          #9#9#9'"FontSize": 8,'
          #9#9#9'"FontStyle": "",'
          #9#9#9'"WordWrap": false,'
          #9#9#9'"HorizontalAlignment": 0,'
          #9#9#9'"VerticalAlignment": 0'
          #9#9'},'
          #9#9'"Info": {'
          #9#9#9'"BackgroundColor": 16711680,'
          #9#9#9'"FontName": "Segoe UI",'
          #9#9#9'"FontColor": 16777215,'
          #9#9#9'"FontSize": 8,'
          #9#9#9'"FontStyle": "fsBold",'
          #9#9#9'"WordWrap": false,'
          #9#9#9'"HorizontalAlignment": 0,'
          #9#9#9'"VerticalAlignment": 0'
          #9#9'},'
          #9#9'"Warning": {'
          #9#9#9'"BackgroundColor": 42495,'
          #9#9#9'"FontName": "Segoe UI",'
          #9#9#9'"FontColor": 16777215,'
          #9#9#9'"FontSize": 8,'
          #9#9#9'"FontStyle": "fsBold",'
          #9#9#9'"WordWrap": false,'
          #9#9#9'"HorizontalAlignment": 0,'
          #9#9#9'"VerticalAlignment": 0'
          #9#9'},'
          #9#9'"Error": {'
          #9#9#9'"BackgroundColor": 255,'
          #9#9#9'"FontName": "Segoe UI",'
          #9#9#9'"FontColor": 16777215,'
          #9#9#9'"FontSize": 8,'
          #9#9#9'"FontStyle": "fsBold",'
          #9#9#9'"WordWrap": false,'
          #9#9#9'"HorizontalAlignment": 0,'
          #9#9#9'"VerticalAlignment": 0'
          #9#9'},'
          #9#9'"TimeStamp": {'
          #9#9#9'"BackgroundColor": 15724527,'
          #9#9#9'"FontName": "Segoe UI",'
          #9#9#9'"FontColor": 16711680,'
          #9#9#9'"FontSize": 8,'
          #9#9#9'"FontStyle": "",'
          #9#9#9'"WordWrap": false,'
          #9#9#9'"HorizontalAlignment": 0,'
          #9#9#9'"VerticalAlignment": 0'
          #9#9'},'
          #9#9'"ValueName": {'
          #9#9#9'"BackgroundColor": 16777215,'
          #9#9#9'"FontName": "Segoe UI",'
          #9#9#9'"FontColor": 128,'
          #9#9#9'"FontSize": 8,'
          #9#9#9'"FontStyle": "",'
          #9#9#9'"WordWrap": false,'
          #9#9#9'"HorizontalAlignment": 0,'
          #9#9#9'"VerticalAlignment": 0'
          #9#9'},'
          #9#9'"ValueType": {'
          #9#9#9'"BackgroundColor": 16777215,'
          #9#9#9'"FontName": "Consolas",'
          #9#9#9'"FontColor": 8388608,'
          #9#9#9'"FontSize": 8,'
          #9#9#9'"FontStyle": "",'
          #9#9#9'"WordWrap": false,'
          #9#9#9'"HorizontalAlignment": 0,'
          #9#9#9'"VerticalAlignment": 0'
          #9#9'},'
          #9#9'"Value": {'
          #9#9#9'"BackgroundColor": 16777215,'
          #9#9#9'"FontName": "Consolas",'
          #9#9#9'"FontColor": 0,'
          #9#9#9'"FontSize": 8,'
          #9#9#9'"FontStyle": "fsBold",'
          #9#9#9'"WordWrap": false,'
          #9#9#9'"HorizontalAlignment": 0,'
          #9#9#9'"VerticalAlignment": 0'
          #9#9'},'
          #9#9'"CheckPoint": {'
          #9#9#9'"BackgroundColor": 32768,'
          #9#9#9'"FontName": "Segoe UI",'
          #9#9#9'"FontColor": 16777215,'
          #9#9#9'"FontSize": 8,'
          #9#9#9'"FontStyle": "fsBold",'
          #9#9#9'"WordWrap": false,'
          #9#9#9'"HorizontalAlignment": 0,'
          #9#9#9'"VerticalAlignment": 0'
          #9#9'},'
          #9#9'"Counter": {'
          #9#9#9'"BackgroundColor": 16777215,'
          #9#9#9'"FontName": "Segoe UI",'
          #9#9#9'"FontColor": 8388736,'
          #9#9#9'"FontSize": 8,'
          #9#9#9'"FontStyle": "",'
          #9#9#9'"WordWrap": false,'
          #9#9#9'"HorizontalAlignment": 0,'
          #9#9#9'"VerticalAlignment": 0'
          #9#9'},'
          #9#9'"Tracing": {'
          #9#9#9'"BackgroundColor": 15724527,'
          #9#9#9'"FontName": "Segoe UI",'
          #9#9#9'"FontColor": 32896,'
          #9#9#9'"FontSize": 8,'
          #9#9#9'"FontStyle": "fsBold,fsUnderline",'
          #9#9#9'"WordWrap": false,'
          #9#9#9'"HorizontalAlignment": 0,'
          #9#9#9'"VerticalAlignment": 0'
          #9#9'},'
          #9#9'"Enter": {'
          #9#9#9'"BackgroundColor": 15724527,'
          #9#9#9'"FontName": "Segoe UI",'
          #9#9#9'"FontColor": 32896,'
          #9#9#9'"FontSize": 8,'
          #9#9#9'"FontStyle": "fsBold",'
          #9#9#9'"WordWrap": false,'
          #9#9#9'"HorizontalAlignment": 0,'
          #9#9#9'"VerticalAlignment": 0'
          #9#9'},'
          #9#9'"Leave": {'
          #9#9#9'"BackgroundColor": 15724527,'
          #9#9#9'"FontName": "Segoe UI",'
          #9#9#9'"FontColor": 32896,'
          #9#9#9'"FontSize": 8,'
          #9#9#9'"FontStyle": "fsBold",'
          #9#9#9'"WordWrap": false,'
          #9#9#9'"HorizontalAlignment": 0,'
          #9#9#9'"VerticalAlignment": 0'
          #9#9'},'
          #9#9'"Conditional": {'
          #9#9#9'"BackgroundColor": 16777215,'
          #9#9#9'"FontName": "Segoe UI",'
          #9#9#9'"FontColor": 8421376,'
          #9#9#9'"FontSize": 8,'
          #9#9#9'"FontStyle": "",'
          #9#9#9'"WordWrap": false,'
          #9#9#9'"HorizontalAlignment": 0,'
          #9#9#9'"VerticalAlignment": 0'
          #9#9'}'
          #9'},'
          #9'"WatchSettings": {'
          #9#9'"ColumnHeadersVisible": false,'
          #9#9'"OnlyTrackChanges": true,'
          #9#9'"WatchHistoryVisible": true,'
          #9#9'"SyncWithSelection": true'
          #9'},'
          #9'"CallStackSettings": {'
          #9#9'"ColumnHeadersVisible": false'
          #9'},'
          #9'"LogLevelSettings": {'
          #9#9'"LogLevels": ['
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 15793151,'
          #9#9#9#9'"Level": 0'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 32768,'
          #9#9#9#9'"Level": 1'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 32896,'
          #9#9#9#9'"Level": 2'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 8454143,'
          #9#9#9#9'"Level": 3'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 8453888,'
          #9#9#9#9'"Level": 4'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 65535,'
          #9#9#9#9'"Level": 5'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 8388736,'
          #9#9#9#9'"Level": 8'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 7'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 8454143,'
          #9#9#9#9'"Level": 8'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 9'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 65535,'
          #9#9#9#9'"Level": 10'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 8421376,'
          #9#9#9#9'"Level": 11'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 12'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 65535,'
          #9#9#9#9'"Level": 13'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 65535,'
          #9#9#9#9'"Level": 14'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 15'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 16'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 17'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 18'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 19'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 20'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 21'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 22'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 23'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 24'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 25'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 26'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 27'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 28'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 29'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 30'
          #9#9#9'},'
          #9#9#9'{'
          #9#9#9#9'"Alias": "",'
          #9#9#9#9'"Color": 536870911,'
          #9#9#9#9'"Level": 31'
          #9#9#9'}'
          #9#9']'
          #9'}'
          '}')
        ParentFont = False
        TabOrder = 0
        WordWrap = False
      end
      object pnlObjectInspector: TPanel
        Left = 560
        Top = 0
        Width = 393
        Height = 901
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
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
      OnExecute = actExpandExecute
    end
    object actCollapse: TAction
      Caption = 'Collapse'
      ImageIndex = 1
      OnExecute = actCollapseExecute
    end
    object actParseDocument: TAction
      Caption = 'actParseDocument'
      ImageIndex = 2
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
  object imlMain: TImageList
    ColorDepth = cd32Bit
    Left = 320
    Top = 344
    Bitmap = {
      494C010103000800040010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000042
      5EBF0076AAFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000383838BF6868
      68FF6E6E6EFF747474FF7A7A7AFF7E7E7EFF7E7E7EFF7A7A7AFF747474FF6E6E
      6EFF686868FF383838BF00000000000000000000000000000000383838BF6868
      68FF6E6E6EFF747474FF7A7A7AFF7E7E7EFF7E7E7EFF7A7A7AFF747474FF6E6E
      6EFF686868FF383838BF0000000000000000000000000000000000000000001E
      2B800078ACFF0078ACFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000393939BF808080FF9898
      98FF989898FF989898FF989898FF989898FF989898FF989898FF989898FF9898
      98FF989898FF808080FF393939BF0000000000000000393939BF808080FF9898
      98FF989898FF989898FF989898FF989898FF989898FF989898FF989898FF9898
      98FF989898FF808080FF393939BF000000000000000000000000000000000000
      0000007BAFFF00B8EBFF007BAFFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000006A6A6AFF9B9B9BFF9B9B
      9BFF9B9B9BFF9B9B9BFF919191FF8D8D8DFF8D8D8DFF919191FF9B9B9BFF9B9B
      9BFF9B9B9BFF9B9B9BFF6A6A6AFF00000000000000006A6A6AFF9B9B9BFF9B9B
      9BFF9B9B9BFF9B9B9BFF9B9B9BFF9B9B9BFF9B9B9BFF9B9B9BFF9B9B9BFF9B9B
      9BFF9B9B9BFF9B9B9BFF6A6A6AFF000000000000000000000000000000000000
      000000202C8000A6D9FF00B9ECFF007EB2FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000006D6D6DFF9F9F9FFF9F9F
      9FFF9F9F9FFF9F9F9FFF909090FFFFEEDDFFFFEEDDFF909090FF9F9F9FFF9F9F
      9FFF9F9F9FFF9F9F9FFF6D6D6DFF00000000000000006D6D6DFF9F9F9FFF9F9F
      9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F
      9FFF9F9F9FFF9F9F9FFF6D6D6DFF000000000000000000000000000000000000
      0000000000000082B5FF00CCFFFF00B9ECFF0082B5FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000727272FFA4A4A4FFA4A4
      A4FFA4A4A4FFA4A4A4FF949494FFFFF0E0FFFFF0E0FF949494FFA4A4A4FFA4A4
      A4FFA4A4A4FFA4A4A4FF727272FF0000000000000000727272FFA4A4A4FFA4A4
      A4FFA4A4A4FFA4A4A4FFA4A4A4FFA4A4A4FFA4A4A4FFA4A4A4FFA4A4A4FFA4A4
      A4FFA4A4A4FFA4A4A4FF727272FF000000000000000000000000000000000000
      00000000000000212E8006ABDCFF0DCFFFFF09BDEDFF0086B9FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000767676FFA9A9A9FF9C9C
      9CFF989898FF989898FF989898FFFFF2E5FFFFF2E5FF989898FF989898FF9898
      98FF9C9C9CFFA9A9A9FF767676FF0000000000000000767676FFA9A9A9FF9C9C
      9CFF989898FF989898FF989898FF989898FF989898FF989898FF989898FF9898
      98FF9C9C9CFFA9A9A9FF767676FF0000000000000000004C6ABF008ABDFF008A
      BDFF008ABDFF008ABDFF089DCEFF21D4FFFF21D4FFFF18C1EEFF008ABDFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7BFFAFAFAFFF9C9C
      9CFFFFF5EBFFFFF5EBFFFFF5EBFFFFF5EBFFFFF5EBFFFFF5EBFFFFF5EBFFFFF5
      EBFF9C9C9CFFAFAFAFFF7B7B7BFF00000000000000007B7B7BFFAFAFAFFF9C9C
      9CFFFFEEDDFFFFEEDDFFFFEEDDFFFFEEDDFFFFEEDDFFFFEEDDFFFFEEDDFFFFEE
      DDFF9C9C9CFFAFAFAFFF7B7B7BFF0000000000000000002330801CB4E0FF38D9
      FFFF38D9FFFF38D9FFFF38D9FFFF38D9FFFF38D9FFFF38D9FFFF2AC6EFFF008E
      C1FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000818181FFB4B4B4FFA0A0
      A0FFFFF8F1FFFFF8F1FFFFF8F1FFFFF8F1FFFFF8F1FFFFF8F1FFFFF8F1FFFFF8
      F1FFA0A0A0FFB4B4B4FF818181FF0000000000000000818181FFB4B4B4FFA0A0
      A0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFA0A0A0FFB4B4B4FF818181FF0000000000000000000000000093C6FF52DF
      FFFF52DFFFFF52DFFFFF52DFFFFF52DFFFFF52DFFFFF52DFFFFF52DFFFFF3DCC
      F1FF0093C6FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000868686FFBABABAFFAAAA
      AAFFA5A5A5FFA5A5A5FFA5A5A5FFFFFBF7FFFFFBF7FFA5A5A5FFA5A5A5FFA5A5
      A5FFAAAAAAFFBABABAFF868686FF0000000000000000868686FFBABABAFFAAAA
      AAFFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFAAAAAAFFBABABAFF868686FF0000000000000000000000000026328037BE
      E5FF6EE5FFFF6EE5FFFF6EE5FFFFA6F2FFFFDDFFFFFFDDFFFFFFDDFFFFFFDDFF
      FFFFA6E5F2FF0097CAFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008A8A8AFFBFBFBFFFBFBF
      BFFFBFBFBFFFBFBFBFFFA8A8A8FFFFFDFCFFFFFDFCFFA8A8A8FFBFBFBFFFBFBF
      BFFFBFBFBFFFBFBFBFFF8A8A8AFF00000000000000008A8A8AFFBFBFBFFFBFBF
      BFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBF
      BFFFBFBFBFFFBFBFBFFF8A8A8AFF00000000000000000000000000000000009B
      CEFF8AECFFFF8AECFFFF8AECFFFF8AECFFFF66D8F3FF22AFDAFF009BCEFF009B
      CEFF009BCEFF009BCEFF005673BF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008F8F8FFFC4C4C4FFC4C4
      C4FFC4C4C4FFC4C4C4FFACACACFFFFFFFFFFFFFFFFFFACACACFFC4C4C4FFC4C4
      C4FFC4C4C4FFC4C4C4FF8F8F8FFF00000000000000008F8F8FFFC4C4C4FFC4C4
      C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFC4C4
      C4FFC4C4C4FFC4C4C4FF8F8F8FFF000000000000000000000000000000000028
      348051C9E9FFA4F2FFFFA4F2FFFFA4F2FFFFA4F2FFFF7ADDF4FF009FD2FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000929292FFC9C9C9FFC9C9
      C9FFC9C9C9FFC9C9C9FFB5B5B5FFAFAFAFFFAFAFAFFFB5B5B5FFC9C9C9FFC9C9
      C9FFC9C9C9FFC9C9C9FF929292FF0000000000000000929292FFC9C9C9FFC9C9
      C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9
      C9FFC9C9C9FFC9C9C9FF929292FF000000000000000000000000000000000000
      000000A2D5FFBBF7FFFFBBF7FFFFBBF7FFFFBBF7FFFFBBF7FFFF8CE2F4FF00A2
      D5FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000535353BFB1B1B1FFCCCC
      CCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCC
      CCFFCCCCCCFFB1B1B1FF535353BF0000000000000000535353BFB1B1B1FFCCCC
      CCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCCCCFFCCCC
      CCFFCCCCCCFFB1B1B1FF535353BF000000000000000000000000000000000000
      00000029368067D1ECFFCFFCFFFFCFFCFFFFCFFCFFFFCFFCFFFFCFFCFFFF9BE6
      F5FF00A5D8FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000555555BF9C9C
      9CFFA2A2A2FFA8A8A8FFAEAEAEFFB2B2B2FFB2B2B2FFAEAEAEFFA8A8A8FFA2A2
      A2FF9C9C9CFF555555BF00000000000000000000000000000000555555BF9C9C
      9CFFA2A2A2FFA8A8A8FFAEAEAEFFB2B2B2FFB2B2B2FFAEAEAEFFA8A8A8FFA2A2
      A2FF9C9C9CFF555555BF00000000000000000000000000000000000000000000
      00000000000000A8DBFFDDFFFFFFDDFFFFFFDDFFFFFFDDFFFFFFDDFFFFFFDDFF
      FFFFA6E9F6FF00A8DBFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000002A378016B3E0FF36BFE6FF57CCEBFF6ED5EEFF6ED5EEFF57CC
      EBFF36BFE6FF16B3E0FF005E7CBF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object ppmTree: TPopupMenu
    Left = 312
    Top = 472
    object mniCopy: TMenuItem
      Action = actCopy
    end
  end
end
