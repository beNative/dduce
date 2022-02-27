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
  OldCreateOrder = False
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
    object btnExpand: TButton
      Left = 8
      Top = 4
      Width = 150
      Height = 25
      Action = actExpand
      TabOrder = 0
    end
    object btnCollapse: TButton
      Left = 164
      Top = 4
      Width = 150
      Height = 25
      Action = actCollapse
      TabOrder = 1
    end
    object btnParseDocument: TButton
      Left = 320
      Top = 4
      Width = 150
      Height = 25
      Action = actParseDocument
      Caption = 'Parse document'
      TabOrder = 2
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
    Left = 320
    Top = 256
    object actExpand: TAction
      Caption = 'Expand'
      OnExecute = actExpandExecute
    end
    object actCollapse: TAction
      Caption = 'Collapse'
      OnExecute = actCollapseExecute
    end
    object actParseDocument: TAction
      Caption = 'actParseDocument'
      OnExecute = actParseDocumentExecute
    end
  end
end
