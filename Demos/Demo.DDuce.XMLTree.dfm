object frmXMLTree: TfrmXMLTree
  Left = 0
  Top = 0
  ClientHeight = 1035
  ClientWidth = 1719
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1719
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 1713
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
  end
  object pnlMain: TPanel
    Left = 0
    Top = 32
    Width = 1719
    Height = 1003
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlMain'
    TabOrder = 1
    ExplicitWidth = 1713
    ExplicitHeight = 986
    object splVertical: TSplitter
      Left = 473
      Top = 0
      Width = 6
      Height = 1003
      ResizeStyle = rsLine
      ExplicitLeft = 361
      ExplicitHeight = 516
    end
    object pnlTree: TPanel
      Left = 0
      Top = 0
      Width = 473
      Height = 1003
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 986
    end
    object pnlEditor: TPanel
      Left = 479
      Top = 0
      Width = 1240
      Height = 1003
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 1234
      ExplicitHeight = 986
      object mmoXml: TMemo
        Left = 0
        Top = 0
        Width = 745
        Height = 1003
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
          '<?xml version="1.0" encoding="UTF-8"?>'
          
            '<!-- Voorbeeld van een generiek XML-document met verschillende f' +
            'eatures -->'
          '<Library xmlns:meta="http://example.org/metadata"'
          '         version="2.1"'
          '         created="2025-04-05"'
          '         language="en">'
          ''
          '  <meta:Info>'
          '    <meta:Author id="1234" active="true">Tim</meta:Author>'
          '    <meta:License type="MIT"/>'
          '  </meta:Info>'
          ''
          '  <Collection name="TechBooks" category="Technology">'
          '    <Book id="B001" published="2023-05-12" available="true">'
          '      <Title>Artificial Intelligence Demystified</Title>'
          '      <Author>Jane Doe</Author>'
          '      <Pages>358</Pages>'
          '      <Tags>'
          '        <Tag>AI</Tag>'
          '        <Tag>Machine Learning</Tag>'
          '        <Tag>Neural Networks</Tag>'
          '      </Tags>'
          
            '      <Summary><![CDATA[This book offers a deep dive into modern' +
            ' AI techniques, including deep learning, reinforcement '
          'learning, and LLMs.'
          ']]></Summary>'
          '    </Book>'
          ''
          '    <Book id="B002" published="2022-11-20" available="false">'
          '      <Title>Embedded Systems with Python</Title>'
          '      <Author>John Smith</Author>'
          '      <Pages>290</Pages>'
          '      <Tags>'
          '        <Tag>Python</Tag>'
          '        <Tag>Hardware</Tag>'
          '        <Tag>IoT</Tag>'
          '      </Tags>'
          '      <Rating score="4.5" reviewers="128"/>'
          '    </Book>'
          '  </Collection>'
          ''
          '  <Settings>'
          
            '    <AutoSave enabled="true" interval="15"/> <!-- interval in mi' +
            'nutes -->'
          '    <Display theme="dark" fontSize="12" showLineNumbers="true"/>'
          '    <Logging level="debug" output="logs/app.log"/>'
          '  </Settings>'
          ''
          '</Library>')
        ParentFont = False
        TabOrder = 0
        OnChange = mmoXmlChange
        ExplicitWidth = 739
        ExplicitHeight = 986
      end
      object pnlObjectInspector: TPanel
        Left = 745
        Top = 0
        Width = 495
        Height = 1003
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitLeft = 739
        ExplicitHeight = 986
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
  end
end
