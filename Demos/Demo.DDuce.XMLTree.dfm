object frmXMLTree: TfrmXMLTree
  Left = 0
  Top = 0
  ClientHeight = 548
  ClientWidth = 1097
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
    Width = 1097
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
  end
  object pnlMain: TPanel
    Left = 0
    Top = 32
    Width = 1097
    Height = 516
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlMain'
    TabOrder = 1
    object splVertical: TSplitter
      Left = 361
      Top = 0
      Width = 6
      Height = 516
      ResizeStyle = rsLine
    end
    object pnlXMLTree: TPanel
      Left = 0
      Top = 0
      Width = 361
      Height = 516
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
    end
    object pnlEditor: TPanel
      Left = 367
      Top = 0
      Width = 730
      Height = 516
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object mmoXML: TMemo
        Left = 0
        Top = 0
        Width = 432
        Height = 516
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
          
            '<?xml version="1.0" encoding="UTF-8"?>'#10'<library>'#10'    <book id="1' +
            '01">'#10'       '
          ' <title>The Great Gatsby</title>'#10'        <author>F. Scott '
          'Fitzgerald</author>'#10'        <genre>Fiction</genre>'#10'        '
          '<publishedYear>1925</publishedYear>'#10'        '
          
            '<publisher>Scribner</publisher>'#10'        <isbn>9780743273565</isb' +
            'n>'#10'    '
          
            '</book>'#10'    <book id="102">'#10'        <title>To Kill a Mockingbird' +
            '</title>'#10'   '
          
            '     <author>Harper Lee</author>'#10'        <genre>Fiction</genre>'#10 +
            '        '
          
            '<publishedYear>1960</publishedYear>'#10'        <publisher>J.B. Lipp' +
            'incott & '
          
            'Co.</publisher>'#10'        <isbn>9780061120084</isbn>'#10'    </book>'#10' ' +
            '   <book '
          'id="103">'#10'        <title>1984</title>'#10'        <author>George '
          'Orwell</author>'#10'        <genre>Dystopian</genre>'#10'        '
          '<publishedYear>1949</publishedYear>'#10'        <publisher>Secker & '
          
            'Warburg</publisher>'#10'        <isbn>9780451524935</isbn>'#10'    </boo' +
            'k>'
          '</library>')
        ParentFont = False
        TabOrder = 0
        OnChange = mmoXMLChange
      end
      object pnlInspector: TPanel
        Left = 432
        Top = 0
        Width = 298
        Height = 516
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
  end
end
