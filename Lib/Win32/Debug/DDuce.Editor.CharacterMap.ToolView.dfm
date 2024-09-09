inherited frmCharacterMap: TfrmCharacterMap
  Left = 38
  Top = 38
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  ActiveControl = pcMain
  BorderStyle = bsSingle
  ClientHeight = 649
  ClientWidth = 375
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  PopupMode = pmAuto
  Position = poDefault
  TextHeight = 23
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 375
    Height = 404
    ActivePage = tsANSI
    Align = alClient
    TabOrder = 0
    object tsANSI: TTabSheet
      Caption = 'ANSI'
      DesignSize = (
        367
        366)
      object lblCharInfo: TLabel
        Left = 6
        Top = 350
        Width = 84
        Height = 23
        Margins.Bottom = 2
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'lblCharInfo'
        Color = clBtnFace
        ParentColor = False
        ExplicitTop = 337
      end
      object grdANSI: TStringGrid
        Left = 0
        Top = 0
        Width = 357
        Height = 339
        Anchors = [akLeft, akTop, akRight, akBottom]
        BorderStyle = bsNone
        ColCount = 17
        DefaultColWidth = 25
        RowCount = 15
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnKeyUp = grdANSIKeyUp
        OnMouseDown = grdANSIMouseDown
        OnMouseMove = grdANSIMouseMove
        OnSelectCell = grdANSISelectCell
        ExplicitWidth = 360
        ExplicitHeight = 343
      end
    end
    object tsUnicode: TTabSheet
      Caption = 'Unicode'
      DesignSize = (
        367
        366)
      object lblUnicodeCharInfo: TLabel
        Left = 6
        Top = 342
        Width = 147
        Height = 23
        Margins.Bottom = 2
        Caption = 'lblUnicodeCharInfo'
        Color = clBtnFace
        ParentColor = False
      end
      object grdUnicode: TStringGrid
        Left = 0
        Top = 0
        Width = 357
        Height = 332
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColCount = 16
        DefaultColWidth = 25
        FixedCols = 0
        RowCount = 15
        FixedRows = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnKeyUp = grdUnicodeKeyUp
        OnMouseDown = grdUnicodeMouseDown
        OnMouseMove = grdUnicodeMouseMove
        OnSelectCell = grdUnicodeSelectCell
        ExplicitWidth = 360
        ExplicitHeight = 336
      end
      object cbxUnicodeRange: TComboBox
        Left = 162
        Top = 338
        Width = 189
        Height = 21
        Style = csDropDownList
        Anchors = [akRight]
        DropDownCount = 30
        TabOrder = 1
        OnSelect = cbxUnicodeRangeSelect
      end
    end
  end
  object pnlChar: TPanel
    Left = 0
    Top = 404
    Width = 375
    Height = 245
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 1
    object shpChar: TShape
      Left = 0
      Top = 0
      Width = 375
      Height = 245
      Align = alClient
      Brush.Style = bsClear
      Pen.Color = clSilver
      Pen.Width = 2
    end
    object imgChar: TImage
      Left = 0
      Top = 0
      Width = 375
      Height = 245
      Align = alClient
      AutoSize = True
      Center = True
      Transparent = True
    end
  end
end
