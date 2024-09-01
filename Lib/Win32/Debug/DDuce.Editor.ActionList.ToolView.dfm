object frmActionListView: TfrmActionListView
  Left = 506
  Top = 90
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  ActiveControl = edtFilterActions
  Caption = 'Actions'
  ClientHeight = 789
  ClientWidth = 1209
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 144
  TextHeight = 23
  object pnlEditorList: TPanel
    Left = 0
    Top = 0
    Width = 1209
    Height = 789
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pgcMain: TPageControl
      Left = 0
      Top = 0
      Width = 1209
      Height = 789
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ActivePage = tsActions
      Align = alClient
      TabOrder = 0
      object tsActions: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Actions'
        object pnlActions: TPanel
          Left = 0
          Top = 32
          Width = 1188
          Height = 715
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
        end
        object edtFilterActions: TEdit
          Left = 0
          Top = 0
          Width = 1188
          Height = 31
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alTop
          TabOrder = 1
          OnChange = edtFilterActionsChange
          OnKeyDown = edtFilterActionsKeyDown
          OnKeyUp = edtFilterActionsKeyUp
        end
      end
      object tsCommands: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Editor key commands'
      end
    end
  end
end
