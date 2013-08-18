unit DDuce.Demos.PropertyInspector;

{$I ..\Source\DDuce.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, ButtonGroup,

  DDuce.Components.PropertyInspector;

type
  TfrmPropertyInspector = class(TForm)
    {$REGION 'designer controls'}
    pnlMain       : TPanel;
    pnlLeft       : TPanel;
    pnlRight      : TPanel;
    btnButton     : TButton;
    chkCheckBox   : TCheckBox;
    edtEdit       : TEdit;
    bgButtonGroup : TButtonGroup;
    cbxControls   : TComboBox;
    sbrStatusBar  : TStatusBar;
    trbTrackBar   : TTrackBar;
    splSplitter   : TSplitter;
    lblLabel      : TLabel;
    {$ENDREGION}

    procedure cbxControlsChange(Sender: TObject);

  private
    FPropertyInspector: TPropertyInspector;

  public
    procedure AfterConstruction; override;

  end;

implementation

uses
  DDuce.Demos.Helpers;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmPropertyInspector.AfterConstruction;
var
  I: Integer;
  C: TWinControl;
begin
  inherited;
  FPropertyInspector := CreateInspector(Self, pnlLeft, bgButtonGroup);
  FPropertyInspector.Name := 'PropertyInspector';
  for I := 0 to ComponentCount - 1 do
  begin
    if Components[I] is TWinControl then
    begin
      C := TWinControl(Components[I]);
      cbxControls.AddItem(C.Name, C);
    end;
  end;
  cbxControls.ItemIndex := 0;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmPropertyInspector.cbxControlsChange(Sender: TObject);
var
  C: TWinControl;
begin
  C := cbxControls.Items.Objects[cbxControls.ItemIndex] as TWinControl;
  FPropertyInspector.Objects[0] := C;
end;
{$ENDREGION}

end.
