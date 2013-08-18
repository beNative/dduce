unit DDuce.Demos.Reflect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmReflect = class(TForm)
    mmoMain      : TMemo;
    lblReflected : TLabel;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Reflect;

procedure TfrmReflect.AfterConstruction;
begin
  inherited;
  mmoMain.Text := Reflect.Properties(Self).ToString;
end;

end.
