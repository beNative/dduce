unit DDuce.Demos.Reflect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmReflect = class(TForm)
    mmoMain: TMemo;
    procedure FormShow(Sender: TObject);

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Reflect;

procedure TfrmReflect.AfterConstruction;
var
  S: string;
begin
  inherited;

  //S :=Reflect.Properties(Self).ToString;
    S := Reflect.Properties(Self).Values['Caption'].AsString;
//  S := Reflect.Properties(Self).Data.Caption;
  mmoMain.Text := S;
end;

procedure TfrmReflect.FormShow(Sender: TObject);
begin
  Reflect.Properties(Self).Data.Caption := 'Reflected' ;
  Reflect.Properties(Self).Data.Tile;
end;

end.
