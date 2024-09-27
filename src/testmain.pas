unit testmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, Pointinteret,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  JaugeCir, FMX.Controls.Presentation, FMX.StdCtrls, JaugeRect, GraphicXYdeT,
  Histogramme, Radar, FMX.Menus, FMX.Edit, EditDG;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Timer1: TTimer;
    Button2: TButton;

    MainMenu1: TMainMenu;
    // procedure Button1Click(Sender: TObject);
    // procedure Timer1Timer(Sender: TObject);
    procedure Button2Click(Sender: TObject);

    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
    histo1, histo2: array [0 .. 511] of integer;
    un: boolean;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  i: integer;
begin

  myPI := TPi.Create('0deg', 30 / 180 * PI, 0.25, TForme.FrmCercle,
    $FFFF0000, 4);


end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin

end;

end.
