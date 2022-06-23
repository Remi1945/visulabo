unit testmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, Pointinteret,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  JaugeCir, FMX.Controls.Presentation, FMX.StdCtrls, JaugeRect, GraphicXYdeT,
  Histogramme, Radar;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Timer1: TTimer;
    Button2: TButton;
    Radar1: TRadar;
    GraphicXYdeT1: TGraphicXYdeT;
    vue: TImage;
    // procedure Button1Click(Sender: TObject);
    // procedure Timer1Timer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
    histo1, histo2: array [0 .. 511] of integer;
    un: boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{
  procedure TForm1.Button1Click(Sender: TObject);
  var
  I: integer;
  begin
  for I := 0 to 511 do
  begin
  histo1[I] := 0;
  histo2[I] := 0;
  end;
  histo1[125] := 100;
  histo2[125] := 2000;
  grHisto.MajHisto(0, 511, histo1, 512, true);
  Timer1.Enabled := true;
  end;
}
procedure TForm1.Button1Click(Sender: TObject);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create(200, 200);
  bmp.Canvas.BeginScene;
  bmp.Canvas.Fill.Color:=$FFFF0000;
  bmp.Canvas.FillArc(TPointF.Create(100, 100), TPointF.Create(50, 50),
    60, 25, 1);
  bmp.Canvas.EndScene;
  vue.Bitmap.Assign(bmp);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  myPI: TPi;
begin
  myPI := TPi.Create('0deg', 30 / 180 * PI, 0.25, TForme.FrmCercle,
    $FFFF0000, 4);
  Radar1.AjoutePI(myPI, true);
end;

{
  procedure TForm1.Timer1Timer(Sender: TObject);
  begin
  if un then
  grHisto.MajHisto(0, 511, histo1, 512, true)
  else
  grHisto.MajHisto(0, 511, histo2, 512, true);
  un := not(un);

  end;
}
end.
