unit testmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, Pointinteret,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  JaugeCir, FMX.Controls.Presentation, FMX.StdCtrls, JaugeRect, GraphicXYdeT,
  Histogramme;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Timer1: TTimer;
    Button2: TButton;
    grhisto: THistogramme;
    procedure Button1Click(Sender: TObject);
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
  Timer1.Enabled := true;
  for i := 0 to 511 do
  begin
    histo1[i] := 0;
    histo2[i] := 0;
  end;
  histo1[127] := 100;
  histo2[127] := 10000;
  Timer1.Enabled := true;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  un := not(un);
  if un then
    grhisto.MajValeurs(histo1, 512, true)
  else
    grhisto.MajValeurs(histo2, 512, true);
end;

end.
