unit testmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,  Pointinteret,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  JaugeCir, FMX.Controls.Presentation, FMX.StdCtrls, JaugeRect, GraphicXYdeT;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Timer1: TTimer;
    GraphicXYdeT1: TGraphicXYdeT;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
//Timer1.Enabled:=not(Timer1.Enabled);
GraphicXYdeT1.AjoutePI(TPI.Create('Test',0.2,0.4,TForme.FrmX,$FFFF0000,16),true);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
GraphicXYdeT1.Surlignage:=0;
end;

end.
