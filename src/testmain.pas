unit testmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  JaugeCir, FMX.Controls.Presentation, FMX.StdCtrls, JaugeRect, DoubleCurseur, Histogramme;

type

  TForm1 = class(TForm)

    Label1: TLabel;
    Histogramme1: THistogramme;
    dblcurs: TDoubleCurseur;
    Rectangle1: TRectangle;

    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure majLabel(Sender:TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
dblcurs.setProcUtilisateur(majLabel,false,true);
end;

procedure TForm1.majLabel(Sender: TObject);
begin
 label1.Text:=Format('%d %d',[TDoubleCurseur(Sender).Valeur1,TDoubleCurseur(Sender).Valeur2]);
 Histogramme1.MajHisto(TDoubleCurseur(Sender).Valeur1,TDoubleCurseur(Sender).Valeur2,true);
end;

end.
