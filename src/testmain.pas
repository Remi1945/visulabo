unit testmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  JaugeCir, FMX.Controls.Presentation, FMX.StdCtrls, JaugeRect;

type
  TForm1 = class(TForm)
    JaugeRect1: TJaugeRect;
    Button1: TButton;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
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
Timer1.Enabled:=not(Timer1.Enabled);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  JaugeRect1.Valeur := JaugeRect1.Valeur + 0.01;
end;

end.
