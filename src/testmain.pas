unit testmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, Pointinteret,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  JaugeCir, FMX.Controls.Presentation, FMX.StdCtrls, JaugeRect, GraphicXYdeT,
  Histogramme, Radar, FMX.Menus, FMX.Edit, EditDG, Afficheur;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Timer1: TTimer;
    Button2: TButton;

    MainMenu1: TMainMenu;
    Afficheur1: TAfficheur;

    procedure Button1Click(Sender: TObject);
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
  lst: TList;
  bmp: TBitmap;
begin
  lst := TList.Create;
  for i := 1 to 5 do
  begin
    bmp := TBitmap.CreateFromFile
      (Format('E:\perso\pdb\Graph\Terrains\France\Z3\TER%.4d.png', [i * 10]));
    lst.Add(bmp);
  end;
  Afficheur1.setListe(lst);
end;

end.
