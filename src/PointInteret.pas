unit PointInteret;

interface

uses System.UITypes, System.UIConsts;

type
  TForme = (FrmCarre, FrmCercle, FrmX, FrmCroix);

  TPi = class(TObject)
  private
    ltxt: string;
    lx, ly: double;
    lForme: TForme;
    lcouleur: TAlphaColor;
    lTaille: Single;
  public
    constructor Create(cmt: string; mx, my: double; frm: TForme;
      coul: TAlphaColor; tt: Single);
    property x: double read lx write lx;
    property y: double read ly write ly;
    property maForme: TForme read lForme write lForme;
    property taille: Single read lTaille write lTaille;
    property Couleur: TAlphaColor read lcouleur write lcouleur;
    property montexte: string read ltxt write ltxt;
  end;

implementation

{ TPi }

constructor TPi.Create(cmt: string; mx, my: double; frm: TForme;
  coul: TAlphaColor; tt: Single);
begin
  ltxt := cmt;
  lx := mx;
  ly := my;
  lForme := frm;
  lcouleur := coul;
  lTaille := tt;
end;

end.
