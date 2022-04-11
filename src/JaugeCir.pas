unit JaugeCir;

interface

uses
  System.SysUtils, System.Classes, System.Types, FMX.Types, FMX.Controls,
  FMX.Objects, FMX.Graphics, System.UITypes, Couleurs,
  System.UIConsts, System.Math.Vectors;

type
  TGenre = (DemiCercle, Cercle, QuartDeCercleHautDroit, QuartDeCercleHautGauche, QuartDeCercleBasDroit,
    QuartDeCercleBasGauche);
  TCadran = (Blanc, DegradeNoirClair, DegradeNoirFonce, Custom);

  TJaugeCir = class(TRectangle)
  private
    FFormatValeurs: String;
    FMaxi, FMini, FValeur, FSeuil_A, FSeuil_B, FGraduationMajeure, FGraduationMineure: double;
    FMontreGraduationMajeure, FMontreGraduationMineure, FMontreValeurs, FMontreSeuils: boolean;
    FEpaisseurBordure, FEpaisseurSeuil, FLgGrdMaj, FLgGrdMin, FGenreSeuil: integer;
    FGenre: TGenre;
    FCadran: TCadran;
    FAspectBordure: TMatiere;
    procedure SetFormatValeurs(Value: String);
    procedure SetGenreSeuil(Value: integer);
    procedure SetEpaisseurBordure(Value: integer);
    procedure SetEpaisseurSeuil(Value: integer);
    procedure SetLgGrdMaj(Value: integer);
    procedure SetLgGrdMin(Value: integer);
    procedure SetMaxi(Value: double);
    procedure SetMini(Value: double);
    procedure SetValeur(Value: double);
    procedure SetSeuil_A(Value: double);
    procedure SetSeuil_B(Value: double);
    procedure SetGraduationMajeure(Value: double);
    procedure SetGraduationMineure(Value: double);
    procedure SetMontreGraduationMajeure(Value: boolean);
    procedure SetMontreGraduationMineure(Value: boolean);
    procedure SetMontreValeurs(Value: boolean);
    procedure SetMontreSeuils(Value: boolean);
    function dessineBordure(mat: TMatiere; lrg, htr: integer; gnr: TGenre; Xc, Yc, Rx, Ry: Single): TBitmap;
  protected
    { Déclarations protégées }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    function HorsLimites: integer;
  published
    property FormatValeurs: String read FFormatValeurs write SetFormatValeurs;
    property Genre: TGenre read FGenre write FGenre;
    property Cadran: TCadran read FCadran write FCadran;
    property GenreSeuil: integer read FGenreSeuil write SetGenreSeuil;
    property EpaisseurBordure: integer read FEpaisseurBordure write SetEpaisseurBordure;
    property AspectBordure: TMatiere read FAspectBordure write FAspectBordure;
    property EpaisseurSeuil: integer read FEpaisseurSeuil write SetEpaisseurSeuil;
    property LongueurGraduationMajeure: integer read FLgGrdMaj write SetLgGrdMaj;
    property LongueurGraduationMineure: integer read FLgGrdMin write SetLgGrdMin;
    property Maxi: double read FMaxi write SetMaxi;
    property Mini: double read FMini write SetMini;
    property Valeur: double read FValeur write SetValeur;
    property Seuil_A: double read FSeuil_A write SetSeuil_A;
    property Seuil_B: double read FSeuil_B write SetSeuil_B;
    property GraduationMajeure: double read FGraduationMajeure write SetGraduationMajeure;
    property GraduationMineure: double read FGraduationMineure write SetGraduationMineure;
    property MontreGraduationMineure: boolean read FMontreGraduationMineure write SetMontreGraduationMineure;
    property MontreGraduationMajeure: boolean read FMontreGraduationMajeure write SetMontreGraduationMajeure;
    property MontreValeurs: boolean read FMontreValeurs write SetMontreValeurs;
    property MontreSeuils: boolean read FMontreSeuils write SetMontreSeuils;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TJaugeCir]);
end;

procedure TJaugeCir.Paint;
const
  marge: Single = 4;
  Ns: integer = 200;
var

  rect, RectEcr: TRectF;
  decr: double;
  stMax, stMin, st: String;
  Wtxt, Htxt: Single;
  Xc, Yc, Rx, Ry, RxEcr, RyEcr: Single;
  alpha0, alpha1, alpha: Single;
  br: TBrush;
  pen: TStrokeBrush;
  aiguille, Cadran, cadranS: TPolygon;
  i, j: integer;
  angle, dalpha, grad: double;
  x, y, x1, x2, y1, y2: Single;
  indxA, indxB, i0, i1, L: integer;
  dalphaMaj, dalphaMin: Single;
  nbgradMaj, nbgradMin: integer;
  xecr, yecr: Single;
  angleMaj, angleMin: double;
  Bordure: TBitmap;
begin

  Canvas.BeginScene;
  // Fond transparent
  br := TBrush.Create(TBrushKind.Solid, 0);
  rect := TRectF.Create(0, 0, Width, Height);
  Canvas.FillRect(rect, 0, 0, AllCorners, 100, br);
  br.Free;

  // Calcul de la hauteur et de la largeur max des étiquettes
  stMax := Format(FFormatValeurs, [Maxi]);
  stMin := Format(FFormatValeurs, [Mini]);
  Htxt := Canvas.TextHeight(stMax);
  if Canvas.TextHeight(stMin) > Htxt then
    Htxt := Canvas.TextHeight(stMin);
  Wtxt := Canvas.TextWidth(stMax);
  if Canvas.TextWidth(stMin) > Wtxt then
    Wtxt := Canvas.TextWidth(stMin);

  // Calcul des valeurs centre rayon amplitude angulaire pour le genre par défaut
  // et dessin du fond du cadran
  case FGenre of
    DemiCercle:
      begin
        Xc := Width / 2;
        Yc := Height - marge - Htxt / 2 - EpaisseurBordure;
        Rx := Width / 2 - marge - EpaisseurBordure;
        Ry := Height - marge - Htxt / 2 - EpaisseurBordure;
        if Ry > Rx then
          Ry := Rx;
        alpha0 := Pi;
        alpha1 := 0;
        setLength(Cadran, 4 + Ns);
        Cadran[0] := TPointF.Create(Xc - Rx, Height - EpaisseurBordure);
        dalpha := (alpha1 - alpha0) / Ns;
        for i := 0 to Ns do
        begin
          angle := alpha0 + dalpha * i;
          x := Xc + cos(angle) * Rx;
          y := Yc - sin(angle) * Ry;
          Cadran[i + 1] := TPointF.Create(x, y);
        end;
        Cadran[Ns + 2] := TPointF.Create(Xc + Rx, Height - EpaisseurBordure);
        Cadran[Ns + 3] := Cadran[0];
      end;
    Cercle:
      begin
        Xc := Width / 2;
        Yc := Height / 2;
        Rx := Width / 2 - marge - EpaisseurBordure;
        Ry := Height / 2 - marge - EpaisseurBordure;
        alpha0 := 4 * Pi / 3;
        alpha1 := -Pi / 3;
        setLength(Cadran, 2 + Ns);
        dalpha := (alpha1 - alpha0) / Ns;
        for i := 0 to Ns do
        begin
          angle := alpha0 + dalpha * i;
          x := Xc + cos(angle) * Rx;
          y := Yc - sin(angle) * Ry;
          Cadran[i] := TPointF.Create(x, y);
        end;
        Cadran[Ns + 1] := Cadran[0];
      end;
    QuartDeCercleHautDroit:
      begin
        Xc := Wtxt + EpaisseurBordure;
        Yc := Height - Htxt - EpaisseurBordure;
        Rx := Width - marge - Wtxt - 2 * EpaisseurBordure;
        Ry := Height - marge - Htxt - 2 * EpaisseurBordure;
        alpha0 := 0;
        alpha1 := Pi / 2;
        setLength(Cadran, 5 + Ns);
        dalpha := (alpha1 - alpha0) / Ns;
        for i := 0 to Ns do
        begin
          angle := alpha0 + dalpha * i;
          x := Xc + cos(angle) * Rx;
          y := Yc - sin(angle) * Ry;
          Cadran[i] := TPointF.Create(x, y);
        end;
        Cadran[Ns + 1] := TPointF.Create(EpaisseurBordure, Cadran[Ns].y);
        Cadran[Ns + 2] := TPointF.Create(EpaisseurBordure, Height - EpaisseurBordure);
        Cadran[Ns + 3] := TPointF.Create(Xc + Rx, Height - EpaisseurBordure);
        Cadran[Ns + 4] := Cadran[0];
      end;
    QuartDeCercleHautGauche:
      begin
        Xc := Width - Wtxt - EpaisseurBordure;
        Yc := Height - Htxt - EpaisseurBordure;
        Rx := Width - marge - Wtxt - 2 * EpaisseurBordure;
        Ry := Height - marge - Htxt - 2 * EpaisseurBordure;
        alpha0 := Pi;
        alpha1 := Pi / 2;
        setLength(Cadran, 5 + Ns);
        dalpha := (alpha1 - alpha0) / Ns;
        for i := 0 to Ns do
        begin
          angle := alpha0 + dalpha * i;
          x := Xc + cos(angle) * Rx;
          y := Yc - sin(angle) * Ry;
          Cadran[i] := TPointF.Create(x, y);
        end;
        Cadran[Ns + 1] := TPointF.Create(Width - EpaisseurBordure, Cadran[Ns].y);
        Cadran[Ns + 2] := TPointF.Create(Width - EpaisseurBordure, Height - EpaisseurBordure);
        Cadran[Ns + 3] := TPointF.Create(Cadran[0].x, Height - EpaisseurBordure);
        Cadran[Ns + 4] := Cadran[0];
      end;
    QuartDeCercleBasGauche:
      begin
        Xc := Width - Wtxt - EpaisseurBordure;
        Yc := Htxt + EpaisseurBordure;
        Rx := Width - marge - Wtxt - 2 * EpaisseurBordure;
        Ry := Height - marge - Htxt - 2 * EpaisseurBordure;
        alpha0 := 3 * Pi / 2;
        alpha1 := Pi;
        setLength(Cadran, 5 + Ns);
        dalpha := (alpha1 - alpha0) / Ns;
        for i := 0 to Ns do
        begin
          angle := alpha0 + dalpha * i;
          x := Xc + cos(angle) * Rx;
          y := Yc - sin(angle) * Ry;
          Cadran[i] := TPointF.Create(x, y);
        end;
        Cadran[Ns + 1] := TPointF.Create(Cadran[Ns].x, EpaisseurBordure);
        Cadran[Ns + 2] := TPointF.Create(Width - EpaisseurBordure, EpaisseurBordure);
        Cadran[Ns + 3] := TPointF.Create(Width - EpaisseurBordure, Cadran[0].y);
        Cadran[Ns + 4] := Cadran[0];
      end;
    QuartDeCercleBasDroit:
      begin
        Xc := Wtxt + EpaisseurBordure;
        Yc := Htxt + EpaisseurBordure;
        Rx := Width - marge - Wtxt - 2 * EpaisseurBordure;
        Ry := Height - marge - Htxt - 2 * EpaisseurBordure;
        alpha0 := -Pi / 2;
        alpha1 := 0;
        setLength(Cadran, 5 + Ns);
        dalpha := (alpha1 - alpha0) / Ns;
        for i := 0 to Ns do
        begin
          angle := alpha0 + dalpha * i;
          x := Xc + cos(angle) * Rx;
          y := Yc - sin(angle) * Ry;
          Cadran[i] := TPointF.Create(x, y);
        end;
        Cadran[Ns + 1] := TPointF.Create(Cadran[Ns].x, EpaisseurBordure);
        Cadran[Ns + 2] := TPointF.Create(EpaisseurBordure, EpaisseurBordure);
        Cadran[Ns + 3] := TPointF.Create(EpaisseurBordure, Cadran[0].y);
        Cadran[Ns + 4] := Cadran[0];
      end;
  end;
  // Dessin de la bordure
  if (FEpaisseurBordure > 0) and (FAspectBordure <> MAT_SANS) then
  begin
    Bordure := dessineBordure(FAspectBordure,round(Width), round(Height), FGenre, Xc, Yc, Rx, Ry);
    Canvas.DrawBitmap(Bordure, TRectF.Create(0, 0, Width, Height), TRectF.Create(0, 0, Width, Height), 1, true);
    Bordure.Free;
  end;

  // Dession du cadran
  decr := sqrt(Wtxt * Wtxt + Htxt * Htxt) / 2;
  RxEcr := Rx - FLgGrdMaj - decr;
  RyEcr := Ry - FLgGrdMaj - decr;
  case FCadran of
    Blanc:
      begin
        Canvas.Fill.Color := claWhite;
        Canvas.Fill.Kind := TBrushKind.Solid;
      end;
    DegradeNoirClair:
      begin
        Canvas.Fill.Gradient.Color1 := claWhite;
        Canvas.Fill.Gradient.Color := claBlack;
        Canvas.Fill.Gradient.Style := TGradientStyle.Linear;
        Canvas.Fill.Kind := TBrushKind.Gradient;
      end;
    DegradeNoirFonce:
      begin
        Canvas.Fill.Gradient.Color1 := claGray;
        Canvas.Fill.Gradient.Color := claBlack;
        Canvas.Fill.Gradient.Style := TGradientStyle.Linear;
        Canvas.Fill.Kind := TBrushKind.Gradient;
      end;
    Custom:
      begin
        Canvas.Fill := Fill;
      end;
  end;

  Canvas.FillPolygon(Cadran, 1);

  // Calcul du polygone de contour du cadran
  setLength(Cadran, 3 + Ns * 2);
  dalpha := (alpha1 - alpha0) / Ns;
  for i := 0 to Ns do
  begin
    angle := alpha0 + dalpha * i;
    x1 := Xc + cos(angle) * Rx;
    y1 := Yc - sin(angle) * Ry;
    Cadran[i] := TPointF.Create(x1, y1);
    x2 := Xc + cos(angle) * (Rx - EpaisseurSeuil);
    y2 := Yc - sin(angle) * (Ry - EpaisseurSeuil);
    Cadran[Ns * 2 + 1 - i] := TPointF.Create(x2, y2);
  end;
  Cadran[Ns * 2 + 2] := Cadran[0];
  // Colorisation des seuils
  if MontreSeuils then
  begin
    indxA := -1;
    indxB := -1;

    if (Seuil_A > Mini) and (Seuil_A < Maxi) and (Seuil_B > Mini) and (Seuil_B < Maxi) and (Seuil_A < Seuil_B) then
    begin
      // Coloration de mini à seuil A
      indxA := round((Seuil_A - Mini) / (Maxi - Mini) * Ns);
      L := indxA;
      i0 := 0;
      i1 := Ns * 2 + 1;
      setLength(cadranS, L * 2 + 3);
      for i := 0 to L do
      begin
        cadranS[i] := Cadran[i0 + i];
        cadranS[L * 2 + 1 - i] := Cadran[i1 - i];
      end;
      cadranS[indxA * 2 + 2] := Cadran[i0];
      br := TBrush.Create(TBrushKind.Solid, claGreen);
      if GenreSeuil = 2 then
        br.Color := claRed;
      Canvas.Fill := br;
      Canvas.FillPolygon(cadranS, 1);
      br.Free;
      // Coloration de SeuilA à seuil B
      indxB := round((Seuil_B - Mini) / (Maxi - Mini) * Ns);
      L := indxB - indxA;
      i0 := indxA;
      i1 := Ns * 2 + 1 - indxA;
      setLength(cadranS, L * 2 + 3);
      for i := 0 to L do
      begin
        cadranS[i] := Cadran[i0 + i];
        cadranS[L * 2 + 1 - i] := Cadran[i1 - i];
      end;
      cadranS[L * 2 + 2] := Cadran[i0];
      br := TBrush.Create(TBrushKind.Solid, claYellow);
      if GenreSeuil = 2 then
        br.Color := claGreen;
      Canvas.Fill := br;
      Canvas.FillPolygon(cadranS, 1);
      br.Free;
      // Coloration de SeuilB à maxi
      L := Ns - indxB;
      i0 := indxB;
      i1 := Ns * 2 + 1 - indxB;
      setLength(cadranS, L * 2 + 3);
      for i := 0 to L do
      begin
        cadranS[i] := Cadran[i0 + i];
        cadranS[L * 2 + 1 - i] := Cadran[i1 - i];
      end;
      cadranS[L * 2 + 2] := Cadran[i0];
      br := TBrush.Create(TBrushKind.Solid, claRed);
      Canvas.Fill := br;
      Canvas.FillPolygon(cadranS, 1);
      br.Free;

    end;
  end;
  // Dessin du contour du cadran
  Canvas.Stroke.Color := Stroke.Color;
  Canvas.DrawPolygon(Cadran, 1);
  // Dessin des chiffres de graduation
  dalphaMaj := alpha1 - alpha0;
  dalphaMin := alpha1 - alpha0;
  nbgradMaj := 2;
  nbgradMin := 0;
  if GraduationMajeure > 0 then
  begin
    nbgradMaj := round((Maxi - Mini) / GraduationMajeure) + 1;
    dalphaMaj := (alpha1 - alpha0) / (nbgradMaj - 1);
    if (GraduationMineure > 0) and (GraduationMajeure > GraduationMineure) then
    begin
      nbgradMin := round(GraduationMajeure / GraduationMineure);
      dalphaMin := dalphaMaj / nbgradMin;
    end;
  end;
  if MontreValeurs then
  begin
    Canvas.Fill.Color := Stroke.Color;
    for i := 0 to nbgradMaj - 1 do
    begin
      angleMaj := alpha0 + i * dalphaMaj;
      xecr := RxEcr * cos(angleMaj) + Xc;
      yecr := RyEcr * -sin(angleMaj) + Yc;
      RectEcr.left := xecr - Wtxt / 2;
      RectEcr.top := yecr - Htxt / 2;
      RectEcr.right := xecr + Wtxt / 2;
      RectEcr.bottom := yecr + Htxt / 2;
      grad := Mini + i * GraduationMajeure;
      st := Format(FFormatValeurs, [grad]);
      Canvas.FillText(RectEcr, st, false, 1, [], TTextAlign.Center, TTextAlign.Center);
      if MontreGraduationMajeure then
      begin
        x1 := Rx * cos(angleMaj) + Xc;
        y1 := Ry * -sin(angleMaj) + Yc;
        x2 := (Rx - LongueurGraduationMajeure) * cos(angleMaj) + Xc;
        y2 := (Ry - LongueurGraduationMajeure) * -sin(angleMaj) + Yc;
        Canvas.DrawLine(TPointF.Create(x1, y1), TPointF.Create(x2, y2), 1);
        if (MontreGraduationMineure) and (GraduationMineure > 0) and (i <> nbgradMaj - 1) then
        begin
          for j := 1 to nbgradMin - 1 do
          begin
            angle := angleMaj + dalphaMin * j;
            x1 := Rx * cos(angle) + Xc;
            y1 := Ry * -sin(angle) + Yc;
            x2 := (Rx - LongueurGraduationMineure) * cos(angle) + Xc;
            y2 := (Ry - LongueurGraduationMineure) * -sin(angle) + Yc;
            Canvas.DrawLine(TPointF.Create(x1, y1), TPointF.Create(x2, y2), 1);
          end;
        end;
      end;
    end;
  end;
  // Dessin de l'aiguille

  if Valeur < Mini then
  begin
    alpha := alpha0;
  end
  else
  begin
    if Valeur > Maxi then
    begin
      alpha := alpha1;
    end
    else
    begin
      alpha := (Valeur - Mini) / (Maxi - Mini) * (alpha1 - alpha0) + alpha0;
    end;
  end;
  {
    setLength(aiguille, 8);
    x1 := (Rx + 4) * cos(alpha) + Xc;
    y1 := (Ry + 4) * -sin(alpha) + Yc;
    aiguille[0] := TPointF(x1, y1);
    x1 := 4 * cos(alpha + Pi / 2) + Xc;
    y1 := 4 * -sin(alpha + Pi / 2) + Yc;
    aiguille[1] := TPointF(x1, y1);
    x1 := 2 * cos(alpha + Pi / 2) + Xc;
    y1 := 2 * -sin(alpha + Pi / 2) + Yc;
    aiguille[2] := TPointF(x1, y1);
    x1 := -(Rx + 4) / 10 * cos(alpha) + 4 * cos(alpha + Pi / 2) + Xc;
    y1 := -(Ry + 4) / 10 * -sin(alpha) + 4 * -sin(alpha + Pi / 2) + Yc;
    aiguille[3] := TPointF(x1, y1);
    x1 := -(Rx + 4) / 10 * cos(alpha) + 4 * cos(alpha + Pi / 2) + Xc;
    y1 := -(Ry + 4) / 10 * -sin(alpha) + 4 * -sin(alpha + Pi / 2) + Yc;
    aiguille[4] := TPointF(x1, y1);
  }

  x1 := (Rx + 4) * cos(alpha) + Xc;
  y1 := (Ry + 4) * -sin(alpha) + Yc;
  pen := TStrokeBrush.Create(TBrushKind.Solid, claBlack);
  pen.Thickness := 2;
  Canvas.DrawLine(TPointF.Create(x1, y1), TPointF.Create(Xc, Yc), 1, pen);
  pen.Free;
  br := TBrush.Create(TBrushKind.Solid, claBlack);
  Canvas.FillEllipse(TRectF.Create(Xc - 4, Yc - 4, Xc + 4, Yc + 4), 1, br);
  br.Free;

  // test
  // Canvas.DrawLine(TPointF.Create(Width, 0), TPointF.Create(0, Height), 1);
  Canvas.EndScene;
end;

constructor TJaugeCir.Create(AOwner: TComponent);
begin
  inherited;
  FFormatValeurs := '%2.1f';
  FMaxi := 100;
  FMini := 0;
  FValeur := 50;
  FSeuil_A := 25;
  FSeuil_B := 75;
  FGraduationMajeure := 10;
  FGraduationMineure := 2;
  FMontreGraduationMajeure := true;
  FMontreGraduationMineure := true;
  FMontreValeurs := true;
  FMontreSeuils := true;
  FEpaisseurSeuil := 4;
  FEpaisseurBordure := 16;
  FLgGrdMaj := 16;
  FLgGrdMin := 8;
  FGenre := DemiCercle;
  FCadran := Custom;
end;

function TJaugeCir.dessineBordure(mat: TMatiere; lrg, htr: integer; gnr: TGenre; Xc, Yc, Rx, Ry: Single): TBitmap;

var
  bmp: TBitmap;
  // ptA, ptB, v: TPointF;
  ptO: TPointF;
  x, y, n: integer;
  L, d, dc, rr, gg, bb: Single;
  dpt, dx2, dy2, rx2, ry2, rbx2, rby2: Single;
  coul: TAlphaColor;
  dta: TBitmapData;
  dedans: boolean;
  dx, dy: Single;
  {
    const

    vr: array [0 .. 8] of byte = (136, 124, 231, 216, 203, 142, 149, 243, 131);
    vg: array [0 .. 8] of byte = (108, 99, 194, 179, 165, 112, 120, 209, 102);
    vb: array [0 .. 8] of byte = (45, 35, 124, 111, 103, 40, 50, 137, 32);
    fracs: array [0 .. 8] of Single = (0, 0.139116203, 0.291325696, 0.340425532,
    0.425531915, 0.602291326, 0.669394435, 0.818330606, 1);
  }
begin
  bmp := TBitmap.Create(lrg, htr);
  // ptA := TPointF.Create(Xc - 0.707 * (Rx + FEpaisseurBordure / 2),
  // Yc - 0.707 * (Ry + FEpaisseurBordure / 2));
  // ptB := TPointF.Create(Xc + 0.707 * (Rx + FEpaisseurBordure / 2),
  // Yc + 0.707 * (Ry + FEpaisseurBordure / 2));
  // v := TPointF.Create(ptB.y - ptA.y, ptA.x - ptB.x);
  // d := v.x * v.x + v.y * v.y;
  rx2 := Rx * Rx;
  ry2 := Ry * Ry;
  rbx2 := (Rx + FEpaisseurBordure) * (Rx + FEpaisseurBordure);
  rby2 := (Ry + FEpaisseurBordure) * (Ry + FEpaisseurBordure);

  ptO := TPointF.Create(Xc - 2 * Rx / 3, Yc - 2 * Ry / 3);
  // ptO := TPointF.Create(Xc,Yc);
  if bmp.Map(TMapAccess.Write, dta) then
  begin
    for x := 0 to bmp.Width - 1 do
    begin
      for y := 0 to bmp.Height - 1 do
      begin
        dedans := false;
        dx2 := (x - Xc) * (x - Xc);
        dy2 := (y - Yc) * (y - Yc);
        case FGenre of
          DemiCercle:
            begin
              if (y > Yc) then
              begin
                dpt := dx2 / rbx2 + dy2 / rby2;
                dedans := (dpt <= 1);
              end
              else
              begin
                dpt := dx2 / rx2 + dy2 / ry2;
                if dpt >= 1 then
                begin
                  dpt := dx2 / rbx2 + dy2 / rby2;
                  dedans := (dpt <= 1);
                end;
              end;
            end;
          Cercle:
            begin
              dpt := dx2 / rx2 + dy2 / ry2;
              if dpt >= 1 then
              begin
                dpt := dx2 / rbx2 + dy2 / rby2;
                dedans := (dpt <= 1);
              end;
            end;
          QuartDeCercleHautDroit:
            begin
              if (x < Xc) or (y > Yc) then
              begin
                dpt := dx2 / rbx2 + dy2 / rby2;
                dedans := (dpt <= 1);
              end
              else
              begin
                dpt := dx2 / rx2 + dy2 / ry2;
                if dpt >= 1 then
                begin
                  dpt := dx2 / rbx2 + dy2 / rby2;
                  dedans := (dpt <= 1);
                end;
              end;
            end;
          QuartDeCercleHautGauche:
            begin
              if (x > Xc) or (y > Yc) then
              begin
                dpt := dx2 / rbx2 + dy2 / rby2;
                dedans := (dpt <= 1);
              end
              else
              begin
                dpt := dx2 / rx2 + dy2 / ry2;
                if dpt >= 1 then
                begin
                  dpt := dx2 / rbx2 + dy2 / rby2;
                  dedans := (dpt <= 1);
                end;
              end;
            end;
          QuartDeCercleBasDroit:
            begin
              if (x < Xc) or (y < Yc) then
              begin
                dpt := dx2 / rbx2 + dy2 / rby2;
                dedans := (dpt <= 1);
              end
              else
              begin
                dpt := dx2 / rx2 + dy2 / ry2;
                if dpt >= 1 then
                begin
                  dpt := dx2 / rbx2 + dy2 / rby2;
                  dedans := (dpt <= 1);
                end;
              end;
            end;
          QuartDeCercleBasGauche:
            begin
              if (x > Xc) or (y < Yc) then
              begin
                dpt := dx2 / rbx2 + dy2 / rby2;
                dedans := (dpt <= 1);
              end
              else
              begin
                dpt := dx2 / rx2 + dy2 / ry2;
                if dpt >= 1 then
                begin
                  dpt := dx2 / rbx2 + dy2 / rby2;
                  dedans := (dpt <= 1);
                end;
              end;
            end;
        end;
        if dedans then
        begin
          // L := ((ptA.x - x) * v.y + (y - ptA.y) * v.x) / d;
          dx := x - ptO.x;
          dy := y - ptO.y;
          if dx = 0 then
          begin
            if dy < 0 then
              L := 3 * Pi / 4
            else
              L := Pi / 4;
          end
          else
          begin
            L := arctan(dy / dx);
            if dx < 0 then
              L := L + Pi;
            if (dx > 0) and (dy < 0) then
              L := L + 2 * Pi;
          end;
          L := L / 2 / Pi;
          coul := getCoulMatiere(FAspectBordure, L)

        end
        else
          coul := 0;
        dta.SetPixel(x, y, coul);
      end;
    end;
    bmp.Unmap(dta);
  end;
  result := bmp;
end;

function TJaugeCir.HorsLimites: integer;
begin
  if FValeur < FSeuil_A then
    result := -1
  else if FValeur > FSeuil_B then
    result := 1
  else
    result := 0;
end;

procedure TJaugeCir.SetFormatValeurs(Value: String);
begin
  FFormatValeurs := Value;
end;

procedure TJaugeCir.SetGenreSeuil(Value: integer);
begin
  if (Value >= 1) and (Value <= 2) then
    FGenreSeuil := Value;
end;

procedure TJaugeCir.SetEpaisseurBordure(Value: integer);
begin
  FEpaisseurBordure := Value;
end;

procedure TJaugeCir.SetEpaisseurSeuil(Value: integer);
begin
  FEpaisseurSeuil := Value;
end;

procedure TJaugeCir.SetMaxi(Value: double);
begin
  FMaxi := Value;
end;

procedure TJaugeCir.SetMini(Value: double);
begin
  FMini := Value;
end;

procedure TJaugeCir.SetValeur(Value: double);
begin
  if FValeur <> Value then
  begin
    FValeur := Value;
    Repaint;
  end;
end;

procedure TJaugeCir.SetSeuil_A(Value: double);
begin
  FSeuil_A := Value;
end;

procedure TJaugeCir.SetSeuil_B(Value: double);
begin
  FSeuil_B := Value;
end;

procedure TJaugeCir.SetGraduationMajeure(Value: double);
begin
  FGraduationMajeure := Value;
end;

procedure TJaugeCir.SetGraduationMineure(Value: double);
begin
  FGraduationMineure := Value;
end;

procedure TJaugeCir.SetLgGrdMaj(Value: integer);
begin
  FLgGrdMaj := Value;
end;

procedure TJaugeCir.SetLgGrdMin(Value: integer);
begin
  FLgGrdMin := Value;
end;

procedure TJaugeCir.SetMontreGraduationMajeure(Value: boolean);
begin
  FMontreGraduationMajeure := Value;
end;

procedure TJaugeCir.SetMontreGraduationMineure(Value: boolean);
begin
  FMontreGraduationMineure := Value;
end;

procedure TJaugeCir.SetMontreValeurs(Value: boolean);
begin
  FMontreValeurs := Value;
end;

procedure TJaugeCir.SetMontreSeuils(Value: boolean);
begin
  FMontreSeuils := Value;
end;

// initialization

// FMX.Types.GlobalUseGPUCanvas := true;

end.
