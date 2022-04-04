unit JaugeRect;

interface

uses
  System.SysUtils, System.Classes, System.Types, FMX.Types, FMX.Controls,
  FMX.Objects, FMX.Graphics, System.UITypes,
  System.UIConsts, System.Math.Vectors;

type
  TGenre = (Horizontale, Vertcale);
  TCadran = (Blanc, DegradeNoirClair, DegradeNoirFonce, Custom);

  TJaugeRect = class(TRectangle)
  private
    FFormatValeurs: String;
    FMaxi, FMini, FValeur, FSeuil_A, FSeuil_B, FGraduationMajeure, FGraduationMineure, FDynamique: double;
    FMontreGraduationMajeure, FMontreGraduationMineure, FMontreValeurs, FMontreSeuils, FGradMobile: boolean;
    FEpaisseurBordure, FEpaisseurSeuil, FLgGrdMaj, FLgGrdMin, FGenreSeuil: integer;
    FGenre: TGenre;
    FCadran: TCadran;
    procedure SetFormatValeurs(Value: String);
    procedure SetGenreSeuil(Value: integer);
    procedure SetEpaisseurBordure(Value: integer);
    procedure SetEpaisseurSeuil(Value: integer);
    procedure SetLgGrdMaj(Value: integer);
    procedure SetLgGrdMin(Value: integer);
    procedure SetMaxi(Value: double);
    procedure SetMini(Value: double);
    procedure SetDynamique(Value: double);
    procedure SetValeur(Value: double);
    procedure SetSeuil_A(Value: double);
    procedure SetSeuil_B(Value: double);
    procedure SetGraduationMajeure(Value: double);
    procedure SetGraduationMineure(Value: double);
    procedure SetMontreGraduationMajeure(Value: boolean);
    procedure SetMontreGraduationMineure(Value: boolean);
    procedure SetMontreValeurs(Value: boolean);
    procedure SetMontreSeuils(Value: boolean);
    procedure SetGradMobile(Value: boolean);
    function dessineBordure(lrg, htr: integer; gnr: TGenre; Xc, Yc, Rx, Ry: Single): TBitmap;
    procedure SetFGradMobile(const Value: boolean);
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
    property EpaisseurSeuil: integer read FEpaisseurSeuil write SetEpaisseurSeuil;
    property LongueurGraduationMajeure: integer read FLgGrdMaj write SetLgGrdMaj;
    property LongueurGraduationMineure: integer read FLgGrdMin write SetLgGrdMin;
    property Maxi: double read FMaxi write SetMaxi;
    property Dynamique: double read FDynamique write SetDynamique;
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
    property GraduationMobile: boolean read FGradMobile write SetGradMobile;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TJaugeRect]);
end;

procedure TJaugeRect.Paint;
const
  marge: Single = 4;
  Ns: integer = 200;
var

  rect, RectEcr: TRectF;
  decr: double;
  st: String;
  Wtxt, Htxt: Single;
  Xc, Yc: Single;

  br: TBrush;
  pen: TStrokeBrush;

  i, j: integer;
  grad: double;
  x1, x2, y1, y2: Single;
  indxA, indxB, i0, i1, L: integer;
  dpixMaj, dpixMin: Single;
  nbgradMaj, nbgradMin: integer;

  Bordure: TBitmap;
  letendue: double;
  grad0: integer;
  xecr, yecr: Single;
  aa: Single;
  bb: Single;
  fin: boolean;
begin

  Canvas.BeginScene;
  // Fond transparent
  br := TBrush.Create(TBrushKind.Solid, 0);
  rect := TRectF.Create(0, 0, Width, Height);
  Canvas.FillRect(rect, 0, 0, AllCorners, 100, br);
  br.Free;

  // Dessin de la bordure
  {
    if FEpaisseurBordure > 0 then
    begin
    Bordure := dessineBordure(round(Width), round(Height), FGenre, Xc, Yc, Rx, Ry);
    Canvas.DrawBitmap(Bordure, TRectF.Create(0, 0, Width, Height), TRectF.Create(0, 0, Width, Height), 1, true);
    Bordure.Free;
    end;
  }
  // dessin de la bordure
  Canvas.Fill.Color := claBlack;
  Canvas.FillRect(rect, 1);
  // Dession du fond

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
  rect.Left:=rect.Left+FEpaisseurBordure;
  rect.Right:=rect.Right-FEpaisseurBordure;
  rect.Top:=rect.Top+FEpaisseurBordure;
  rect.Bottom:=rect.Bottom-FEpaisseurBordure;

  Canvas.FillRect(rect, 1);
  // affichage des graduations
  nbgradMaj := 2;
  nbgradMin := 0;
  if FGradMobile then
    letendue := FDynamique
  else
    letendue := FMaxi - FMini;
  if GraduationMajeure > 0 then
  begin
    nbgradMaj := round(letendue / GraduationMajeure) + 1;
    dpixMaj := (Width - 2 * FEpaisseurBordure) / (nbgradMaj - 1);
    if (GraduationMineure > 0) and (GraduationMajeure > GraduationMineure) then
    begin
      nbgradMin := round(GraduationMajeure / GraduationMineure);
      dpixMin := dpixMaj / nbgradMin;
    end;
  end;

  if MontreValeurs then
  begin
    Canvas.Fill.Color := Stroke.Color;
    if FGenre = Horizontale then
    begin
      Xc := Width / 2;
      Yc := Height / 2;
      if FGradMobile then
      begin
        aa := (Width - 2 * FEpaisseurBordure) / letendue;
        bb := Width / 2 - aa * Valeur;
        grad0 := trunc(FValeur / FGraduationMajeure);
        i := 0;
        fin := false;
        while not(fin) do
        begin
          grad := (grad0 - i) * GraduationMajeure;
          st := Format(FFormatValeurs, [grad]);
          Htxt := Canvas.TextHeight(st);
          Wtxt := Canvas.TextWidth(st);
          xecr := aa * (grad0 - i) * FGraduationMajeure + bb;
          yecr := EpaisseurBordure + LongueurGraduationMajeure + 4;
          fin := xecr - Wtxt / 2 < EpaisseurBordure;
          if not(fin) then
          begin
            RectEcr.left := xecr - Wtxt / 2;
            RectEcr.top := yecr;
            RectEcr.right := xecr + Wtxt / 2;
            RectEcr.bottom := yecr + Htxt ;
            Canvas.FillText(RectEcr, st, false, 1, [], TTextAlign.Center, TTextAlign.Center);
            if MontreGraduationMajeure then
            begin
              x1 := xecr;
              y1 := EpaisseurBordure + 4;
              x2 := xecr;
              y2 := y1 + LongueurGraduationMajeure;
              Canvas.DrawLine(TPointF.Create(x1, y1), TPointF.Create(x2, y2), 1);
              if (MontreGraduationMineure) and (GraduationMineure > 0) then
              begin
                j := 1;
                while not(fin) and (j < nbgradMin) do
                begin
                  x1 := xecr - j * dpixMin;
                  fin := x1 < FEpaisseurBordure;
                  if not(fin) then
                  begin
                    y1 := EpaisseurBordure + 4;
                    x2 := x1;
                    y2 := y1 + LongueurGraduationMineure;
                    Canvas.DrawLine(TPointF.Create(x1, y1), TPointF.Create(x2, y2), 1);
                  end;
                  inc(j);
                end;
              end;
            end;
          end;
          inc(i);
        end;
      end;
    end;
  end;

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

      i1 := Ns * 2 + 1;

      br := TBrush.Create(TBrushKind.Solid, claGreen);
      if GenreSeuil = 2 then
        br.Color := claRed;
      Canvas.Fill := br;

      br.Free;
      // Coloration de SeuilA à seuil B
      indxB := round((Seuil_B - Mini) / (Maxi - Mini) * Ns);

      br := TBrush.Create(TBrushKind.Solid, claYellow);
      if GenreSeuil = 2 then
        br.Color := claGreen;
      Canvas.Fill := br;

      br.Free;
      // Coloration de SeuilB à maxi

      i0 := indxB;
      i1 := Ns * 2 + 1 - indxB;

      br := TBrush.Create(TBrushKind.Solid, claRed);
      Canvas.Fill := br;

      br.Free;

    end;
  end;

  // Dessin de l'aiguille

  // test
  // Canvas.DrawLine(TPointF.Create(Width, 0), TPointF.Create(0, Height), 1);
  Canvas.EndScene;
end;

constructor TJaugeRect.Create(AOwner: TComponent);
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
  FGenre := Horizontale;
  FCadran := Custom;
end;

function TJaugeRect.dessineBordure(lrg, htr: integer; gnr: TGenre; Xc, Yc, Rx, Ry: Single): TBitmap;

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
const

  vr: array [0 .. 8] of byte = (136, 124, 231, 216, 203, 142, 149, 243, 131);
  vg: array [0 .. 8] of byte = (108, 99, 194, 179, 165, 112, 120, 209, 102);
  vb: array [0 .. 8] of byte = (45, 35, 124, 111, 103, 40, 50, 137, 32);
  fracs: array [0 .. 8] of Single = (0, 0.139116203, 0.291325696, 0.340425532, 0.425531915, 0.602291326, 0.669394435,
    0.818330606, 1);
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

        if dedans then
        begin
          // L := ((ptA.x - x) * v.y + (y - ptA.y) * v.x) / d;
          dx := x - ptO.x;
          dy := y - ptO.y;
          if dx = 0 then
          begin
            if dy < 0 then
              L := 3 * PI / 4
            else
              L := PI / 4;
          end
          else
          begin
            L := arctan(dy / dx);
            if dx < 0 then
              L := L + PI;
            if (dx > 0) and (dy < 0) then
              L := L + 2 * PI;
          end;
          L := L / 2 / PI;

          if L < 0 then
          begin
            coul := $FF000000 + vr[0] shl 16 + vg[0] shl 8 + vb[0];
          end
          else
          begin
            if L > 1 then
            begin
              coul := $FF000000 + vr[4] shl 16 + vg[4] shl 8 + vb[4];
            end
            else
            begin
              n := 0;
              while (L > fracs[n]) and (n < 9) do
                inc(n);
              dc := (L - fracs[n - 1]) / (fracs[n] - fracs[n - 1]);
              dc := sin(PI / 2 * dc);
              rr := (vr[n] - vr[n - 1]) * dc + vr[n - 1];
              gg := (vg[n] - vg[n - 1]) * dc + vg[n - 1];
              bb := (vb[n] - vb[n - 1]) * dc + vb[n - 1];
              coul := $FF000000 + round(rr) shl 16 + round(gg) shl 8 + round(bb);
            end;
          end;
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

function TJaugeRect.HorsLimites: integer;
begin
  if FValeur < FSeuil_A then
    result := -1
  else if FValeur > FSeuil_B then
    result := 1
  else
    result := 0;
end;

procedure TJaugeRect.SetFGradMobile(const Value: boolean);
begin
  FGradMobile := Value;
end;

procedure TJaugeRect.SetFormatValeurs(Value: String);
begin
  FFormatValeurs := Value;
end;

procedure TJaugeRect.SetGenreSeuil(Value: integer);
begin
  if (Value >= 1) and (Value <= 2) then
    FGenreSeuil := Value;
end;

procedure TJaugeRect.SetDynamique(Value: double);
begin
  FDynamique := Value;
end;

procedure TJaugeRect.SetEpaisseurBordure(Value: integer);
begin
  FEpaisseurBordure := Value;
end;

procedure TJaugeRect.SetEpaisseurSeuil(Value: integer);
begin
  FEpaisseurSeuil := Value;
end;

procedure TJaugeRect.SetMaxi(Value: double);
begin
  FMaxi := Value;
end;

procedure TJaugeRect.SetMini(Value: double);
begin
  FMini := Value;
end;

procedure TJaugeRect.SetValeur(Value: double);
begin
  if FValeur <> Value then
  begin
    FValeur := Value;
    Repaint;
  end;
end;

procedure TJaugeRect.SetSeuil_A(Value: double);
begin
  FSeuil_A := Value;
end;

procedure TJaugeRect.SetSeuil_B(Value: double);
begin
  FSeuil_B := Value;
end;

procedure TJaugeRect.SetGradMobile(Value: boolean);
begin
  FGradMobile := true;
end;

procedure TJaugeRect.SetGraduationMajeure(Value: double);
begin
  FGraduationMajeure := Value;
end;

procedure TJaugeRect.SetGraduationMineure(Value: double);
begin
  FGraduationMineure := Value;
end;

procedure TJaugeRect.SetLgGrdMaj(Value: integer);
begin
  FLgGrdMaj := Value;
end;

procedure TJaugeRect.SetLgGrdMin(Value: integer);
begin
  FLgGrdMin := Value;
end;

procedure TJaugeRect.SetMontreGraduationMajeure(Value: boolean);
begin
  FMontreGraduationMajeure := Value;
end;

procedure TJaugeRect.SetMontreGraduationMineure(Value: boolean);
begin
  FMontreGraduationMineure := Value;
end;

procedure TJaugeRect.SetMontreValeurs(Value: boolean);
begin
  FMontreValeurs := Value;
end;

procedure TJaugeRect.SetMontreSeuils(Value: boolean);
begin
  FMontreSeuils := Value;
end;

// initialization

// FMX.Types.GlobalUseGPUCanvas := true;

end.
