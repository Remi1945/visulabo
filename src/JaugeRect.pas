unit JaugeRect;

interface

uses
  System.SysUtils, System.Classes, System.Types, FMX.Types, FMX.Controls,
  FMX.Objects, FMX.Graphics, System.UITypes,
  System.UIConsts, System.Math.Vectors;

type
  TGenre = (Horizontale, Vertcale);
  TCadran = (Blanc, DegradeNoirClair, DegradeNoirFonce, Custom);
  TGenreSeuil = (SeuilRougeVertRouge, SeuilVertJauneRouge);

  TJaugeRect = class(TRectangle)
  private
    FFormatValeurs: String;
    FMaxi, FMini, FValeur, FSeuil_A, FSeuil_B, FGraduationMajeure, FGraduationMineure, FDynamique: double;
    FMontreGraduationMajeure, FMontreGraduationMineure, FMontreValeurs, FMontreSeuils, FGradMobile: boolean;
    FEpaisseurBordure, FEpaisseurSeuil, FLgGrdMaj, FLgGrdMin: integer;
    FGenre: TGenre;
    FCadran: TCadran;
    FGenreSeuil: TGenreSeuil;
    procedure SetFormatValeurs(Value: String);
    procedure SetGenreSeuil(Value: TGenreSeuil);
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
    property GenreSeuil: TGenreSeuil read FGenreSeuil write SetGenreSeuil;
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
  xs0, xs1: Single;
  // L: integer;
  dpixMaj, dpixMin: Single;
  nbgradMaj, nbgradMin: integer;
  sens: integer;
  Bordure: TBitmap;
  letendue: double;
  grad0: integer;
  xecr, yecr: Single;
  aa: Single;
  bb: Single;
  fin: boolean;
  curseur: TPolygon;
  wcurs: Single;
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
  //Dession du fond

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
  rect.Left := rect.Left + FEpaisseurBordure;
  rect.Right := rect.Right - FEpaisseurBordure;
  rect.Top := rect.Top + FEpaisseurBordure;
  rect.Bottom := rect.Bottom - FEpaisseurBordure;

  Canvas.FillRect(rect, 1);
  Xc := Width / 2;
  Yc := Height / 2;

  // calculs des constantes en fonction de la configuration de la jauge
  if FGenre = Horizontale then
  begin
    if FGradMobile then
    begin
      letendue := FDynamique;
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
      aa := (Width - 2 * FEpaisseurBordure) / letendue;
      bb := Width / 2 - aa * Valeur;
    end
    else
    begin
      letendue := FMaxi - FMini;
      st := Format(FFormatValeurs, [FMini]);
      Wtxt := Canvas.TextWidth(st);
      st := Format(FFormatValeurs, [FMaxi]);
      if Wtxt < Canvas.TextWidth(st) then
        Wtxt := Canvas.TextWidth(st);
      if GraduationMajeure > 0 then
      begin
        nbgradMaj := round(letendue / GraduationMajeure) + 1;
        dpixMaj := (Width - 2 * FEpaisseurBordure - Wtxt) / (nbgradMaj - 1);
        if (GraduationMineure > 0) and (GraduationMajeure > GraduationMineure) then
        begin
          nbgradMin := round(GraduationMajeure / GraduationMineure);
          dpixMin := dpixMaj / nbgradMin;
        end;
      end;
      aa := (Width - 2 * FEpaisseurBordure - Wtxt) / letendue;
      bb := Width / 2 - aa * (FMaxi + FMini) / 2;
    end;
  end
  else
  begin
    if FGradMobile then
    begin
    end
    else
    begin
    end;
  end;

  // Colorisation des seuils
  if MontreSeuils then
  begin

    if (Seuil_A > FMini) and (Seuil_A < FMaxi) and (Seuil_B > FMini) and (Seuil_B < FMaxi) and (Seuil_A < Seuil_B) then
    begin
      br := TBrush.Create(TBrushKind.Solid, claGreen);
      if GenreSeuil = SeuilRougeVertRouge then
        br.Color := claRed;
      if FGenre = Horizontale then
      begin
        // Coloration de mini à seuil A
        if FGradMobile then
        begin
          xs0 := aa * (FValeur - letendue / 2) + bb;
          if xs0 < FEpaisseurBordure then
            xs0 := FEpaisseurBordure;
          if xs0 > Width - FEpaisseurBordure then
            xs0 := Width - FEpaisseurBordure;
          xs1 := aa * Seuil_A + bb;
          if xs1 < FEpaisseurBordure then
            xs1 := FEpaisseurBordure;
          if xs1 > Width - FEpaisseurBordure then
            xs1 := Width - FEpaisseurBordure;
        end
        else
        begin
          xs0 := aa * FMini + bb;
          xs1 := aa * Seuil_A + bb;
        end;
        if xs1 > xs0 then
          Canvas.FillRect(TRectF.Create(xs0, FEpaisseurBordure, xs1, FEpaisseurBordure + FEpaisseurSeuil), 0, 0,
            AllCorners, 100, br);
        br.Free;
        xs0 := xs1;
        // Coloration de SeuilA à seuil B
        if GenreSeuil = SeuilRougeVertRouge then
          br.Color := claGreen;
        if FGradMobile then
        begin

          xs1 := aa * Seuil_B + bb;
          if xs1 < FEpaisseurBordure then
            xs1 := FEpaisseurBordure;
          if xs1 > Width - FEpaisseurBordure then
            xs1 := Width - FEpaisseurBordure;
        end
        else
        begin
          xs1 := aa * Seuil_B + bb;
        end;
        br := TBrush.Create(TBrushKind.Solid, claYellow);
        if xs1 > xs0 then
        Canvas.FillRect(TRectF.Create(xs0, FEpaisseurBordure, xs1, FEpaisseurBordure + FEpaisseurSeuil), 0, 0,
          AllCorners, 100, br);

        br.Free;
        // Coloration de SeuilB à maxi
        xs0 := xs1;
        if FGradMobile then
        begin

          xs1 := aa * (FValeur + letendue / 2)  + bb;
          if xs1 < FEpaisseurBordure then
            xs1 := FEpaisseurBordure;
          if xs1 > Width - FEpaisseurBordure then
            xs1 := Width - FEpaisseurBordure;
        end
        else
        begin
          xs1 := aa * FMaxi + bb;
        end;
        br := TBrush.Create(TBrushKind.Solid, claRed);
        if xs1 > xs0 then
        Canvas.FillRect(TRectF.Create(xs0, FEpaisseurBordure, xs1, FEpaisseurBordure + FEpaisseurSeuil), 0, 0,
          AllCorners, 100, br);
        br.Free;

      end;
    end;
  end;

  if MontreValeurs and (letendue > 0) then
  begin
    Canvas.Fill.Color := Stroke.Color;
    if FGenre = Horizontale then
    begin
      Xc := Width / 2;
      Yc := Height / 2;

      if FGradMobile then
      begin
        grad0 := trunc(FValeur / FGraduationMajeure);
        sens := -1;
        repeat
          i := 0;
          fin := false;
          while not(fin) do
          begin
            grad := (grad0 + i * sens) * GraduationMajeure;
            st := Format(FFormatValeurs, [grad]);
            Htxt := Canvas.TextHeight(st);
            Wtxt := Canvas.TextWidth(st);
            xecr := aa * (grad0 + i * sens) * FGraduationMajeure + bb;
            yecr := EpaisseurBordure + LongueurGraduationMajeure + 4;
            if sens < 0 then
              fin := xecr - Wtxt / 2 < EpaisseurBordure
            else
              fin := xecr + Wtxt / 2 > Width - EpaisseurBordure;
            if not(fin) then
            begin
              RectEcr.Left := xecr - Wtxt / 2;
              RectEcr.Top := yecr;
              RectEcr.Right := xecr + Wtxt / 2;
              RectEcr.Bottom := yecr + Htxt;
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
                    x1 := xecr + j * sens * dpixMin;
                    if sens < 0 then
                      fin := x1 < FEpaisseurBordure
                    else
                      fin := x1 > Width - FEpaisseurBordure;
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
          sens := sens + 2;
        until sens > 1;
        // dessin du curseur
        // ligne verticale avec en dessous un triangle equilatéral
        Canvas.Fill.Color := claRed;
        Canvas.Stroke.Color := claRed;
        Canvas.DrawLine(TPointF.Create(Xc, FEpaisseurBordure), TPointF.Create(Xc, Height - FEpaisseurBordure), 1);
        setLength(curseur, 4);
        if MontreGraduationMajeure then
        begin
          curseur[0] := TPointF.Create(Xc, yecr + 4 + Htxt);
          wcurs := Htxt / sqrt(3);
        end
        else
        begin
          curseur[0] := TPointF.Create(Xc, Yc);
          wcurs := (Yc - FEpaisseurBordure) / sqrt(3);
        end;
        curseur[1] := TPointF.Create(Xc - wcurs, Height - FEpaisseurBordure);
        curseur[2] := TPointF.Create(Xc + wcurs, Height - FEpaisseurBordure);
        curseur[3] := curseur[0];
        Canvas.FillPolygon(curseur, 1);
      end
      else
      begin

        grad := FMini;

        while grad < FMaxi do
        begin
          st := Format(FFormatValeurs, [grad]);
          Htxt := Canvas.TextHeight(st);
          Wtxt := Canvas.TextWidth(st);
          xecr := aa * grad + bb;
          yecr := EpaisseurBordure + LongueurGraduationMajeure + 4;
          grad := grad + FGraduationMajeure;
          RectEcr.Left := xecr - Wtxt / 2;
          RectEcr.Top := yecr;
          RectEcr.Right := xecr + Wtxt / 2;
          RectEcr.Bottom := yecr + Htxt;
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
              for j := 1 to nbgradMin - 1 do
              begin
                x1 := xecr + j * dpixMin;
                y1 := EpaisseurBordure + 4;
                x2 := x1;
                y2 := y1 + LongueurGraduationMineure;
                Canvas.DrawLine(TPointF.Create(x1, y1), TPointF.Create(x2, y2), 1);
              end;
            end;
          end;
        end;
        st := Format(FFormatValeurs, [FMaxi]);
        Htxt := Canvas.TextHeight(st);
        Wtxt := Canvas.TextWidth(st);
        xecr := aa * grad + bb;
        yecr := EpaisseurBordure + LongueurGraduationMajeure + 4;
        grad := grad + FGraduationMajeure;
        RectEcr.Left := xecr - Wtxt / 2;
        RectEcr.Top := yecr;
        RectEcr.Right := xecr + Wtxt / 2;
        RectEcr.Bottom := yecr + Htxt;
        Canvas.FillText(RectEcr, st, false, 1, [], TTextAlign.Center, TTextAlign.Center);
        if MontreGraduationMajeure then
        begin
          x1 := xecr;
          y1 := EpaisseurBordure + 4;
          x2 := xecr;
          y2 := y1 + LongueurGraduationMajeure;
          Canvas.DrawLine(TPointF.Create(x1, y1), TPointF.Create(x2, y2), 1);
        end;
        // curseur
        Canvas.Fill.Color := claRed;
        Canvas.Stroke.Color := claRed;
        x1 := aa * Valeur + bb;
        Canvas.DrawLine(TPointF.Create(x1, FEpaisseurBordure), TPointF.Create(x1, Height - FEpaisseurBordure), 1);
        setLength(curseur, 4);
        if MontreGraduationMajeure then
        begin
          curseur[0] := TPointF.Create(x1, yecr + 4 + Htxt);
          wcurs := Htxt / sqrt(3);
        end
        else
        begin
          curseur[0] := TPointF.Create(x1, Yc);
          wcurs := (Yc - FEpaisseurBordure) / sqrt(3);
        end;
        curseur[1] := TPointF.Create(x1 - wcurs, Height - FEpaisseurBordure);
        curseur[2] := TPointF.Create(x1 + wcurs, Height - FEpaisseurBordure);
        curseur[3] := curseur[0];
        Canvas.FillPolygon(curseur, 1);
      end;
    end;
  end;

  // test
  // Canvas.DrawLine(TPointF.Create(Width, 0), TPointF.Create(0, Height), 1);
  Canvas.EndScene;
end;

constructor TJaugeRect.Create(AOwner: TComponent);
begin
  inherited;
  FFormatValeurs := '%2.1f';
  FMaxi := 40;
  FMini := -40;
  FValeur := 17;
  FSeuil_A := -25;
  FSeuil_B := +25;
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
  FGenreSeuil := SeuilVertJauneRouge;
  FCadran := Custom;
  FDynamique := 20;
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

procedure TJaugeRect.SetGenreSeuil(Value: TGenreSeuil);
begin
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
    if FGradMobile then
      FValeur := Value
    else
    begin
      if Value > FMaxi then
        FValeur := FMaxi
      else
      begin
        if Value < FMini then
          FValeur := FMini
        else
          FValeur := Value
      end;
    end;
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
  FGradMobile := Value;
  Repaint;
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
