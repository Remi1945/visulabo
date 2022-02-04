unit JaugeCirculaire;

interface

uses
  System.SysUtils, System.Classes, System.Types, FMX.Types, FMX.Controls, FMX.Objects, FMX.Graphics, System.UITypes,
  System.UIConsts, System.Math.Vectors;

type
  TJaugeCirculaire = class(TRectangle)
  private
  {$IFDEF BON}
    FFormatValeurs: String;
    FMaxi, FMini, FValeur, FSeuil_A, FSeuil_B, FGraduationMajeure, FGraduationMineure: double;
    FMontreGraduationMajeure, FMontreGraduationMineure, FMontreValeurs, FMontreSeuils, FMontreLed: boolean;
    FGenre, FEpaisseurBordure, FLgGrdMaj, FLgGrdMin: integer;


    procedure SetFormatValeurs(Value: String);
    procedure SetGenre(Value: integer);
    procedure SetEpaisseurBordure(Value: integer);
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
    procedure SetMontreLed(Value: boolean);
    {$ENDIF}

  protected
    { Déclarations protégées }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
  {$IFDEF BON}
    property FormatValeurs: String read FFormatValeurs write SetFormatValeurs;
    property Genre: integer read FGenre write SetGenre;
    property EpaisseurBordure: integer read FEpaisseurBordure write SetEpaisseurBordure;
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
    property MontreLed: boolean read FMontreLed write SetMontreLed;
    {$ENDIF}
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TJaugeCirculaire]);
end;

constructor TJaugeCirculaire.Create(AOwner: TComponent);
begin

  {
  FormatValeurs := '%2.1f';
  Maxi := 100;
  Mini := 0;
  Valeur := 50;
  Seuil_A := 25;
  Seuil_B := 75;
  GraduationMajeure := 10;
  GraduationMineure := 2;
  MontreGraduationMajeure := true;
  MontreGraduationMineure := true;
  MontreValeurs := true;
  MontreSeuils := true;
  MontreLed := true;
  EpaisseurBordure := 4;
  LongueurGraduationMajeure := 16;
  LongueurGraduationMineure := 8;
  }
end;
{$IFDEF BON}
procedure TJaugeCirculaire.SetFormatValeurs(Value: String);
begin
  FFormatValeurs := Value;
end;

procedure TJaugeCirculaire.SetGenre(Value: integer);
begin
  FGenre := Value;
end;

procedure TJaugeCirculaire.SetEpaisseurBordure(Value: integer);
begin
  FEpaisseurBordure := Value;
end;

procedure TJaugeCirculaire.SetMaxi(Value: double);
begin
  FMaxi := Value;
end;

procedure TJaugeCirculaire.SetMini(Value: double);
begin
  FMini := Value;
end;

procedure TJaugeCirculaire.SetValeur(Value: double);
begin
  FValeur := Value;
end;

procedure TJaugeCirculaire.SetSeuil_A(Value: double);
begin
  FSeuil_A := Value;
end;

procedure TJaugeCirculaire.SetSeuil_B(Value: double);
begin
  FSeuil_B := Value;
end;

procedure TJaugeCirculaire.SetGraduationMajeure(Value: double);
begin
  FGraduationMajeure := Value;
end;

procedure TJaugeCirculaire.SetGraduationMineure(Value: double);
begin
  FGraduationMineure := Value;
end;

procedure TJaugeCirculaire.SetLgGrdMaj(Value: integer);
begin
  FLgGrdMaj := Value;
end;

procedure TJaugeCirculaire.SetLgGrdMin(Value: integer);
begin
  FLgGrdMin := Value;
end;

procedure TJaugeCirculaire.SetMontreGraduationMajeure(Value: boolean);
begin
  FMontreGraduationMajeure := Value;
end;

procedure TJaugeCirculaire.SetMontreGraduationMineure(Value: boolean);
begin
  FMontreGraduationMineure := Value;
end;

procedure TJaugeCirculaire.SetMontreValeurs(Value: boolean);
begin
  FMontreValeurs := Value;
end;

procedure TJaugeCirculaire.SetMontreSeuils(Value: boolean);
begin
  FMontreSeuils := Value;
end;

procedure TJaugeCirculaire.SetMontreLed(Value: boolean);
begin
  FMontreLed := Value;
end;
{$ENDIF}

procedure TJaugeCirculaire.Paint;
const
  marge: Single = 4;
  Ns: integer = 200;
var
  rect, led, RectEcr: TRectF;
  decr: double;
  stMax, stMin, st: String;
  Wtxt, Htxt: Single;
  Xc, Yc, Rx, Ry, RxEcr, RyEcr: Single;
  alpha0, alpha1, alpha: Single;
  br: TBrush;
  pen: TStrokeBrush;
  cadran, cadranS: TPolygon;
  dalpha: Single;
  i, j: integer;
  angle: double;
  x, y, x1, x2, y1, y2: Single;
  indxA, indxB, i0, i1, L: integer;
  dalphaMaj, dalphaMin: Single;
  nbgradMaj, nbgradMin: integer;
  xecr, yecr, wrecr, hrecr: Single;
  angleMaj, angleMin: double;
begin
  Canvas.BeginScene;
  {$IFDEF OK}
  fgljghlghl
  // calcul de la hauteur des textes
  // Jauge en demi cercle
  stMax := Format(FormatValeurs, [Maxi]);
  stMin := Format(FormatValeurs, [Mini]);
  Htxt := Canvas.TextHeight(stMax);
  if Canvas.TextHeight(stMin) > Htxt then
    Htxt := Canvas.TextHeight(stMin);
  Wtxt := Canvas.TextWidth(stMax);
  if Canvas.TextWidth(stMin) > Wtxt then
    Wtxt := Canvas.TextWidth(stMin);
  Xc := Width / 2;
  Yc := Height - marge - Htxt / 2;
  Rx := Width / 2 - marge;
  Ry := Height - marge - Htxt / 2;
  if Ry > Rx then
    Ry := Rx;
  led := TRectF.Create(0, 0, Rx / 10, Rx / 10);
  alpha0 := Pi;
  alpha1 := 0;
  led.SetLocation(Width / 2 - led.Width / 2, Height - Rx / 2 - led.Height / 2);
  if Genre = 1 then
  begin
    Xc := Width / 2;
    Yc := Height / 2;
    Rx := Width / 2 - marge;
    Ry := Height / 2 - marge;
    alpha0 := 4 * Pi / 3;
    alpha1 := -Pi / 3;
    led.SetLocation(Width / 2 - led.Width / 2, Ry / 2 - led.Width / 2);
  end;
  if Genre = 2 then
  begin
    Xc := Wtxt;
    Yc := Height - Htxt;
    Rx := Width - marge - Wtxt;
    Ry := Height - marge - Htxt;
    alpha0 := 0;
    alpha1 := Pi / 2;

  end;
  if Genre = 3 then
  begin
    Xc := Width - Wtxt;
    Yc := Height - Htxt;
    Rx := Width - marge - Wtxt;
    Ry := Height - marge - Htxt;
    alpha0 := Pi;
    alpha1 := Pi / 2;

  end;
  if Genre = 4 then
  begin
    Xc := Width - Wtxt;
    Yc := Htxt;
    Rx := Width - marge - Wtxt;
    Ry := Height - marge - Htxt;
    alpha0 := 3 * Pi / 2;
    alpha1 := Pi;

  end;
  if Genre = 5 then
  begin
    Xc := Wtxt;
    Yc := Htxt;
    Rx := Width - marge - Wtxt;
    Ry := Height - marge - Htxt;
    alpha0 := -Pi / 2;
    alpha1 := 0;
    led.SetLocation(marge + 32, Height / 2 - led.Width / 2);

  end;
  // Fond transparent
  br := TBrush.Create(TBrushKind.Solid, 0);
  rect := TRectF.Create(0, 0, Width, Height);
  Canvas.FillRect(rect, 0, 0, AllCorners, 100, br);
  br.Free;

  // Dessin du cadran
  // --------------------------------------------
  setLength(cadran, 3 + Ns);
  cadran[0] := TPointF.Create(Xc, Yc);
  dalpha := (alpha1 - alpha0) / Ns;
  for i := 0 to Ns do
  begin
    angle := alpha0 + dalpha * i;
    x := Xc + cos(angle) * Rx;
    y := Yc - sin(angle) * Ry;
    cadran[i + 1] := TPointF.Create(x, y);
  end;
  cadran[Ns + 2] := TPointF.Create(Xc, Yc);
  Canvas.Fill := Fill;
  Canvas.FillPolygon(cadran, 1);
  // Dessin du contour du cadran
  setLength(cadran, 3 + Ns * 2);
  dalpha := (alpha1 - alpha0) / Ns;
  for i := 0 to Ns do
  begin
    angle := alpha0 + dalpha * i;
    x1 := Xc + cos(angle) * Rx;
    y1 := Yc - sin(angle) * Ry;
    cadran[i] := TPointF.Create(x1, y1);
    x2 := Xc + cos(angle) * (Rx - EpaisseurBordure);
    y2 := Yc - sin(angle) * (Ry - EpaisseurBordure);
    cadran[Ns * 2 + 1 - i] := TPointF.Create(x2, y2);
  end;
  cadran[Ns * 2 + 2] := cadran[0];
  if MontreSeuils then
  begin
    indxA := -1;
    indxB := -1;
    i0 := -1;
    i1 := -1;
    L := -1;
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
        cadranS[i] := cadran[i0 + i];
        cadranS[L * 2 + 1 - i] := cadran[i1 - i];
      end;
      cadranS[indxA * 2 + 2] := cadran[i0];
      br := TBrush.Create(TBrushKind.Solid, claGreen);
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
        cadranS[i] := cadran[i0 + i];
        cadranS[L * 2 + 1 - i] := cadran[i1 - i];
      end;
      cadranS[L * 2 + 2] := cadran[i0];
      br := TBrush.Create(TBrushKind.Solid, claYellow);
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
        cadranS[i] := cadran[i0 + i];
        cadranS[L * 2 + 1 - i] := cadran[i1 - i];
      end;
      cadranS[L * 2 + 2] := cadran[i0];
      br := TBrush.Create(TBrushKind.Solid, claRed);
      Canvas.Fill := br;
      Canvas.FillPolygon(cadranS, 1);
      br.Free;
      // affichage de la led
      if MontreLed then
      begin
        br := TBrush.Create(TBrushKind.Solid, claGreen);
        br.Kind := TBrushKind.Gradient;
        br.Gradient.Style := TGradientStyle.Radial;
        br.Gradient.Color := claGray;
        if Valeur < Seuil_A then
        begin
          br.Gradient.Color := claGreen;
        end;
        if Valeur > Seuil_B then
        begin
          br.Gradient.Color := claRed;
        end;
        br.Gradient.Color1 := claWhite;
        br.Gradient.RadialTransform.RotationCenter.x := 0.7;
        br.Gradient.RadialTransform.RotationCenter.y := 0.2;
        Canvas.FillEllipse(led, 1, br);
        br.Free;
      end;
    end;
  end;
  Canvas.Stroke.Color := Stroke.Color;
  Canvas.DrawPolygon(cadran, 1);

  // --------------------------------------------
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
  wrecr := 0;
  hrecr := 0;

  if MontreValeurs then
  begin
    for i := 0 to nbgradMaj - 1 do
    begin
      st := Format(FormatValeurs, [Mini + i * GraduationMajeure]);
      if Canvas.TextHeight(st) > hrecr then
        hrecr := Canvas.TextHeight(st);
      if Canvas.TextWidth(st) > wrecr then
        wrecr := Canvas.TextWidth(st);
    end;
    decr := sqrt(wrecr * wrecr + hrecr * hrecr);
    RxEcr := Rx - decr + 4;
    RyEcr := Ry - decr + 4;
    Canvas.Fill.Color := Stroke.Color;
    for i := 0 to nbgradMaj - 1 do
    begin
      angleMaj := alpha0 + i * dalphaMaj;
      xecr := RxEcr * cos(angleMaj) + Xc;
      yecr := RyEcr * -sin(angleMaj) + Yc;
      RectEcr.left := xecr - wrecr / 2;
      RectEcr.top := yecr - hrecr / 2;
      RectEcr.right := xecr + wrecr / 2;
      RectEcr.bottom := yecr + hrecr / 2;
      st := Format(FormatValeurs, [Mini + i * GraduationMajeure]);
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
  alpha := 0;
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
  x1 := (Rx + 4) * cos(alpha) + Xc;
  y1 := (Ry + 4) * -sin(alpha) + Yc;
  pen := TStrokeBrush.Create(TBrushKind.Solid, claRed);
  pen.Thickness := 2;
  Canvas.DrawLine(TPointF.Create(x1, y1), TPointF.Create(Xc, Yc), 1, pen);
  pen.Free;
  br := TBrush.Create(TBrushKind.Solid, claRed);
  Canvas.FillEllipse(TRectF.Create(Xc - 4, Yc - 4, Xc + 4, Yc + 4), 1, br);
  br.Free;
  {$ENDIF}
  Canvas.EndScene;

end;

end.
