unit Histogramme;

interface

uses
  System.SysUtils, System.Classes, System.Types, FMX.Types, FMX.Controls, FMX.Objects, FMX.Graphics, System.UITypes,
  System.UIConsts, System.Math.Vectors, Math;

type
  THistogramme = class(TRectangle)
  private
    FFormatX: String;
    FSeuilMax: double;
    FSeuilMin: double;
    FMontreSeuils: boolean;
    FNbBins: integer;
    minX, maxX: double;
    Valeurs: array [0 .. 2047] of uint64;

    procedure SetFormatX(Value: string);
    procedure SetSeuilMin(Value: double);
    procedure SetSeuilMax(Value: double);
    procedure SetNbBins(Value: integer);
    procedure SetMontreSeuils(Value: boolean);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    // procedure MajValeurs(var val: array of double; nbvals: integer; redessine: boolean); overload;
    // procedure MajValeurs(min, max: double; var val: array of double; nbvals: integer; redessine: boolean); overload;
    procedure MajValeurs(min, max: integer; var val: array of uint16; nbvals: integer; redessine: boolean); overload;
    procedure MajValeurs(var val: array of uint16; nbvals: integer; redessine: boolean); overload;
    procedure MajValeurs(min, max: integer; var val: array of integer; nbvals: integer; redessine: boolean); overload;
    procedure MajValeurs(var val: array of integer; nbvals: integer; redessine: boolean); overload;
    procedure MajHisto(min, max: double; var val: array of integer; lng: integer; redessine: boolean); overload;
  published
    property FormatX: String read FFormatX write SetFormatX;
    property SeuilMax: double read FSeuilMax write SetSeuilMax;
    property SeuilMin: double read FSeuilMin write SetSeuilMin;
    property MontreSeuils: boolean read FMontreSeuils write SetMontreSeuils;
    property NombreDeClasse: integer read FNbBins write SetNbBins;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [THistogramme]);
end;

// ---------------------------------------------------------------------------
constructor THistogramme.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited;
  FFormatX := '%3.2f';
  minX := 4;
  maxX := 2000;
  FNbBins := 200;
  for i := 0 to FNbBins div 2 - 1 do
  begin
    Valeurs[i] := i * 2;
    Valeurs[FNbBins - 1 - i] := Valeurs[i];
  end;
end;

// ---------------------------------------------------------------------------
procedure THistogramme.SetFormatX(Value: String);
begin
  FFormatX := Value;
end;

// ---------------------------------------------------------------------------
procedure THistogramme.SetSeuilMin(Value: double);
begin
  FSeuilMin := Value;
end;

// ---------------------------------------------------------------------------
procedure THistogramme.SetSeuilMax(Value: double);
begin
  FSeuilMax := Value;
end;

// ---------------------------------------------------------------------------
procedure THistogramme.SetMontreSeuils(Value: boolean);
begin
  FMontreSeuils := Value;
end;

procedure THistogramme.SetNbBins(Value: integer);
begin
  if (Value > 0) and (Value < 2048) then
    FNbBins := Value;
end;

// ---------------------------------------------------------------------------
{
  procedure THistogramme.MajValeurs(var val: array of double; nbvals: integer; redessine: boolean);
  var
  n, i: integer;
  begin
  minX := val[0];
  maxX := val[0];
  for i := 1 to nbvals - 1 do
  begin
  if val[i] < minX then
  minX := val[i];
  if val[i] > maxX then
  maxX := val[i];
  end;
  MajValeurs(minX, maxX, val, nbvals, redessine);
  end;
}
// ---------------------------------------------------------------------------
{
  procedure THistogramme.MajValeurs(min, max: double; var val: array of double; nbvals: integer; redessine: boolean);
  var
  n, i: integer;
  ahx: double;
  begin
  ahx := (FNbBins - 1) / (max - min);
  for i := 0 to FNbBins - 1 do
  begin
  Valeurs[i] := 0;
  end;
  for i := 0 to nbvals do
  begin
  n := round(ahx * (val[i] - min));
  if n < 0 then
  n := 0;
  if n >= FNbBins then
  n := FNbBins - 1;
  inc(Valeurs[n]);
  end;
  minX := min;
  maxX := max;
  if (redessine) then
  begin
  Repaint();
  end;
  end;
}

procedure THistogramme.MajValeurs(min, max: integer; var val: array of uint16; nbvals: integer; redessine: boolean);
var
  n, i: integer;
  ahx: double;
begin
  ahx := (FNbBins - 1) / (max - min);
  for i := 0 to FNbBins - 1 do
  begin
    Valeurs[i] := 0;
  end;
  for i := 0 to nbvals - 1 do
  begin
    n := round(ahx * (val[i] - min));
    if (n >= 0) and (n < FNbBins) then
      inc(Valeurs[n]);
  end;
  minX := min;
  maxX := max;
  if (redessine) then
  begin
    Repaint();
  end;
end;

procedure THistogramme.MajValeurs(var val: array of uint16; nbvals: integer; redessine: boolean);
var
  i: integer;
begin
  minX := val[0];
  maxX := val[0];
  for i := 1 to nbvals - 1 do
  begin
    if val[i] < minX then
      minX := val[i];
    if val[i] > maxX then
      maxX := val[i];
  end;
  MajValeurs(round(minX), round(maxX), val, nbvals, redessine);
end;

procedure THistogramme.MajValeurs(min, max: integer; var val: array of integer; nbvals: integer; redessine: boolean);
var
  n, i: integer;
  ahx: double;
begin
  ahx := (FNbBins - 1) / (max - min);
  for i := 0 to FNbBins - 1 do
  begin
    Valeurs[i] := 0;
  end;
  for i := 0 to nbvals - 1 do
  begin
    n := round(ahx * (val[i] - min));
    if (n >= 0) and (n < FNbBins) then
      inc(Valeurs[n]);
  end;
  minX := min;
  maxX := max;
  if (redessine) then
  begin
    Repaint();
  end;
end;

procedure THistogramme.MajValeurs(var val: array of integer; nbvals: integer; redessine: boolean);
var
  i: integer;
begin
  minX := val[0];
  maxX := val[0];
  for i := 1 to nbvals - 1 do
  begin
    if val[i] < minX then
      minX := val[i];
    if val[i] > maxX then
      maxX := val[i];
  end;
  MajHisto(round(minX), round(maxX), val, nbvals, redessine);
end;

// ---------------------------------------------------------------------------

procedure THistogramme.MajHisto(min, max: double; var val: array of integer; lng: integer; redessine: boolean);
var
  i: integer;
begin
  if lng = FNbBins then
  begin
    for i := 0 to FNbBins - 1 do
    begin
      Valeurs[i] := val[i];
    end;
    minX := min;
    maxX := max;
    if (redessine) then
    begin
      Repaint();
    end;
  end;
end;

// ---------------------------------------------------------------------------
{$DEFINE GRADSIMPLE}

procedure THistogramme.Paint;
const
  marge: integer = 5;
var
  i, maxY: integer;
  stYmin, stYmax, stXmax, stXmin, stGrad: String;
  br: TBrush;
  rect: TRectF;
  HXtxt, LYtxt, HYtxt1, HYtxt0, LXtxt1, LXtxt0, Ay, Ax, Ox, Oy, By, Bx, cAx, cAy, x0, y0: Single;
  pol: TPolygon;
  x, y: Single;
  //p0, p1: TPointF;
  amplitude, grad, agrad: Single;
  ig: integer;
  valG, xGrad, Hgrad, Wgrad: Single;
  deci: double;
  ndeci, ng0, ng1: integer;
  limgradx: Single;

begin
  maxY := -1;
  for i := 0 to FNbBins - 1 do
  begin
    if Valeurs[i] > maxY then
      maxY := Valeurs[i];
  end;
  if maxY = 0 then
  begin
    maxY := 1;
  end;
  Canvas.BeginScene();
  stYmin := '0';
  stYmax := Format('%d', [maxY]);
  stXmin := Format(FormatX, [minX]);
  stXmax := Format(FormatX, [maxX]);

  // Fond transparent
  br := TBrush.Create(TBrushKind.Solid, 0);
  rect := TRectF.Create(0, 0, Width, Height);
  Canvas.FillRect(rect, 0, 0, AllCorners, 100, br);
  br.Free;

  HXtxt := Canvas.TextHeight(stXmin);
  if Canvas.TextHeight(stXmax) > HXtxt then
    HXtxt := Canvas.TextHeight(stXmax);

  LYtxt := Canvas.TextWidth(stYmin);
  if Canvas.TextWidth(stYmax) > LYtxt then
    LYtxt := Canvas.TextWidth(stYmax);

  HYtxt1 := Canvas.TextHeight(stYmax);
  HYtxt0 := Canvas.TextHeight(stYmin);
  LXtxt1 := Canvas.TextWidth(stXmax);
  LXtxt0 := Canvas.TextWidth(stXmin);

  Ay := marge + HYtxt1 / 2;
  Ax := marge + LYtxt + marge;
  Ox := Ax;
  Oy := Height - marge - HXtxt - marge;
  By := Oy;
  Bx := Width - marge - LXtxt1 / 2 - marge;

  cAx := (Bx - Ox) / (FNbBins - 1);
  cAy := (Oy - Ay) / maxY;
  x0 := Ox;
  y0 := Ay * Valeurs[0] + Oy;

  setLength(pol, FNbBins + 3);
  x := Ox;
  y := Oy;
  pol[0] := TPointF.Create(Ox, Oy);
  for i := 0 to FNbBins - 1 do
  begin
    y := Oy - cAy * Valeurs[i];
    pol[i + 1] := TPointF.Create(x, y);
    x := x + cAx;
  end;
  pol[FNbBins + 1] := TPointF.Create(x - cAx, By);
  pol[FNbBins + 2] := TPointF.Create(Ox, Oy);
  Canvas.Fill.Color := Stroke.Color;
  Canvas.FillPolygon(pol, 1);
  // Canvas.Fill.Color := claBlack;
  rect.Left := marge;
  rect.Top := marge;
  rect.Right := rect.Left + LYtxt;
  rect.Bottom := rect.Top + HYtxt1;
  Canvas.FillText(rect, stYmax, false, 1, [], TTextAlign.Center, TTextAlign.Center);
  rect.Left := marge;
  rect.Top := Oy - HYtxt0 / 2;
  rect.Right := marge + LYtxt;
  rect.Bottom := Oy - HYtxt0 / 2 + HYtxt0;
  Canvas.FillText(rect, stYmin, false, 1, [], TTextAlign.Leading, TTextAlign.Center);
  rect.Left := Ox - LXtxt0 / 2;
  rect.Top := Oy + marge;
  rect.Right := Ox + LXtxt0 / 2;
  rect.Bottom := Oy + marge + HXtxt;
  limgradx := rect.Right;
  Canvas.FillText(rect, stXmin, false, 1, [], TTextAlign.Center, TTextAlign.Center);
  rect.Left := Bx - LXtxt1 / 2;
  rect.Top := Oy + marge;
  rect.Right := Bx + LXtxt1 / 2;
  rect.Bottom := Oy + marge + HXtxt;
  Canvas.FillText(rect, stXmax, false, 1, [], TTextAlign.Center, TTextAlign.Center);
  // tracé des graduations:
{$IFDEF GRADSIMPLE}
  amplitude := (maxX - minX);
  if amplitude > 0 then
  begin
    grad := amplitude / 5;
    agrad := (Bx - Ox) / (amplitude);
    ig := 1;
    valG := ig * grad;
    xGrad := Ox + agrad * valG;
    stGrad := '';
    while (xGrad < Bx) do
    begin
      Canvas.DrawLine(TPointF.Create(xGrad, Oy), TPointF.Create(xGrad, Oy + 5), 100);
      stGrad := Format(FormatX, [round(valG) + minX]);
      Hgrad := Canvas.TextHeight(stGrad);
      Wgrad := Canvas.TextWidth(stGrad);
      Canvas.FillText(TRectF.Create(xGrad - Wgrad / 2, Oy + 5, xGrad + Wgrad / 2, Oy + 8 + Hgrad), stGrad, false, 1, [],
        TTextAlign.Center, TTextAlign.Center);
      inc(ig);
      valG := ig * grad;
      xGrad := Ox + agrad * valG;
    end;
  end;
{$ELSE}
  amplitude := (maxX - minX);
  if amplitude > 0 then
  begin

    deci := Math.Log10(amplitude);
    ndeci := trunc(deci);
    if deci = ndeci then
      ndeci := ndeci - 1;
    grad := Math.Power(10, ndeci);
    ng0 := trunc(abs(minX) / grad);
    if minX < 0 then
      ng0 := -ng0;
    ng1 := trunc(abs(maxX) / grad);
    if maxX < 0 then
      ng1 := -ng1;
    agrad := (Bx - Ox) / (amplitude);
    if ng0 * grad = minX then
      inc(ng0);
    if ng1 * grad = maxX then
      dec(ng1);

    for ig := ng0 to ng1 do
    begin
      valG := ig * grad;
      xGrad := Ox + agrad * (valG - minX);
      Canvas.DrawLine(TPointF.Create(xGrad, Oy), TPointF.Create(xGrad, Oy + 5), 100);
      stGrad := Format(FormatX, [round(valG) + minX]);
      Hgrad := Canvas.TextHeight(stGrad);
      Wgrad := Canvas.TextWidth(stGrad);
      if xGrad - Wgrad / 2 > limgradx then
      begin
        Canvas.FillText(TRectF.Create(xGrad - Wgrad / 2, Oy + 5, xGrad + Wgrad / 2, Oy + 8 + Hgrad), stGrad, false, 1,
          [], TTextAlign.Center, TTextAlign.Center);
        limgradx := xGrad + Wgrad / 2;
      end;
    end;
  end;
{$ENDIF}
  if MontreSeuils then
  begin
    if (SeuilMax > minX) and (SeuilMax < maxX) then
    begin
      Canvas.Stroke.Color := claRed;
      Canvas.Stroke.Thickness := 1;
      x := (SeuilMax - minX) * (Bx - Ox) / (maxX - minX) + Ox;
      Canvas.DrawLine(TPointF.Create(x, Oy),TPointF.Create(x, Ay), 1);
    end;

    if (SeuilMin > minX) and (SeuilMin < maxX) then
    begin
      Canvas.Stroke.Color := claBlue;
      Canvas.Stroke.Thickness := 1;
      x := (SeuilMin - minX) * (Bx - Ox) / (maxX - minX) + Ox;
      Canvas.DrawLine(TPointF.Create(x, Oy),TPointF.Create(x, Ay), 1);
    end;
  end;

  Canvas.EndScene();
end;

end.
