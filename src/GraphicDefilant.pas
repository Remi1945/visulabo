unit GraphicDefilant;

interface

uses
  System.SysUtils, System.Classes, System.Types, FMX.Types, FMX.Controls, FMX.Objects, System.UITypes, System.UIConsts;

type
  TGraphicDefilant = class(TRectangle)
  private
    tx: integer;
    FUnite: String;
    FFormatY: String;
    FTitre: String;
    Valeurs: array [0 .. 199] of double;
    FSeuilMax: double;
    FSeuilMin: double;
    FMontreSeuils: boolean;
    FEchelleAuto: boolean;
    FminY, FmaxY: double;

    procedure SetUnite(Value: String);
    procedure SetFormatY(Value: String);
    procedure SetSeuilMin(Value: double);
    procedure SetSeuilMax(Value: double);
    procedure SetMini(Value: double);
    procedure SetMaxi(Value: double);
    procedure SetEchelleAuto(Value: boolean);
    procedure SetMontreSeuils(Value: boolean);
    procedure SetTitre(Value: String);
  protected
    { Déclarations protégées }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure AjouteValeur(val: double; redessine: boolean); overload;
    procedure AjouteValeur(min, max, val: double; redessine: boolean); overload;
    function HorsLimites: integer;
  published
    property Unite: String read FUnite write SetUnite;
    property FormatY: String read FFormatY write SetFormatY;
    property SeuilMax: double read FSeuilMax write SetSeuilMax;
    property SeuilMin: double read FSeuilMin write SetSeuilMin;
    property MontreSeuils: boolean read FMontreSeuils write SetMontreSeuils;
    property EchelleMax: double read FmaxY write SetMaxi;
    property EchelleMin: double read FminY write SetMini;
    property EchelleAuto: boolean read FEchelleAuto write SetEchelleAuto;

    property Titre: String read FTitre write SetTitre;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TGraphicDefilant]);
end;

constructor TGraphicDefilant.Create(AOwner: TComponent);
var
  i: integer;

begin
  inherited;
  Unite := '';
  FFormatY := '%3.2f';
  FSeuilMax := 1.0;
  FSeuilMin := -1.0;
  FMontreSeuils := false;
  FEchelleAuto := true;
  FminY := 0;
  FmaxY := 1;
  for i := 0 to 199 do
    Valeurs[i] := i * 1.5 - 10;
end;

procedure TGraphicDefilant.Paint;
const
  marge = 4;
var
  i, j: integer;
  stYmin, stYmax, stXmin, stXmax, st: String;
  HXtxt, LYtxt: Single;
  HYtxt1, HYtxt0, LXtxt1, LXtxt0: Single;
  Ay, Ax, Ox, Oy, By, Bx: Single;
  x0, y0, x1, y1: Single;
  dx, dy: Single;
  rect: TRectF;
  p0, p1: TPointF;

begin
  p0 := TPointF.Create(0, 0);
  p1 := TPointF.Create(0, 0);
  rect := TRectF.Create(0, 0, 1, 1);

  stYmin := Format(FormatY, [FminY]) + Unite;
  stYmax := Format(FormatY, [FmaxY]) + Unite;
  stXmin := Format('%d', [tx]);
  stXmax := Format('%d', [tx + 199]);
  Canvas.BeginScene();

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

  dx := (Bx - Ox) / (200 - 1);
  dy := (Ay - Oy) / (FmaxY - FminY);
  x0 := Ox;
  y0 := Valeurs[0];
  if y0 < FminY then
    y0 := FminY;
  if y0 > FmaxY then
    y0 := FmaxY;

  y0 := dy * (y0 - FminY) + Oy;
  Canvas.Fill := Fill;
  rect.Left := Ax;
  rect.Top := Ay;
  rect.Right := Bx;
  rect.Bottom := By;
  Canvas.FillRect(rect, 0, 0, AllCorners, 100);
  Canvas.Stroke.Color := Stroke.Color;
  Canvas.Stroke.Thickness := 2;
  p0.SetLocation(x0, y0);
  for i := 1 to 199 do
  begin
    y1 := Valeurs[i];
    if y1 < FminY then
      y1 := FminY;
    if y1 > FmaxY then
      y1 := FmaxY;
    y1 := dy * (y1 - FminY) + Oy;
    x1 := i * dx + Ox;
    p1.SetLocation(x1, y1);
    Canvas.DrawLine(p0, p1, 1);
    p0.SetLocation(x1, y1);
  end;
  // Canvas.Fill.Color := claBlack;
  rect.Left := marge;
  rect.Top := marge;
  rect.Right := rect.Left + LYtxt;
  rect.Bottom := rect.Top + HYtxt1;
  Canvas.FillText(rect, stYmax, false, 1, [], TTextAlign.Center, TTextAlign.Center);

  rect.SetLocation(marge, Oy - HYtxt0 / 2);
  Canvas.FillText(rect, stYmin, false, 1, [], TTextAlign.Leading, TTextAlign.Center);

  rect.SetLocation(Ox - LXtxt0 / 2, Oy + marge);
  rect.Width := LXtxt0;
  Canvas.FillText(rect, stXmin, false, 1, [], TTextAlign.Center, TTextAlign.Center);
  rect.SetLocation(Bx - LXtxt1 / 2, Oy + marge);
  rect.Width := LXtxt1;
  Canvas.FillText(rect, stXmax, false, 1, [], TTextAlign.Center, TTextAlign.Center);
  st := FTitre + Format(FormatY, [Valeurs[199]]) + Unite;
  rect.Left := (Ox + Bx) / 2 - Canvas.TextWidth(st) / 2;
  rect.Right := rect.Left + Canvas.TextWidth(st);
  Canvas.FillText(rect, st, false, 1, [], TTextAlign.Center, TTextAlign.Center);
  if MontreSeuils then
  begin
    if (SeuilMax > FminY) and (SeuilMax < FmaxY) then
    begin
      Canvas.Stroke.Color := claRed;
      Canvas.Stroke.Thickness := 1;
      y1 := dy * (SeuilMax - FminY) + Oy;
      p0.SetLocation(Ox, y1);
      p1.SetLocation(Bx, y1);
      Canvas.DrawLine(p0, p1, 1);
    end;
    if (SeuilMin > FminY) and (SeuilMin < FmaxY) then
    begin
      Canvas.Stroke.Color := claBlue;
      Canvas.Stroke.Thickness := 1;
      y1 := dy * (SeuilMin - FminY) + Oy;
      p0.SetLocation(Ox, y1);
      p1.SetLocation(Bx, y1);
      Canvas.DrawLine(p0, p1, 1);
    end;
  end;
  Canvas.EndScene();
end;

// ---------------------------------------------------------------------------
procedure TGraphicDefilant.SetEchelleAuto(Value: boolean);
begin
  FEchelleAuto := Value;
end;

procedure TGraphicDefilant.SetFormatY(Value: String);
begin
  FFormatY := Value;
end;

// ---------------------------------------------------------------------------
procedure TGraphicDefilant.SetTitre(Value: String);
begin
  FTitre := Value;
end;

procedure TGraphicDefilant.SetUnite(Value: String);
begin
  FUnite := Value;
end;

// ---------------------------------------------------------------------------
procedure TGraphicDefilant.SetSeuilMin(Value: double);
begin
  FSeuilMin := Value;
end;

// ---------------------------------------------------------------------------
procedure TGraphicDefilant.SetSeuilMax(Value: double);
begin
  FSeuilMax := Value;
end;

// ---------------------------------------------------------------------------
procedure TGraphicDefilant.SetMaxi(Value: double);
begin
  FmaxY := Value;
end;

procedure TGraphicDefilant.SetMini(Value: double);
begin
  FminY := Value;
end;

procedure TGraphicDefilant.SetMontreSeuils(Value: boolean);
begin
  FMontreSeuils := Value;
end;

// ---------------------------------------------------------------------------
procedure TGraphicDefilant.AjouteValeur(val: double; redessine: boolean);
var
  i: integer;
  maxi, mini: double;
begin
  if FEchelleAuto then
  begin
    maxi := val;
    mini := val;
    for i := 1 to 199 do
    begin
      if Valeurs[i] < mini then
        mini := Valeurs[i];
      if Valeurs[i] > maxi then
        maxi := Valeurs[i];
    end;
    if mini = maxi then
    begin
      if mini = 0 then
      begin
        mini := -1;
        maxi := 1;
      end
      else
      begin
        mini := 0.9 * mini;
        maxi := 1.1 * maxi;
      end;
    end;
    AjouteValeur(mini, maxi, val, redessine);
  end
  else
    AjouteValeur(FminY, FmaxY, val, redessine);
end;

procedure TGraphicDefilant.AjouteValeur(min, max, val: double; redessine: boolean);
var
  i: integer;
begin
  FminY := min;
  FmaxY := max;
  for i := 0 to 198 do
  begin
    Valeurs[i] := Valeurs[i + 1];
  end;
  Valeurs[199] := val;
  inc(tx);
  if redessine then
    Repaint;
end;
// initialization

// FMX.Types.GlobalUseGPUCanvas := true;
function TGraphicDefilant.HorsLimites: integer;
begin
  if Valeurs[199] < FSeuilMin then
    result := -1
  else if Valeurs[199] > FSeuilMax then
    result := 1
  else
    result := 0;
end;

end.
