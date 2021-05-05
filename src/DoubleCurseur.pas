unit DoubleCurseur;

interface

uses
  System.SysUtils, Couleurs, System.Classes, System.Types, System.UIConsts, FMX.Types, FMX.Controls, FMX.Objects,
  FMX.Graphics, System.UITypes, System.Math.Vectors, TextControlTextSettings;

const
  Lcurs = 8;
  Lcurs2 = 4;

type
  TGenre = (Horizontal, Vertical);

  TDoubleCurseur = class(TRectangle)
  private

    cursEnMain: integer;
    FMax: integer;
    FMin: integer;
    deltaV: integer;
    FValeur1: integer;
    FValeur2: integer;
    FGenre: TGenre;
    function getPosCurseur(valcurs: integer): Single;
    function getValCurseur(x, y: Single): integer;
    function calculeDeltaV: integer;
    function getCurseurEnMain(x, y: Single): integer;
    procedure setMax(const Value: integer);
    procedure setMin(const Value: integer);

    procedure setValeur1(const Value: integer);
    procedure setValeur2(const Value: integer);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Single); override;
    procedure MouseMove(Shift: TShiftState; x, y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property Genre: TGenre read FGenre write FGenre;

    property Valeur1: integer read FValeur1 write setValeur1;
    property Valeur2: integer read FValeur2 write setValeur2;
    property Maximum: integer read FMax write setMax;
    property Minimum: integer read FMin write setMin;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TDoubleCurseur]);
end;

function TDoubleCurseur.calculeDeltaV: integer;
var
  fact: Single;
begin
  if FGenre = Vertical then
  begin
    fact := (FMax - FMin) / (Height - 16);
  end;

  if FGenre = Horizontal then
  begin
    fact := (FMax - FMin) / (Width - 16);
  end;
  result := round(9 * fact);
end;

constructor TDoubleCurseur.Create(AOwner: TComponent);
begin
  inherited;
  FValeur1 := 25;
  FValeur2 := 75;
  FGenre := Horizontal;
  FMax := 100;
  FMin := 0;
  Width := 200;
  Height := 32;
  deltaV := calculeDeltaV;
end;

function TDoubleCurseur.getCurseurEnMain(x, y: Single): integer;
var
  dpc1, dpc2, pc: Single;
  n: integer;
begin
  pc := getPosCurseur(FValeur1);
  case Genre of
    Horizontal:
      dpc1 := abs(pc - x);
    Vertical:
      dpc1 := abs(pc - y);
  end;
  pc := getPosCurseur(FValeur2);
  case Genre of
    Horizontal:
      dpc2 := abs(pc - x);
    Vertical:
      dpc2 := abs(pc - y);
  end;
  n := 0;
  if (dpc2 < dpc1) and (dpc2 < Lcurs) then
    n := 2;
  if (dpc1 < dpc2) and (dpc1 < Lcurs) then
    n := 1;
  result := n;
end;

function TDoubleCurseur.getPosCurseur(valcurs: integer): Single;
var
  pc, L: Single;
begin
  case Genre of
    Horizontal:
      L := Width - 2 * Lcurs;
    Vertical:
      L := Height - 2 * Lcurs;
  end;
  pc := Lcurs + L * (valcurs - FMin) / (FMax - FMin);
  result := pc;
end;

function TDoubleCurseur.getValCurseur(x, y: Single): integer;
var
  pc, L: Single;
  n: integer;
begin
  case Genre of
    Horizontal:
      begin
        L := Width - 2 * Lcurs;
        pc := x;
      end;
    Vertical:
      begin
        L := Height - 2 * Lcurs;
        pc := y;
      end;
  end;
  n := round((pc - Lcurs) / L * (FMax - FMin) + FMin);
  result := n;
end;

procedure TDoubleCurseur.MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: Single);
begin
  // SELECTION DU CURSEUR
  cursEnMain := getCurseurEnMain(x, y);
end;

procedure TDoubleCurseur.MouseMove(Shift: TShiftState; x, y: Single);
var
  nindx: integer;
begin
  nindx := getValCurseur(x, y);
  if cursEnMain = 1 then
    setValeur1(nindx);
  if cursEnMain = 2 then
    setValeur2(nindx);
end;

procedure TDoubleCurseur.MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Single);
var
  nindx: integer;
begin
  nindx := getValCurseur(x, y);
  if cursEnMain = 1 then
    setValeur1(nindx);
  if cursEnMain = 2 then
    setValeur2(nindx);
  cursEnMain := 0;
end;

procedure TDoubleCurseur.Paint;
var
  Xc: Single;
  Yc: Single;

begin

  Canvas.BeginScene;
  Canvas.Fill.Color := 0;
  Canvas.FillRect(TRectF.Create(0, 0, Width, Height), 0, 0, AllCorners, 1, TCornerType.round);
  if FGenre = Vertical then
  begin
    // Fond de glissière
    Canvas.Fill.Color := ClaGray;
    Canvas.FillRect(TRectF.Create(Width / 4, 0, 3 * Width / 4, Height), 0, 0, AllCorners, 1, TCornerType.round);
    Canvas.Fill.Color := ClaBlack;
    Canvas.FillRect(TRectF.Create(Width * (1 / 4 + 1 / 6), Lcurs, Width * (3 / 4 - 1 / 6), Height - Lcurs), 0, 0,
      AllCorners, 1, TCornerType.round);
    // Dessin des curseurs
    Yc := getPosCurseur(FValeur1);
    Canvas.Fill.Color := ClaGray;
    Canvas.FillRect(TRectF.Create(0, Yc - Lcurs, Width, Yc + Lcurs), 0, 0, AllCorners, 1, TCornerType.round);
    Canvas.Fill.Color := ClaBlue;
    Canvas.FillRect(TRectF.Create(lCurs2, Yc - Lcurs2, Width-Lcurs2, Yc + Lcurs2), 0, 0, AllCorners, 1, TCornerType.round);
    Yc := getPosCurseur(FValeur2);
    Canvas.Fill.Color := ClaGray;
    Canvas.FillRect(TRectF.Create(0, Yc - Lcurs, Width, Yc + Lcurs), 0, 0, AllCorners, 1, TCornerType.round);
    Canvas.Fill.Color := ClaRed;
    Canvas.FillRect(TRectF.Create(lCurs2, Yc - Lcurs2, Width-Lcurs2, Yc + Lcurs2), 0, 0, AllCorners, 1, TCornerType.round);
  end;

  if FGenre = Horizontal then
  begin
    // Fond de glissière
    Canvas.Fill.Color := ClaGray;
    Canvas.FillRect(TRectF.Create(0, Height / 4, Width, 3 * Height / 4), 0, 0, AllCorners, 1, TCornerType.round);
    Canvas.Fill.Color := ClaBlack;
    Canvas.FillRect(TRectF.Create(Lcurs, Height * (1 / 4 + 1 / 6), Width - Lcurs, Height * (3 / 4 - 1 / 6)), 0, 0,
      AllCorners, 1, TCornerType.round);
    // Dessin des curseurs
    Xc := getPosCurseur(FValeur1);
    Canvas.Fill.Color := ClaGray;
    Canvas.FillRect(TRectF.Create(Xc - Lcurs, 0, Xc + Lcurs, Height), 0, 0, AllCorners, 1, TCornerType.round);
    Canvas.Fill.Color := ClaBlue;
    Canvas.FillRect(TRectF.Create(Xc - Lcurs2, Lcurs2, Xc + Lcurs2, Height - Lcurs2), 0, 0, AllCorners, 1,
      TCornerType.round);
    Xc := getPosCurseur(FValeur2);
    Canvas.Fill.Color := ClaGray;
    Canvas.FillRect(TRectF.Create(Xc - Lcurs, 0, Xc + Lcurs, Height), 0, 0, AllCorners, 1, TCornerType.round);
    Canvas.Fill.Color := ClaRed;
    Canvas.FillRect(TRectF.Create(Xc - Lcurs2, Lcurs2, Xc + Lcurs2, Height - Lcurs2), 0, 0, AllCorners, 1,
      TCornerType.round);
  end;
  Canvas.EndScene;
end;

procedure TDoubleCurseur.setMax(const Value: integer);
begin
  if (Value >= FMin) then
  begin
    FMax := Value;
    deltaV := calculeDeltaV;
    if FValeur2 > FMax then
      FValeur2 := FMax;
    if FValeur1 > FMax then
    begin
      FValeur1 := FValeur2 - deltaV;
    end;
  end;
end;

procedure TDoubleCurseur.setMin(const Value: integer);
begin
  if (Value <= FMax) then
  begin
    FMin := Value;
  end;

end;

procedure TDoubleCurseur.setValeur1(const Value: integer);
begin
  if (Value >= FMin) and (Value <= FMax) and (Value < FValeur2 - deltaV) then
  begin
    FValeur1 := Value;
    repaint;
  end;
end;

procedure TDoubleCurseur.setValeur2(const Value: integer);
begin
  if (Value >= FMin) and (Value <= FMax) and (Value > FValeur1 + deltaV) then
  begin
    FValeur2 := Value;
    repaint;
  end;
end;

end.
