unit Affichage7Seg;

interface

uses
  System.SysUtils, System.Classes, System.Types, FMX.Types, FMX.Controls, FMX.Objects, FMX.Graphics, System.UITypes,
  System.UIConsts, System.Math.Vectors,
  Data.Bind.Components,System.Bindings.Outputs, System.rtti
  ,Couleurs;

type

  [ObservableMember('Valeur')]
  TAffichage7Seg = class(TRectangle)
  private
    FcoulBORD, FcoulFOND, FcoulON, FcoulOFF: TCouls;
    FEpaisseurBordure: integer;
    FNbDigits: integer;
    FValeur: string;
    procedure SetEpaisseurBordure(Value: integer);
    procedure SetNbDigits(Value: integer);
    procedure SetValeur(Value: string);
  protected
    { Déclarations protégées }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property CouleurBORD: TCouls read FcoulBORD write FcoulBORD;
    property CouleurFOND: TCouls read FcoulFOND write FcoulFOND;
    property CouleurON: TCouls read FcoulON write FcoulON;
    property CouleurOFF: TCouls read FcoulOFF write FcoulOFF;
    property EpaisseurBordure: integer read FEpaisseurBordure write SetEpaisseurBordure;
    property NombreDeDigits: integer read FNbDigits write SetNbDigits;
    property Valeur: String read FValeur write SetValeur;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TAffichage7Seg]);
end;

constructor TAffichage7Seg.Create(AOwner: TComponent);
begin
  inherited;
  FValeur := '0';
  FNbDigits := 1;
  FEpaisseurBordure := 4;
  FcoulBORD:=Gris;
  FcoulFOND:=Noir;
  FcoulON:=Rouge;
  FcoulOFF:=Transparent;
end;

procedure TAffichage7Seg.Paint;
var
  segs: array [1 .. 7] of TPolygon;
  decimale, minute, seconde: TRectF;
  rect: TRectF;
  k, i, j: integer;
  a, b, c, d, w7, h7, wd, wmax: single;
  deltax, ox, oy: single;
  coulFOND, coulBORD, coulON, coulOFF: TAlphaColor;
  br: TBrush;
  nbseg: integer;


begin

  coulON := setCoul(FcoulON);
  coulOFF := setCoul(FcoulOFF);
  coulFOND := setCoul(FcoulFOND);
  if FcoulBORD <> Custom then
    coulBORD := setCoul(FcoulBORD);
  h7 := Height - 2 * FEpaisseurBordure - 8;
  a := h7 * 10 / 34;
  b := a / 3;
  c := 4 * a / 30;
  w7 := a + 2 * b + 2 * c;
  wd := w7 * FNbDigits + b * (FNbDigits - 1);
  wmax := Width - 2 * FEpaisseurBordure;
  while wd >= wmax do
  begin
    h7 := h7 - 1;
    a := h7 * 10 / 34;
    b := a / 3;
    c := 4 * a / 30;
    w7 := a + 2 * b + 2 * c;
    wd := w7 * FNbDigits + b * (FNbDigits - 1);
  end;
  ox := (Width - wd) / 2;
  oy := (Height - h7) / 2;
  for k := 1 to 7 do
    setLength(segs[k], 7);

  segs[1][0] := TPointF.Create(b / 2 + c, b / 2);
  segs[1][1] := TPointF.Create(b + c, 0);
  segs[1][2] := TPointF.Create(b + c + a, 0);
  segs[1][3] := TPointF.Create(3 * b / 2 + c + a, b / 2);
  segs[1][4] := TPointF.Create(b + c + a, b);
  segs[1][5] := TPointF.Create(b + c, b);
  segs[1][6] := TPointF.Create(b / 2 + c, b / 2);
  segs[2][0] := TPointF.Create(b / 2, b / 2 + c);
  segs[2][1] := TPointF.Create(b, b + c);
  segs[2][2] := TPointF.Create(b, b + c + a);
  segs[2][3] := TPointF.Create(b / 2, 3 * b / 2 + c + a);
  segs[2][4] := TPointF.Create(0, b + c + a);
  segs[2][5] := TPointF.Create(0, b + c);
  segs[2][6] := TPointF.Create(b / 2, b / 2 + c);
  d := b + 2 * c + a;
  for i := 0 to 6 do
  begin
    segs[4][i] := TPointF.Create(segs[1][i].X, segs[1][i].Y + d);
    segs[7][i] := TPointF.Create(segs[1][i].X, segs[1][i].Y + 2 * d);
    segs[3][i] := TPointF.Create(segs[2][i].X + d, segs[2][i].Y);
    segs[6][i] := TPointF.Create(segs[2][i].X + d, segs[2][i].Y + d);
    segs[5][i] := TPointF.Create(segs[2][i].X, segs[2][i].Y + d);
  end;
  // décalage par rapport à l'origine
  for i := 1 to 7 do
  begin
    for j := 0 to 6 do
    begin
      segs[i][j].X := segs[i][j].X + ox;
      segs[i][j].Y := segs[i][j].Y + oy;
    end;
  end;
  // création du pont décimal
  decimale := TRectF.Create(segs[6][2].X, segs[7][1].Y, segs[6][2].X + b, segs[7][1].Y + b);
  minute := TRectF.Create(segs[3][1].X, segs[1][1].Y, segs[3][1].X + b / 3, segs[1][1].Y + b);
  seconde := TRectF.Create(segs[3][1].X + 2 * b / 3, segs[1][1].Y, segs[3][1].X + b, segs[1][1].Y + b);
  Canvas.BeginScene;
  // dessin du cadre
  if FcoulBORD = Custom then
    Canvas.Fill := Fill
  else
    Canvas.Fill.Color := coulBORD;
  Canvas.FillRect(TRectF.Create(0, 0, Width, Height), 0, 0, AllCorners, 1);
  Canvas.Fill.Color := coulFOND;
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.FillRect(TRectF.Create(EpaisseurBordure, EpaisseurBordure, Width - EpaisseurBordure,
    Height - EpaisseurBordure), 0, 0, AllCorners, 1);
  nbseg := 0;
  k := 1;
  while k <= length(FValeur) do
  begin
    if (FValeur[k] in ['0' .. '9']) and (nbseg < FNbDigits) then
    begin
      if FValeur[k] = '0' then
      begin // 123567 allumé 4 éteint
        Canvas.Fill.Color := coulON;
        Canvas.FillPolygon(segs[1], 1);
        Canvas.FillPolygon(segs[2], 1);
        Canvas.FillPolygon(segs[3], 1);
        Canvas.FillPolygon(segs[5], 1);
        Canvas.FillPolygon(segs[6], 1);
        Canvas.FillPolygon(segs[7], 1);
        Canvas.Fill.Color := coulOFF;
        Canvas.FillPolygon(segs[4], 1);
      end;
      if FValeur[k] = '1' then
      begin // 36 allumé 12457 éteint
        Canvas.Fill.Color := coulON;
        Canvas.FillPolygon(segs[3], 1);
        Canvas.FillPolygon(segs[6], 1);
        Canvas.Fill.Color := coulOFF;
        Canvas.FillPolygon(segs[4], 1);
        Canvas.FillPolygon(segs[1], 1);
        Canvas.FillPolygon(segs[2], 1);
        Canvas.FillPolygon(segs[5], 1);
        Canvas.FillPolygon(segs[7], 1);
      end;
      if FValeur[k] = '2' then
      begin // 123457 allumé 26 éteint
        Canvas.Fill.Color := coulON;
        Canvas.FillPolygon(segs[1], 1);
        Canvas.FillPolygon(segs[3], 1);
        Canvas.FillPolygon(segs[4], 1);
        Canvas.FillPolygon(segs[5], 1);
        Canvas.FillPolygon(segs[7], 1);
        Canvas.Fill.Color := coulOFF;
        Canvas.FillPolygon(segs[2], 1);
        Canvas.FillPolygon(segs[6], 1);
      end;
      if FValeur[k] = '3' then
      begin // 13467 allumé 25 éteint
        Canvas.Fill.Color := coulON;
        Canvas.FillPolygon(segs[1], 1);
        Canvas.FillPolygon(segs[4], 1);
        Canvas.FillPolygon(segs[3], 1);
        Canvas.FillPolygon(segs[6], 1);
        Canvas.FillPolygon(segs[7], 1);
        Canvas.Fill.Color := coulOFF;
        Canvas.FillPolygon(segs[5], 1);
        Canvas.FillPolygon(segs[2], 1);
      end;
      if FValeur[k] = '4' then
      begin // 2346 allumé 157 éteint
        Canvas.Fill.Color := coulON;
        Canvas.FillPolygon(segs[4], 1);
        Canvas.FillPolygon(segs[2], 1);
        Canvas.FillPolygon(segs[3], 1);
        Canvas.FillPolygon(segs[6], 1);
        Canvas.Fill.Color := coulOFF;
        Canvas.FillPolygon(segs[1], 1);
        Canvas.FillPolygon(segs[5], 1);
        Canvas.FillPolygon(segs[7], 1);
      end;
      if FValeur[k] = '5' then
      begin // 12467 allumé 35 éteint
        Canvas.Fill.Color := coulON;
        Canvas.FillPolygon(segs[4], 1);
        Canvas.FillPolygon(segs[2], 1);
        Canvas.FillPolygon(segs[1], 1);
        Canvas.FillPolygon(segs[7], 1);
        Canvas.FillPolygon(segs[6], 1);
        Canvas.Fill.Color := coulOFF;
        Canvas.FillPolygon(segs[3], 1);
        Canvas.FillPolygon(segs[5], 1);
      end;
      if FValeur[k] = '6' then
      begin // 124567 allumé 3 éteint
        Canvas.Fill.Color := coulON;
        Canvas.FillPolygon(segs[4], 1);
        Canvas.FillPolygon(segs[2], 1);
        Canvas.FillPolygon(segs[1], 1);
        Canvas.FillPolygon(segs[7], 1);
        Canvas.FillPolygon(segs[6], 1);
        Canvas.FillPolygon(segs[5], 1);
        Canvas.Fill.Color := coulOFF;
        Canvas.FillPolygon(segs[3], 1);
      end;
      if FValeur[k] = '7' then
      begin // 136 allumé 2457 éteint
        Canvas.Fill.Color := coulON;
        Canvas.FillPolygon(segs[1], 1);
        Canvas.FillPolygon(segs[3], 1);
        Canvas.FillPolygon(segs[6], 1);
        Canvas.Fill.Color := coulOFF;
        Canvas.FillPolygon(segs[5], 1);
        Canvas.FillPolygon(segs[2], 1);
        Canvas.FillPolygon(segs[4], 1);
        Canvas.FillPolygon(segs[7], 1);
      end;
      if FValeur[k] = '8' then
      begin // 1234567 allumé
        Canvas.Fill.Color := coulON;
        Canvas.FillPolygon(segs[1], 1);
        Canvas.FillPolygon(segs[2], 1);
        Canvas.FillPolygon(segs[3], 1);
        Canvas.FillPolygon(segs[5], 1);
        Canvas.FillPolygon(segs[6], 1);
        Canvas.FillPolygon(segs[7], 1);
        Canvas.FillPolygon(segs[4], 1);
      end;
      if FValeur[k] = '9' then
      begin // 123467 allumé 5 éteint
        Canvas.Fill.Color := coulON;
        Canvas.FillPolygon(segs[1], 1);
        Canvas.FillPolygon(segs[4], 1);
        Canvas.FillPolygon(segs[3], 1);
        Canvas.FillPolygon(segs[6], 1);
        Canvas.FillPolygon(segs[7], 1);
        Canvas.FillPolygon(segs[2], 1);
        Canvas.Fill.Color := coulOFF;
        Canvas.FillPolygon(segs[5], 1);
      end;
      for i := 1 to 7 do
      begin
        for j := 0 to 6 do
          segs[i][j].X := segs[i][j].X + w7 + b;
      end;
      inc(nbseg);
    end
    else
    begin
      if ((FValeur[k] = '.') or (FValeur[k] = ',') or (FValeur[k] = chr(39)) or (FValeur[k] = '"')) and (nbseg <= FNbDigits) then
      begin
        Canvas.Fill.Color := coulON;
        deltax := (nbseg-1) * (w7 + b);
        if (FValeur[k] = '.') or (FValeur[k] = ',') then
          Canvas.FillEllipse(TRectF.Create(decimale.Left + deltax, decimale.Top, decimale.Right + deltax,
            decimale.Bottom), 1);
        if FValeur[k] = chr(39) then
        begin
          Canvas.FillRect(TRectF.Create(minute.Left + deltax, minute.Top, minute.Right + deltax, minute.Bottom), 0, 0,
            AllCorners, 1);
        end;
        if FValeur[k] = '"' then
        begin
          Canvas.FillRect(TRectF.Create(minute.Left + deltax, minute.Top, minute.Right + deltax, minute.Bottom), 0, 0,
            AllCorners, 1);
          Canvas.FillRect(TRectF.Create(seconde.Left + deltax, seconde.Top, seconde.Right + deltax, seconde.Bottom), 0,
            0, AllCorners, 1);
        end;
      end;

    end;
    inc(k);
  end;
  Canvas.EndScene;
end;

procedure TAffichage7Seg.SetEpaisseurBordure(Value: integer);
begin
  FEpaisseurBordure := Value;
end;

procedure TAffichage7Seg.SetNbDigits(Value: integer);
begin
  FNbDigits := Value;
end;

procedure TAffichage7Seg.SetValeur(Value: string);
begin
  FValeur := Value;
  Repaint();
end;

initialization
// FMX.Types.GlobalUseGPUCanvas := true;

 Data.Bind.Components.RegisterObservableMember(TArray<TClass>.Create(TAffichage7Seg), 'Valeur', 'FMX');

finalization
 Data.Bind.Components.UnregisterObservableMember(TArray<TClass>.Create(TAffichage7Seg));
end.
