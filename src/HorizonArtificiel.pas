unit HorizonArtificiel;

interface

uses
  System.SysUtils, System.Classes, System.Types, FMX.Types, FMX.Controls,
  FMX.Objects, FMX.Graphics, System.UITypes, TextControlTextSettings,
  System.UIConsts, System.Math.Vectors;

const
  // angle maximal en roulis/tangage pour les échelles rayon du cercle = anglemax
  anglemax = 30;

type

  THorizonArtificiel = class(TRectangle)
  private
    FEpaisseurBordure: integer;
    FRoulis, Ftangage: double;
    FTextSettingsInfo: TTextSettingsInfo;

    function GetDefaultTextSettings: TTextSettings;
    function GetTextSettings: TTextSettings;
    function GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass;
    procedure SetTextSettings(const Value: TTextSettings);
    procedure SetRoulis(Value: double);
    procedure SetTangage(Value: double);
    function getTangage: double;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure setRoulisTangage(rl, tg: double);
  published
    property Roulis: double read FRoulis write SetRoulis;
    property Tangage: double read getTangage write SetTangage;
    property EpaisseurBordure: integer read FEpaisseurBordure write FEpaisseurBordure;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
  end;

procedure Register;

implementation

uses
  System.Math;

procedure Register;
begin
  RegisterComponents('VisuLabo', [THorizonArtificiel]);
end;

{ THorizonArtificiel }

constructor THorizonArtificiel.Create(AOwner: TComponent);
begin
  inherited;
  FEpaisseurBordure := 10;
  FRoulis := 0;
  Ftangage := 0;
  FTextSettingsInfo := TTextSettingsInfo.Create(Self, GetTextSettingsClass);
end;

procedure THorizonArtificiel.Paint;
var
  rayon, xc, yc, xh, yh: single;
  ptO, ptA, ptB, vectU, vectV: TPointf;
  l1, l2, dlt, b, c: double;
  pol: TPolygon;
  alpha0, alpha1, dalpha: Extended;
  i: integer;
  nbpnts: integer;
  lg: double;
  mrg: double;
  Wtxt: single;
  vr: integer;
  serr: string;

  function toAlpha(x, y: double): double;
  var
    alpha: double;

  begin
    if x = 0 then
    begin
      if y > 0 then
        alpha := PI / 2
      else
      begin
        if y < 0 then
          alpha := 3 * PI / 2
        else
          alpha := 0;
      end;
    end
    else
    begin
      alpha := arctan(y / x);
      if x < 0 then
        alpha := alpha + PI
      else
      begin
        if y < 0 then
          alpha := alpha + 2 * PI;
      end;
    end;
    result := alpha;
  end;
{$IFDEF VERSION1}
  procedure ecrit(valeur: integer; angle: double; pGau, pDRt: TPointf);
  var
    bmpEcr: TBitmap;
    br: TBrush;
    st: string;
    M: TMatrix;
    ptE: TPointf;
  begin
    bmpEcr := TBitmap.Create(round(Wtxt * 2), round(Wtxt * 2));
    bmpEcr.Canvas.BeginScene;
    br := TBrush.Create(TBrushKind.Solid, 0);
    bmpEcr.Canvas.Font.Family := TextSettings.Font.Family;
    bmpEcr.Canvas.Font.Size := TextSettings.Font.Size;
    bmpEcr.Canvas.Font.Style := TextSettings.Font.Style;
    bmpEcr.Canvas.FillRect(TRectF.Create(0, 0, bmpEcr.Width, bmpEcr.Height), 0, 0, AllCorners, 100, br);
    br.Free;
    M := bmpEcr.Canvas.Matrix;
    M := M * TMatrix.CreateTranslation(-Wtxt, -Wtxt);
    M := M * TMatrix.CreateRotation(angle);
    M := M * TMatrix.CreateTranslation(Wtxt, Wtxt);
    bmpEcr.Canvas.SetMatrix(M);

    bmpEcr.Canvas.Fill.Color := TextSettings.FontColor;
    st := Format('%d', [valeur]);
    bmpEcr.Canvas.FillText(TRectF.Create(0, 0, 2 * Wtxt, 2 * Wtxt), st, false, 1, [], TTextAlign.Center, TTextAlign.Center);
    bmpEcr.Canvas.EndScene;
    ptE := TPointf.Create(pGau.x - Wtxt / 2 * vectU.x, pGau.y - Wtxt / 2 * vectU.y);
    Canvas.DrawBitmap(bmpEcr, TRectF.Create(0, 0, bmpEcr.Width, bmpEcr.Height),
      TRectF.Create(ptE.x - bmpEcr.Width / 2, ptE.y - bmpEcr.Height / 2, ptE.x + bmpEcr.Width / 2, ptE.y + bmpEcr.Height / 2),
      1, false);
    ptE := TPointf.Create(pDRt.x + Wtxt / 2 * vectU.x, pDRt.y + Wtxt / 2 * vectU.y);
    Canvas.DrawBitmap(bmpEcr, TRectF.Create(0, 0, bmpEcr.Width, bmpEcr.Height),
      TRectF.Create(ptE.x - bmpEcr.Width / 2, ptE.y - bmpEcr.Height / 2, ptE.x + bmpEcr.Width / 2, ptE.y + bmpEcr.Height / 2),
      1, false);
    FreeAndNil(bmpEcr);
  end;
{$ENDIF}
  procedure ecritT(valeur: integer; angle: double; pGau, pDRt: TPointf);
  var
    bmpEcr: TBitmap;
    st: string;
    ptE: TPointf;
  begin
    bmpEcr := TBitmap.Create(round(Wtxt * 2), round(Wtxt * 2));
    bmpEcr.Clear(0);
    bmpEcr.Canvas.BeginScene;
    bmpEcr.Canvas.Font.Family := TextSettings.Font.Family;
    bmpEcr.Canvas.Font.Size := TextSettings.Font.Size;
    bmpEcr.Canvas.Font.Style := TextSettings.Font.Style;
    bmpEcr.Canvas.Fill.Color := TextSettings.FontColor;

    st := Format('%d', [valeur]);
    bmpEcr.Canvas.FillText(TRectF.Create(0, 0, 2 * Wtxt, 2 * Wtxt), st, false, 1, [], TTextAlign.Center, TTextAlign.Center);
    bmpEcr.Canvas.EndScene;
    bmpEcr.Rotate(angle / PI * 180);
    ptE := TPointf.Create(pGau.x - Wtxt / 2 * vectU.x, pGau.y - Wtxt / 2 * vectU.y);
    Canvas.DrawBitmap(bmpEcr, TRectF.Create(0, 0, bmpEcr.Width, bmpEcr.Height),
      TRectF.Create(ptE.x - bmpEcr.Width / 2, ptE.y - bmpEcr.Height / 2, ptE.x + bmpEcr.Width / 2, ptE.y + bmpEcr.Height / 2),
      1, false);
    ptE := TPointf.Create(pDRt.x + Wtxt / 2 * vectU.x, pDRt.y + Wtxt / 2 * vectU.y);
    Canvas.DrawBitmap(bmpEcr, TRectF.Create(0, 0, bmpEcr.Width, bmpEcr.Height),
      TRectF.Create(ptE.x - bmpEcr.Width / 2, ptE.y - bmpEcr.Height / 2, ptE.x + bmpEcr.Width / 2, ptE.y + bmpEcr.Height / 2),
      1, false);
    FreeAndNil(bmpEcr);
  end;
  procedure ecritR(valeur: integer; angle: double);
  var
    bmpEcr: TBitmap;
    st: string;
    ptE: TPointf;
  begin
    bmpEcr := TBitmap.Create(round(Wtxt * 2), round(Wtxt * 2));
    bmpEcr.Clear(0);
    bmpEcr.Canvas.BeginScene;
    bmpEcr.Canvas.Font.Family := TextSettings.Font.Family;
    bmpEcr.Canvas.Font.Size := TextSettings.Font.Size;
    bmpEcr.Canvas.Font.Style := TextSettings.Font.Style;
    bmpEcr.Canvas.Fill.Color := TextSettings.FontColor;

    st := Format('%d', [valeur]);
    bmpEcr.Canvas.FillText(TRectF.Create(0, 0, 2 * Wtxt, 2 * Wtxt), st, false, 1, [], TTextAlign.Center, TTextAlign.Center);
    bmpEcr.Canvas.EndScene;
    bmpEcr.Rotate(90 + angle / PI * 180);
    ptE.x := ptO.x + (rayon + mrg) * cos(angle);
    ptE.y := ptO.y + (rayon + mrg) * sin(angle);

    Canvas.DrawBitmap(bmpEcr, TRectF.Create(0, 0, bmpEcr.Width, bmpEcr.Height),
      TRectF.Create(ptE.x - bmpEcr.Width / 2, ptE.y - bmpEcr.Height / 2, ptE.x + bmpEcr.Width / 2, ptE.y + bmpEcr.Height / 2),
      1, false);
    FreeAndNil(bmpEcr);
  end;

begin
  inherited;
  Canvas.BeginScene;
  // Fond transparent
  Canvas.FillRect(TRectF.Create(0, 0, Width, Height), 0, 0, AllCorners, 100, TBrush.Create(TBrushKind.Solid, $FF000000));
  if Width > Height then
    rayon := Height / 2 - 2 * FEpaisseurBordure
  else
    rayon := Width / 2 - 2 * FEpaisseurBordure;
  dalpha := arcSin(2 / rayon);
  mrg := rayon / 20;
  Canvas.Font.Family := TextSettings.Font.Family;
  Canvas.Font.Size := TextSettings.Font.Size;
  Canvas.Font.Style := TextSettings.Font.Style;

  Wtxt := Canvas.TextWidth('000');

  // Canvas.FillText(TRectF.Create(0, 0, 2 * Wtxt, 2 * Wtxt), 'toto', false, 1, [], TTextAlign.Center, TTextAlign.Center);
  // calcul de la position de la limite air/sol
  vectU := TPointf.Create(cos(FRoulis * PI / 180), sin(FRoulis * PI / 180));
  vectV := TPointf.Create(vectU.y, -vectU.x);
  xc := Width / 2;
  yc := Height / 2;
  xh := xc + vectV.x * rayon / anglemax * Ftangage;
  yh := yc + vectV.y * rayon / anglemax * Ftangage;
  b := 2 * (vectU.x * (xh - xc) + vectU.y * (yh - yc));
  c := xh * xh - 2 * xh * xc + xc * xc + yh * yh - 2 * yh * yc + yc * yc - rayon * rayon;
  dlt := b * b - 4 * c;
  if dlt >= 0 then
  begin
    l1 := (-b - sqrt(dlt)) / 2;
    l2 := (-b + sqrt(dlt)) / 2;
    ptA := TPointf.Create(xh + vectU.x * l1, yh + vectU.y * l1);
    ptB := TPointf.Create(xh + vectU.x * l2, yh + vectU.y * l2);

    if l1 >= 0 then
    begin
      alpha0 := toAlpha(ptA.x - xc, ptA.y - yc);
      alpha1 := toAlpha(ptB.x - xc, ptB.y - yc);
    end
    else
    begin
      alpha0 := toAlpha(ptB.x - xc, ptB.y - yc);
      alpha1 := toAlpha(ptA.x - xc, ptA.y - yc);
    end;

    // dessin du sol
    if alpha0 > alpha1 then
    begin
      nbpnts := round(((2 * PI - alpha0) + alpha1) / dalpha) + 2;
    end
    else
    begin
      nbpnts := round((alpha1 - alpha0) / dalpha) + 2;
    end;

    setlength(pol, nbpnts);
    for i := 0 to nbpnts - 2 do
    begin
      pol[i] := TPointf.Create(xc + rayon * cos(alpha0 + i * dalpha), yc + rayon * sin(alpha0 + i * dalpha));
    end;
    pol[nbpnts - 1] := pol[0];
    Canvas.Fill.Color := claBrown;
    Canvas.FillPolygon(pol, 1);

    // dessin du ciel
    if alpha0 > alpha1 then
    begin
      nbpnts := round((alpha0 - alpha1) / dalpha) + 2;
    end
    else
    begin
      nbpnts := round(((2 * PI - alpha1) + alpha0) / dalpha) + 2;
    end;
    if nbpnts > 2 then
    begin

      setlength(pol, nbpnts);
      for i := 0 to nbpnts - 2 do
      begin
        pol[i] := TPointf.Create(xc + rayon * cos(alpha1 + i * dalpha), yc + rayon * sin(alpha1 + i * dalpha));
      end;
      pol[nbpnts - 1] := pol[0];
      Canvas.Fill.Color := claBlue;
      Canvas.FillPolygon(pol, 1);
    end;
    Canvas.Stroke.Color := claBlack;
    Canvas.DrawEllipse(TRectF.Create(xc - rayon, yc - rayon, xc + rayon, yc + rayon), 1);
    Canvas.Stroke.Color := claYellow;
    Canvas.DrawEllipse(TRectF.Create(xc - 8, yc - 8, xc + 8, yc + 8), 1);
  end
  else
  begin

  end;
  // dessin graduation tangages
  Canvas.Stroke.Color := claWhite;
  ptO := TPointf.Create(xc, yc);
  dalpha := 5 * rayon / anglemax;
  for i := 1 to 6 do
  begin
    lg := rayon / 8;
    if i mod 2 = 0 then
      lg := rayon / 4;
    ptA.x := xh + vectV.x * i * dalpha - lg * vectU.x;
    ptA.y := yh + vectV.y * i * dalpha - lg * vectU.y;
    if ptO.Distance(ptA) < rayon then
    begin
      ptB.x := xh + vectV.x * i * dalpha + lg * vectU.x;
      ptB.y := yh + vectV.y * i * dalpha + lg * vectU.y;
      if ptO.Distance(ptB) < rayon - lg - mrg then
      begin
        Canvas.DrawLine(ptA, ptB, 1);
        if i mod 2 = 0 then
          ecritT(i * 5, FRoulis / 180 * PI, ptA, ptB);
      end;
    end;

    ptA.x := xh - vectV.x * i * dalpha - lg * vectU.x;
    ptA.y := yh - vectV.y * i * dalpha - lg * vectU.y;
    if ptO.Distance(ptA) < rayon then
    begin
      ptB.x := xh - vectV.x * i * dalpha + lg * vectU.x;
      ptB.y := yh - vectV.y * i * dalpha + lg * vectU.y;
      if ptO.Distance(ptB) < rayon - lg - mrg then
      begin
        Canvas.DrawLine(ptA, ptB, 1);
        if i mod 2 = 0 then
          ecritT(-i * 5, FRoulis / 180 * PI, ptA, ptB);
      end;
    end;
  end;
  lg := rayon / 3;
  ptA.x := xh - lg * vectU.x;
  ptA.y := yh - lg * vectU.y;
  if ptO.Distance(ptA) < rayon then
  begin
    ptB.x := xh + lg * vectU.x;
    ptB.y := yh + lg * vectU.y;
    if ptO.Distance(ptB) < rayon - lg - mrg then
    begin
      Canvas.DrawLine(ptA, ptB, 1);
      ecritT(0, FRoulis / 180 * PI, ptA, ptB);
    end;
  end;

  // dessin graduation roulis

  alpha0 := 210 / 180 * PI;
  dalpha := 5 / 180 * PI;
  vr := -60;
  for i := 0 to 24 do
  begin
    if i mod 2 = 0 then
    begin
      lg := 2 * mrg;
      ecritR(vr, alpha0 + i * dalpha);
    end
    else
      lg := mrg;
    ptA.x := ptO.x + (rayon - mrg) * cos(alpha0 + i * dalpha);
    ptB.x := ptO.x + (rayon - mrg - lg) * cos(alpha0 + i * dalpha);
    ptA.y := ptO.y + (rayon - mrg) * sin(alpha0 + i * dalpha);
    ptB.y := ptO.y + (rayon - mrg - lg) * sin(alpha0 + i * dalpha);
    Canvas.DrawLine(ptA, ptB, 1);
    vr := vr + 5;
  end;
  setlength(pol, 4);
  lg := rayon / 10;
  pol[0] := TPointf.Create(xc + vectV.x * (rayon - 3 * mrg), yc + vectV.y * (rayon - 3 * mrg));
  pol[1] := TPointf.Create(xc + vectV.x * (rayon - 3 * mrg - lg) - vectU.x * mrg, yc + vectV.y * (rayon - 3 * mrg - lg) -
    vectU.y * mrg);
  pol[2] := TPointf.Create(xc + vectV.x * (rayon - 3 * mrg - lg) + vectU.x * mrg, yc + vectV.y * (rayon - 3 * mrg - lg) +
    vectU.y * mrg);
  pol[3] := pol[0];
  Canvas.Fill.Color := claYellow;
  Canvas.FillPolygon(pol, 1);
  Canvas.EndScene;
end;

procedure THorizonArtificiel.SetRoulis(Value: double);
begin
  if FRoulis <> Value then
  begin
    FRoulis := Value;
    Repaint;
  end;

end;

procedure THorizonArtificiel.setRoulisTangage(rl, tg: double);
begin
  if (Ftangage <> -tg) or (FRoulis <> rl) then
  begin
    Ftangage := -tg;
    FRoulis := rl;
    Repaint;
  end;
end;

procedure THorizonArtificiel.SetTangage(Value: double);
begin
  if Ftangage <> -Value then
  begin
    Ftangage := -Value;
    Repaint;
  end;

end;

function THorizonArtificiel.GetDefaultTextSettings: TTextSettings;
begin
  result := FTextSettingsInfo.DefaultTextSettings;
end;

procedure THorizonArtificiel.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettingsInfo.TextSettings.Assign(Value);
end;

function THorizonArtificiel.getTangage: double;
begin
  result := -Ftangage;
end;

function THorizonArtificiel.GetTextSettings: TTextSettings;
begin
  result := FTextSettingsInfo.TextSettings;
end;

function THorizonArtificiel.GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass;
begin
  result := TTextControlTextSettings;
end;

end.
