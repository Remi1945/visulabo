unit Radar;

interface

uses
  System.SysUtils, Couleurs, TextControlTextSettings, System.Classes,
  System.Types, FMX.Types, FMX.Controls, Math.Vectors,
  System.Generics.Collections,
  FMX.Graphics, FMX.Objects, System.UITypes, System.UIConsts, PointInteret;

type

  TRadar = class(TRectangle)
  private

    FcoulRayons: TCouls;
    FcoulCercles: TCouls;
    FcoulDir: TCouls;
    FcoulVisee: TCouls;
    FViseeClaire, FMontrePI, FMontreVise, FMontreDir: boolean;
    FTextSettingsInfo: TTextSettingsInfo;
    Fsurligne: string;
    listePI: TDictionary<String, TPi>;
    FNbCercles, FNbSecteurs: integer;
    Fdir, Fvisee, FdeltaVisee: Single;
    FCercleH: integer;

    procedure SetNbCercles(nb: integer);
    procedure SetNbSecteurs(nb: integer);
    procedure SetCercleH(nb: integer);
    procedure SetSurlignage(id: string);
    procedure SetDirection(ndir: Single);
    procedure SetVisee(ndir: Single);
    function GetDefaultTextSettings: TTextSettings;
    function GetTextSettings: TTextSettings;
    function GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass;
    procedure SetTextSettings(const Value: TTextSettings);

  protected
    { Déclarations protégées }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure AjoutePI(id: string; xPi: TPi; redessine: boolean);
    procedure EffacePI(redessine: boolean);
  published

    property CouleurRayon: TCouls read FcoulRayons write FcoulRayons;
    property CouleurCercles: TCouls read FcoulCercles write FcoulCercles;
    property CouleurDirection: TCouls read FcoulDir write FcoulDir;
    property CouleurLimitesVisee: TCouls read FcoulVisee write FcoulVisee;
    property Direction: Single read Fdir write SetDirection;
    property Visee: Single read Fvisee write SetVisee;
    property LargeurVisee: Single read FdeltaVisee write FdeltaVisee;
    property AffichePI: boolean read FMontrePI write FMontrePI;
    property Affichedir: boolean read FMontreDir write FMontreDir;
    property AfficheVisee: boolean read FMontreVise write FMontreVise;
    property ViseeClaire: boolean read FViseeClaire write FViseeClaire;
    property TextSettings: TTextSettings read GetTextSettings
      write SetTextSettings;
    property Surlignage: string read Fsurligne write SetSurlignage;
    property NbSecteurs: integer read FNbSecteurs write SetNbSecteurs;
    property NbCercles: integer read FNbCercles write SetNbCercles;
    property CerclePrincipal: integer read FCercleH write SetCercleH;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TRadar]);
end;

constructor TRadar.Create(AOwner: TComponent);
var
  i: integer;

begin
  inherited;
  FMontrePI := false;
  FMontreDir := false;
  FMontreVise := false;
  FcoulRayons := Orange;
  FcoulCercles := Vert;
  FcoulDir := Rouge;
  FcoulVisee := Jaune;
  FNbCercles := 4;
  FNbSecteurs := 8;
  FCercleH := 0;
  FViseeClaire := true;
  listePI := TDictionary<String, TPi>.Create;
  FTextSettingsInfo := TTextSettingsInfo.Create(Self, GetTextSettingsClass);
  Fsurligne := '';
end;

procedure TRadar.EffacePI(redessine: boolean);
var
  xPi: TPi;
  key: string;
begin
  for key in listePI.keys do
  begin
    listePI.TryGetValue(key, xPi);
    listePI.Remove(key);
    xPi.Free;
  end;

  if redessine then
    Repaint;
end;

procedure TRadar.Paint;
const
  marge = 4;
  Xfleche: array [0 .. 7] of Single = (-2, 0, 2, 1, 1, -1, -1, -2);
  Yfleche: array [0 .. 7] of Single = (-1, -3, -1, -1, 3, 3, -1, -1);
var
  i, j: integer;
  st: String;
  wt, ht: Single;
  rmax, dr, ll: Single;
  ds, dx, dy: Single;
  rect: TRectF;
  x1, y1: Single;
  ux, uy, vx, vy: Single;
  Xc, Yc, xPi, ypi, alpha, alpha0, dalpha: Single;
  a, b, c: integer;
  lePI: TPi;
  poly: TPolygon;
  bmp: TBitmap;
  br: TBrush;
  nbangle: integer;
  key: String;
  oldCoul: TAlphaColor;

  procedure ValToPoint(alpha, dist: Single; var px, py: Single);
  var
    d: Single;
  begin
    d := dist * rmax;
    px := d * sin(alpha) + Xc;
    py := Yc - d * cos(alpha);

  end;

begin

  Canvas.Font.Family := TextSettings.Font.Family;
  Canvas.Font.Size := TextSettings.Font.Size;
  Canvas.Font.Style := TextSettings.Font.Style;
  Xc := Width / 2;
  Yc := Height / 2;
  rmax := Width / 2;
  if Height / 2 < rmax then
    rmax := Height / 2;
  Canvas.BeginScene();
  // Canvas.Clear(0);
  br := TBrush.Create(TBrushKind.Solid, 0);
  rect := TRectF.Create(0, 0, Width, Height);
  Canvas.FillRect(rect, 0, 0, AllCorners, 100, br);
  br.Free;

  Canvas.Fill := Fill;

  Canvas.FillEllipse(TRectF.Create(Xc - rmax, Yc - rmax, Xc + rmax,
    Yc + rmax), 1);

  if FNbSecteurs > 0 then
  begin
    Canvas.Stroke.Color := setCoul(FcoulRayons);
    ds := 2 * PI / FNbSecteurs;
    for i := 0 to FNbSecteurs - 1 do
    begin
      x1 := rmax * sin(ds * i) + Xc;
      y1 := Yc - rmax * cos(ds * i);
      Canvas.DrawLine(TPointF.Create(Xc, Yc), TPointF.Create(x1, y1), 1);
    end;
  end;
  if FNbCercles > 0 then
  begin
    Canvas.Stroke.Color := setCoul(FcoulCercles);
    dr := rmax / FNbCercles;
    for i := 1 to FNbCercles do
    begin
      if FCercleH = i then
      begin
        Canvas.Stroke.SetCustomDash([12,5,5,5],0);
        Canvas.Stroke.Dash := TStrokeDash.Custom;

        end
      else
        Canvas.Stroke.Dash := TStrokeDash.Solid;
      Canvas.DrawEllipse(TRectF.Create(Xc - dr * i, Yc - dr * i, Xc + dr * i,
        Yc + dr * i), 1);
    end;
  end;
  if FMontrePI then
  begin
    for key in listePI.keys do
    begin
      listePI.TryGetValue(key, lePI);
      st := lePI.montexte;
      wt := Canvas.TextWidth(st);
      ht := Canvas.TextHeight(st);
      ValToPoint(lePI.X, lePI.Y, xPi, ypi);
      Canvas.Fill.Color := lePI.Couleur;
      Canvas.FillText(TRectF.Create(xPi + lePI.taille, ypi - lePI.taille,
        xPi + wt + lePI.taille, ypi + ht - lePI.taille), st, false, 1, [],
        TTextAlign.Center, TTextAlign.Center);

      Canvas.Stroke.Color := lePI.Couleur;
      case lePI.maForme of
        FrmCarre:
          begin
            Canvas.DrawRect(TRectF.Create(xPi - lePI.taille / 2,
              ypi - lePI.taille / 2, xPi + lePI.taille / 2,
              ypi + lePI.taille / 2), 0, 0, AllCorners, 1);
            if Fsurligne = key then
            begin
              Canvas.FillRect(TRectF.Create(xPi - lePI.taille / 2,
                ypi - lePI.taille / 2, xPi + lePI.taille / 2,
                ypi + lePI.taille / 2), 0, 0, AllCorners, 1);
            end;
          end;
        FrmCercle:
          begin
            Canvas.DrawEllipse(TRectF.Create(xPi - lePI.taille / 2,
              ypi - lePI.taille / 2, xPi + lePI.taille / 2,
              ypi + lePI.taille / 2), 1);
            if Fsurligne = key then
            begin
              Canvas.FillEllipse(TRectF.Create(xPi - lePI.taille / 2,
                ypi - lePI.taille / 2, xPi + lePI.taille / 2,
                ypi + lePI.taille / 2), 1);
            end;
          end;
        FrmX:
          begin
            Canvas.DrawLine(TPointF.Create(xPi - lePI.taille / 2,
              ypi - lePI.taille / 2), TPointF.Create(xPi + lePI.taille / 2,
              ypi + lePI.taille / 2), 1);
            Canvas.DrawLine(TPointF.Create(xPi + lePI.taille / 2,
              ypi - lePI.taille / 2), TPointF.Create(xPi - lePI.taille / 2,
              ypi + lePI.taille / 2), 1);
            if Fsurligne = key then
            begin
              Canvas.DrawEllipse(TRectF.Create(xPi - lePI.taille / 4,
                ypi - lePI.taille / 4, xPi + lePI.taille / 4,
                ypi + lePI.taille / 4), 1);

            end;
          end;
        FrmCroix:
          begin
            Canvas.DrawLine(TPointF.Create(xPi, ypi - lePI.taille / 2),
              TPointF.Create(xPi, ypi + lePI.taille / 2), 1);
            Canvas.DrawLine(TPointF.Create(xPi + lePI.taille / 2, ypi),
              TPointF.Create(xPi - lePI.taille / 2, ypi), 1);
            if Fsurligne = key then
            begin
              Canvas.DrawEllipse(TRectF.Create(xPi - lePI.taille / 4,
                ypi - lePI.taille / 4, xPi + lePI.taille / 4,
                ypi + lePI.taille / 4), 1);

            end;
          end;

      end;
    end;
  end;

  if FMontreVise then
  begin
    // bmp := TBitmap.Create(round(Width), round(Height));
    // bmp.Canvas.BeginScene;
    // br := TBrush.Create(TBrushKind.Solid, 0);
    // rect := TRectF.Create(0, 0, Width, Height);
    // bmp.Canvas.FillRect(rect, 0, 0, AllCorners, 100, br);
    // br.Free;
    alpha0 := Fvisee - FdeltaVisee;
    dalpha := 2 * FdeltaVisee / 128;
    setLength(poly, 128 + 3);
    if FViseeClaire then
      Canvas.Fill.Color := claWhite
    else
      Canvas.Fill.Color := claBlack;
    poly[0] := TPointF.Create(Xc, Yc);
    poly[130] := TPointF.Create(Xc, Yc);
    for i := 0 to 128 do
    begin
      alpha := (alpha0 + dalpha * i) / 180 * PI;
      poly[i + 1] := TPointF.Create(rmax * sin(alpha) + Xc,
        Yc - rmax * cos(alpha));
    end;
    Canvas.FillPolygon(poly, 0.5);
    Canvas.Stroke.Color := setCoul(FcoulVisee);
    Canvas.Stroke.Dash := TStrokeDash.Dash;

    x1 := rmax * sin((Fvisee + FdeltaVisee) / 180 * PI) + Xc;
    y1 := Yc - rmax * cos((Fvisee + FdeltaVisee) / 180 * PI);

    Canvas.DrawLine(TPointF.Create(Xc, Yc), TPointF.Create(x1, y1), 1);
    x1 := rmax * sin((Fvisee - FdeltaVisee) / 180 * PI) + Xc;
    y1 := Yc - rmax * cos((Fvisee - FdeltaVisee) / 180 * PI);

    Canvas.DrawLine(TPointF.Create(Xc, Yc), TPointF.Create(x1, y1), 1);
  end;

  if FMontreDir then
  begin
    ll := rmax / 40;
    ux := -ll * sin(Fdir / 180 * PI);
    uy := ll * cos(Fdir / 180 * PI);
    vx := -uy;
    vy := ux;
    setLength(poly, 8);
    for i := 0 to 7 do
    begin
      poly[i] := TPointF.Create(Xc + Xfleche[i] * vx + Yfleche[i] * ux,
        Yc + Xfleche[i] * vy + Yfleche[i] * uy);
    end;
    oldCoul := Canvas.Fill.Color;
    Canvas.Fill.Color := setCoul(FcoulDir);
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.FillPolygon(poly, 1);
    Canvas.Fill.Color := oldCoul;
  end;

  Canvas.EndScene();
end;

// ---------------------------------------------------------------------------

procedure TRadar.SetSurlignage(id: string);
begin
  if listePI.ContainsKey(id) then
    Fsurligne := id
  else
    Fsurligne := '';
  Repaint;
end;

procedure TRadar.SetCercleH(nb: integer);
begin
  FCercleH := nb;
  Repaint;
end;

procedure TRadar.SetDirection(ndir: Single);
begin
  Fdir := ndir;
  while Fdir < 0 do
    Fdir := Fdir + 360;
  while Fdir > 360 do
    Fdir := Fdir - 360;
  Repaint;
end;

procedure TRadar.SetNbCercles(nb: integer);
begin
  FNbCercles := nb;
end;

procedure TRadar.SetNbSecteurs(nb: integer);
begin
  FNbSecteurs := nb;
end;

// ---------------------------------------------------------------------------
procedure TRadar.AjoutePI(id: string; xPi: TPi; redessine: boolean);
begin
  listePI.TryAdd(id, xPi);
  if redessine then
    Repaint;
end;

function TRadar.GetDefaultTextSettings: TTextSettings;
begin
  result := FTextSettingsInfo.DefaultTextSettings;
end;

function TRadar.GetTextSettings: TTextSettings;
begin
  result := FTextSettingsInfo.TextSettings;
end;

procedure TRadar.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettingsInfo.TextSettings.Assign(Value);
end;

procedure TRadar.SetVisee(ndir: Single);
begin
  Fvisee := ndir;
  while Fvisee < 0 do
    Fvisee := Fvisee + 360;
  while Fvisee > 360 do
    Fvisee := Fvisee - 360;
  Repaint;
end;

function TRadar.GetTextSettingsClass
  : TTextSettingsInfo.TCustomTextSettingsClass;
begin
  result := TTextControlTextSettings;
end;

end.
