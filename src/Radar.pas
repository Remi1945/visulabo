unit Radar;

interface

uses
  System.SysUtils, Couleurs, TextControlTextSettings, System.Classes,
  System.Types, FMX.Types, FMX.Controls,
  FMX.Graphics, FMX.Objects, System.UITypes, System.UIConsts, PointInteret;

type

  TRadar = class(TRectangle)
  private

    FcoulRayons: TCouls;
    FcoulCercles: TCouls;
    FMontrePI: boolean;
    FTextSettingsInfo: TTextSettingsInfo;
    Fsurligne: integer;
    listePI: TList;
    FNbCercles, FNbSecteurs: integer;

    procedure SetNbCercles(nb: integer);
    procedure SetNbSecteurs(nb: integer);
    procedure SetSurlignage(num: integer);
    function GetDefaultTextSettings: TTextSettings;
    function GetTextSettings: TTextSettings;
    function GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass;
    procedure SetTextSettings(const Value: TTextSettings);

  protected
    { Déclarations protégées }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;

    procedure AjoutePI(xPi: TPi; redessine: boolean);
    procedure EffacePI(redessine: boolean);
  published

    property CouleurRayon: TCouls read FcoulRayons write FcoulRayons;
    property CouleurCercles: TCouls read FcoulCercles write FcoulCercles;

    property AffichePI: boolean read FMontrePI write FMontrePI;
    property TextSettings: TTextSettings read GetTextSettings
      write SetTextSettings;
    property Surlignage: integer read Fsurligne write SetSurlignage;
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

  FcoulRayons := Orange;
  FcoulCercles := Vert;
  FNbCercles := 4;
  FNbSecteurs := 8;
  listePI := TList.Create;
  FTextSettingsInfo := TTextSettingsInfo.Create(Self, GetTextSettingsClass);
  Fsurligne := -1;

end;

procedure TRadar.EffacePI(redessine: boolean);
var
  xPi: TPi;
begin
  while listePI.Count > 0 do
  begin
    xPi := listePI.Items[0];
    listePI.Delete(0);
    xPi.Free;
  end;

  if redessine then
    Repaint;
end;

procedure TRadar.Paint;
const
  marge = 4;
var
  i, j: integer;
  st: String;
  wt, ht: Single;
  rmax, dr: Single;
  ds, dx, dy: Single;
  rect: TRectF;
  x1, y1: Single;
  Xc, Yc, xPi, ypi: Single;
  a, b, c: integer;
  lePI: TPi;

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
    Canvas.Stroke.Color := SetCoul(FcoulCercles);
    dr := rmax / FNbCercles;
    for i := 1 to FNbCercles do
    begin

      Canvas.DrawEllipse(TRectF.Create(Xc - dr * i, Yc - dr * i, Xc + dr * i,
        Yc + dr * i), 1);

    end;
  end;
  // tracé de l'axe

  if FMontrePI then
  begin
    for i := 0 to listePI.Count - 1 do
    begin
      lePI := TPi(listePI[i]);
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
            if Fsurligne = i then
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
            if Fsurligne = i then
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
            if Fsurligne = i then
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
            if Fsurligne = i then
            begin
              Canvas.DrawEllipse(TRectF.Create(xPi - lePI.taille / 4,
                ypi - lePI.taille / 4, xPi + lePI.taille / 4,
                ypi + lePI.taille / 4), 1);

            end;
          end;

      end;
    end;
  end;
  // Ecriture des textes

  Canvas.EndScene();
end;

// ---------------------------------------------------------------------------

procedure TRadar.SetSurlignage(num: integer);
begin
  if (num >= 0) and (num < listePI.Count) then
    Fsurligne := num
  else
    Fsurligne := -1;
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
procedure TRadar.AjoutePI(xPi: TPi; redessine: boolean);
begin
  listePI.Add(xPi);
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

function TRadar.GetTextSettingsClass
  : TTextSettingsInfo.TCustomTextSettingsClass;
begin
  result := TTextControlTextSettings;
end;

end.
