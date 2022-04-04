unit Boussole;

interface

uses
  System.SysUtils, System.Classes, System.Types, FMX.Types, FMX.Controls, FMX.Objects, FMX.Graphics, System.UITypes,
  System.UIConsts, System.Math.Vectors, System.Math, TextControlTextSettings;

type
  TGenre = (AiguilleTournante, CadranTournant);

  TBoussole = class(TRectangle)
  private
    FValeur, FGraduationMajeure, FGraduationMineure: single;
    FMontreGraduationMajeure, FMontreGraduationMineure: boolean;
    FEpaisseurBordure, FLgGrdMaj, FLgGrdMin: integer;
    FGenre: TGenre;
    FTextSettingsInfo: TTextSettingsInfo;

    procedure SetEpaisseurBordure(Value: integer);
    procedure SetLgGrdMaj(Value: integer);
    procedure SetLgGrdMin(Value: integer);
    procedure SetValeur(Value: single);
    procedure SetGraduationMajeure(Value: single);
    procedure SetGraduationMineure(Value: single);
    procedure SetMontreGraduationMajeure(Value: boolean);
    procedure SetMontreGraduationMineure(Value: boolean);
    function GetDefaultTextSettings: TTextSettings;
    function GetTextSettings: TTextSettings;
    function GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass;
    procedure SetTextSettings(const Value: TTextSettings);
  protected
    { Déclarations protégées }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    /// <summary>Stores a TTextSettings type object keeping the default values of the text representation properties</summary>
    property DefaultTextSettings: TTextSettings read GetDefaultTextSettings;

  published
    property Genre: TGenre read FGenre write FGenre;
    property EpaisseurBordure: integer read FEpaisseurBordure write SetEpaisseurBordure;
    property LongueurGraduationMajeure: integer read FLgGrdMaj write SetLgGrdMaj;
    property LongueurGraduationMineure: integer read FLgGrdMin write SetLgGrdMin;
    property Valeur: single read FValeur write SetValeur;
    property GraduationMajeure: single read FGraduationMajeure write SetGraduationMajeure;
    property GraduationMineure: single read FGraduationMineure write SetGraduationMineure;
    property MontreGraduationMineure: boolean read FMontreGraduationMineure write SetMontreGraduationMineure;
    property MontreGraduationMajeure: boolean read FMontreGraduationMajeure write SetMontreGraduationMajeure;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TBoussole]);
end;

procedure TBoussole.Paint;
var
  rect: TRectF;

  st: String;
  Wtxt, Htxt: single;
  Xc, Yc, Xe, Ye: single;
  xalpha, yalpha, alpha: single;
  br: TBrush;

  aiguille: TPolygon;
  i, j: integer;
  dalpha, grad: double;
  tailleF, x, y, x1, x2, y1, y2: single;
  nbgradMaj, nbgradMin: integer;
  xecr, yecr: single;
  rayon, rtxt, rbord: single;
  bmpEcr: TBitmap;
  M: TMatrix;
begin

  Canvas.BeginScene;

  // Fond transparent
  br := TBrush.Create(TBrushKind.Solid, 0);
  rect := TRectF.Create(0, 0, Width, Height);
  Canvas.FillRect(rect, 0, 0, AllCorners, 100, br);
  br.Free;

  rayon := Width;
  if Height < rayon then
    rayon := Height;
  rayon := rayon / 2;
  rbord := rayon - FEpaisseurBordure;
  // Calcul de la hauteur et de la largeur max des étiquettes
  Canvas.Font.Family := TextSettings.Font.Family;
  Canvas.Font.Size := TextSettings.Font.Size;
  Canvas.Font.Style := TextSettings.Font.Style;

  Wtxt := Canvas.TextWidth('000');
  rtxt := (rayon - FLgGrdMaj) - Wtxt;

  Xc := Width / 2;
  Yc := Height / 2;
  Canvas.Fill.Color := claWhite;
  Canvas.FillEllipse(TRectF.Create(Xc - rayon, Yc - rayon, Xc + rayon, Yc + rayon), 1);
  Canvas.Fill.Color := claBlack;
  Canvas.FillEllipse(TRectF.Create(Xc - rbord, Yc - rbord, Xc + rbord, Yc + rbord), 1);
  if FGraduationMajeure > 0 then
    nbgradMaj := trunc(360 / FGraduationMajeure)
  else
    nbgradMaj := 0;
  if FGraduationMineure > 0 then
    nbgradMin := trunc(FGraduationMajeure / FGraduationMineure)
  else
    nbgradMin := 0;
  if nbgradMaj > 0 then
  begin
    for i := 0 to nbgradMaj - 1 do
    begin
      if FGenre = CadranTournant then
        alpha := DegToRad(i * FGraduationMajeure - FValeur)
      else
        alpha := DegToRad(i * FGraduationMajeure);
      xalpha := sin(alpha);
      yalpha := -cos(alpha);
      bmpEcr := TBitmap.Create;
      bmpEcr.Width := round(Wtxt * 2);
      bmpEcr.Height := round(Wtxt * 2);
      bmpEcr.Canvas.BeginScene;
      br := TBrush.Create(TBrushKind.Solid, claBlack);
      bmpEcr.Canvas.Font.Family := TextSettings.Font.Family;
      bmpEcr.Canvas.Font.Size := TextSettings.Font.Size;
      bmpEcr.Canvas.Font.Style := TextSettings.Font.Style;

      bmpEcr.Canvas.FillRect(TRectF.Create(0, 0, bmpEcr.Width, bmpEcr.Height), 0, 0, AllCorners, 100, br);
      br.Free;
      M := bmpEcr.Canvas.Matrix;
      M := M * TMatrix.CreateTranslation(-Wtxt, -Wtxt);
      M := M * TMatrix.CreateRotation(alpha);
      M := M * TMatrix.CreateTranslation(Wtxt, Wtxt);
      bmpEcr.Canvas.SetMatrix(M);

      bmpEcr.Canvas.Fill.Color := TextSettings.FontColor;
      st := Format('%.3d', [round(i * FGraduationMajeure)]);
      bmpEcr.Canvas.FillText(TRectF.Create(0, 0, 2 * Wtxt, 2 * Wtxt), st, false, 1, [], TTextAlign.Center,
        TTextAlign.Center);

      bmpEcr.Canvas.EndScene;
      // Calcul des valeurs centre rayon amplitude angulaire pour le genre par défaut
      // et dessin du fond du cadran
      Xe := Xc + rtxt * xalpha;
      Ye := Yc + rtxt * yalpha;

      Canvas.DrawBitmap(bmpEcr, TRectF.Create(0, 0, bmpEcr.Width, bmpEcr.Height),
        TRectF.Create(Xe - bmpEcr.Width / 2, Ye - bmpEcr.Height / 2, Xe + bmpEcr.Width / 2, Ye + bmpEcr.Height / 2),
        1, false);
      FreeAndNil(bmpEcr);
      Canvas.Stroke.Color := claWhite;
      if MontreGraduationMajeure then
      begin
        x1 := rayon * xalpha + Xc;
        y1 := rayon * yalpha + Yc;
        x2 := (rayon - LongueurGraduationMajeure) * xalpha + Xc;
        y2 := (rayon - LongueurGraduationMajeure) * yalpha + Yc;
        Canvas.DrawLine(TPointF.Create(x1, y1), TPointF.Create(x2, y2), 1);
        if (MontreGraduationMineure) and (GraduationMineure > 0) then
        begin
          for j := 1 to nbgradMin - 1 do
          begin
            if FGenre = CadranTournant then
              alpha := DegToRad(i * FGraduationMajeure - FValeur + FGraduationMineure * j)
            else
              alpha := DegToRad(i * FGraduationMajeure + FGraduationMineure * j);
            xalpha := sin(alpha);
            yalpha := -cos(alpha);
            x1 := rayon * xalpha + Xc;
            y1 := rayon * yalpha + Yc;
            x2 := (rayon - LongueurGraduationMineure) * xalpha + Xc;
            y2 := (rayon - LongueurGraduationMineure) * yalpha + Yc;
            Canvas.DrawLine(TPointF.Create(x1, y1), TPointF.Create(x2, y2), 1);
          end;
        end;
      end;
    end;

  end;
  if FGenre = CadranTournant then
  begin
    tailleF := Canvas.Font.Size;
    Canvas.Font.Size := tailleF * 2;
    st := Format('%.3d', [round(FValeur)]);
    Wtxt := Canvas.TextWidth(st);
    Htxt := Canvas.TextHeight(st);
    Canvas.Fill.Color := claWhite;
    Canvas.FillText(TRectF.Create(Xc - Wtxt / 2, Yc - Htxt / 2, Xc + Wtxt / 2, Yc + Htxt / 2), st, false, 1, [],
      TTextAlign.Center, TTextAlign.Center);
    Canvas.Font.Size := tailleF;
  end
  else
  begin
    setLength(aiguille, 4);
    alpha := DegToRad(FValeur);

    aiguille[0] := TPointF.Create(Xc + (rayon - FLgGrdMaj) * sin(alpha), Yc - (rayon - FLgGrdMaj) * cos(alpha));
    alpha := DegToRad(FValeur + 90);
    aiguille[1] := TPointF.Create(Xc + rayon / 8 * sin(alpha), Yc - rayon / 8 * cos(alpha));
    alpha := DegToRad(FValeur - 90);
    aiguille[2] := TPointF.Create(Xc + rayon / 8 * sin(alpha), Yc - rayon / 8 * cos(alpha));
    aiguille[3] := aiguille[0];
    Canvas.Fill.Color := claRed;
    Canvas.FillPolygon(aiguille, 1);
    alpha := DegToRad(FValeur + 180);
    aiguille[0] := TPointF.Create(Xc + (rayon - FLgGrdMaj) * sin(alpha), Yc - (rayon - FLgGrdMaj) * cos(alpha));
    aiguille[3] := aiguille[0];
    Canvas.Fill.Color := claWhite;
    Canvas.FillPolygon(aiguille, 1);
  end;
  Canvas.EndScene;

end;

constructor TBoussole.Create(AOwner: TComponent);
begin
  inherited;
  FGraduationMajeure := 45;
  FGraduationMineure := 5;
  FMontreGraduationMajeure := true;
  FMontreGraduationMineure := true;
  FValeur := 0;
  FEpaisseurBordure := 4;
  FLgGrdMaj := 16;
  FLgGrdMin := 8;
  FGenre := CadranTournant;
  FTextSettingsInfo := TTextSettingsInfo.Create(Self, GetTextSettingsClass);
end;

procedure TBoussole.SetEpaisseurBordure(Value: integer);
begin
  FEpaisseurBordure := Value;
end;

procedure TBoussole.SetValeur(Value: single);
begin
  if FValeur <> Value then
  begin
    FValeur := Value;
    Repaint;
  end;
end;

procedure TBoussole.SetGraduationMajeure(Value: single);
begin
  FGraduationMajeure := Value;
end;

procedure TBoussole.SetGraduationMineure(Value: single);
begin
  FGraduationMineure := Value;
end;

procedure TBoussole.SetLgGrdMaj(Value: integer);
begin
  FLgGrdMaj := Value;
end;

procedure TBoussole.SetLgGrdMin(Value: integer);
begin
  FLgGrdMin := Value;
end;

procedure TBoussole.SetMontreGraduationMajeure(Value: boolean);
begin
  FMontreGraduationMajeure := Value;
end;

procedure TBoussole.SetMontreGraduationMineure(Value: boolean);
begin
  FMontreGraduationMineure := Value;
end;

function TBoussole.GetDefaultTextSettings: TTextSettings;
begin
  Result := FTextSettingsInfo.DefaultTextSettings;
end;

function TBoussole.GetTextSettings: TTextSettings;
begin
  Result := FTextSettingsInfo.TextSettings;
end;

procedure TBoussole.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettingsInfo.TextSettings.Assign(Value);
end;

function TBoussole.GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass;
begin
  Result := TTextControlTextSettings;
end;


// initialization

// FMX.Types.GlobalUseGPUCanvas := true;

end.
