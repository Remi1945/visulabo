unit Thermometre;

interface

uses
  System.SysUtils, System.Classes, System.Types, FMX.Types, FMX.Controls, FMX.Objects, FMX.Graphics, System.UITypes,
  System.UIConsts, System.Math.Vectors;

type

  TThermometre = class(TRectangle)
  private
    FUnite,FEtiquette, FFormatValeurs: String;
    FMaxi, FMini, FValeur, FSeuil_A, FSeuil_B, FGraduationMajeure, FGraduationMineure: double;
    FMontreGraduationMajeure, FMontreGraduationMineure, FMontreValeurs, FMontreSeuils: boolean;
    FEpaisseurTube, FLgGrdMaj, FLgGrdMin: integer;

    procedure SetFormatValeurs(Value: String);
    procedure SetEtiquette(Value: String);
    procedure SetUnite(Value: String);
    procedure SetEpaisseurTube(Value: integer);
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

  protected
    { Déclarations protégées }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    function HorsLimites: integer;
  published
    property Etiquette: String read FEtiquette write SetEtiquette;
    property Unite: String read FUnite write SetUnite;
    property FormatValeurs: String read FFormatValeurs write SetFormatValeurs;
    property EpaisseurTube: integer read FEpaisseurTube write SetEpaisseurTube;
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

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TThermometre]);
end;

procedure TThermometre.Paint;
const
  marge: Single = 4;
  Ns: integer = 200;
var
  rect,rectetq, RectEcr: TRectF;
  decr: double;
  stMax, stMin, st: String;
  Wtxt, Htxt: Single;
  Xc, Yc, Rx, Ry, RxEcr, RyEcr: Single;
  BmpEtq, BmpEtq90: TBitmap;
  br: TBrush;
  pen: TStrokeBrush;
  i, j: integer;
  grad: double;
  rxy, x, y, x1, x2, y1, y2: Single;
  indxA, indxB, i0, i1, L: integer;
  yGradMini, YGradMaxi, Yval: Single;
  deltaMaj, deltaMin: Single;
  nbgradMaj, nbgradMin: integer;
  xg1, xg2, yg: Single;
  dta90,dta00:TBitmapData;
begin

  Canvas.BeginScene;

  // Fond transparent
  br := TBrush.Create(TBrushKind.Solid, 0);
  rect := TRectF.Create(0, 0, Width, Height);
  Canvas.FillRect(rect, 0, 0, AllCorners, 100, br);
  br.Free;

  // Calcul de la hauteur et de la largeur max des étiquettes
  stMax := Format(FFormatValeurs, [Maxi]);
  stMin := Format(FFormatValeurs, [Mini]);
  Htxt := Canvas.TextHeight(stMax);
  if Canvas.TextHeight(stMin) > Htxt then
    Htxt := Canvas.TextHeight(stMin);
  Wtxt := Canvas.TextWidth(stMax);
  if Canvas.TextWidth(stMin) > Wtxt then
    Wtxt := Canvas.TextWidth(stMin);

  if MontreSeuils then
  begin
    indxA := -1;
    indxB := -1;
    if (Seuil_A > Mini) and (Seuil_A < Maxi) and (Seuil_B > Mini) and (Seuil_B < Maxi) and (Seuil_A < Seuil_B) then
    begin
    end;
  end;
  // Dessin du contour du thermometre
  Canvas.Stroke.Color := Stroke.Color;

  // Dessin des chiffres de graduation
  nbgradMaj := 2;
  nbgradMin := 0;
  yGradMini := Height - marge - 3 * EpaisseurTube - Htxt / 2;
  YGradMaxi := marge + Htxt / 2;
  if GraduationMajeure > 0 then
  begin
    nbgradMaj := round((Maxi - Mini) / GraduationMajeure) + 1;
    deltaMaj := (YGradMaxi - yGradMini) / (nbgradMaj - 1);
    if (GraduationMineure > 0) and (GraduationMajeure > GraduationMineure) then
    begin
      nbgradMin := round(GraduationMajeure / GraduationMineure);
      deltaMin := deltaMaj / nbgradMin;
    end;
  end;
  if MontreValeurs then
  begin
    Canvas.Fill.Color := Stroke.Color;
    for i := 0 to nbgradMaj - 1 do
    begin
      yg := yGradMini + i * deltaMaj;
      xg1 := Width / 2 - marge - EpaisseurTube / 2 - LongueurGraduationMajeure;
      xg2 := Width / 2 + marge + EpaisseurTube / 2;
      RectEcr.left := xg1 - Wtxt - marge;
      RectEcr.top := yg - Htxt / 2;
      RectEcr.right := RectEcr.left + Wtxt;
      RectEcr.bottom := RectEcr.top + Htxt;
      grad := Mini + i * GraduationMajeure;
      st := Format(FFormatValeurs, [grad]);
      Canvas.FillText(RectEcr, st, false, 1, [], TTextAlign.Center, TTextAlign.Center);
      if MontreGraduationMajeure then
      begin
        Canvas.DrawLine(TPointF.Create(xg1, yg), TPointF.Create(xg1 + LongueurGraduationMajeure, yg), 1);
        Canvas.DrawLine(TPointF.Create(xg2, yg), TPointF.Create(xg2 + LongueurGraduationMajeure, yg), 1);
        if (MontreGraduationMineure) and (GraduationMineure > 0) and (i <> nbgradMaj - 1) then
        begin
          for j := 1 to nbgradMin - 1 do
          begin
            yg := yGradMini + i * deltaMaj + j * deltaMin;
            xg1 := Width / 2 - marge - EpaisseurTube / 2 - LongueurGraduationMineure;
            xg2 := Width / 2 + marge + EpaisseurTube / 2;
            Canvas.DrawLine(TPointF.Create(xg1, yg), TPointF.Create(xg1 + LongueurGraduationMineure, yg), 1);
            Canvas.DrawLine(TPointF.Create(xg2, yg), TPointF.Create(xg2 + LongueurGraduationMineure, yg), 1);
          end;
        end;
      end;
    end;
  end;
  // dessin du tube
  if Valeur < Mini then
  begin
    Yval := yGradMini;
  end
  else
  begin
    if Valeur > Maxi then
    begin
      Yval := YGradMaxi;
    end
    else
    begin
      Yval := (Valeur - Mini) / (Maxi - Mini) * (YGradMaxi - yGradMini) + yGradMini;
    end;
  end;
  x1 := (Width - EpaisseurTube) / 2;
  y1 := Yval;
  x2 := x1 + EpaisseurTube;
  y2 := Height - marge - 1.5 * EpaisseurTube;
  rxy := EpaisseurTube / 3;
  Canvas.FillRect(TRectF.Create(x1, y1, x2, y2), 0, 0, AllCorners, 1, Fill);
  x1 := (Width - EpaisseurTube) / 2;
  y1 := marge;
  x2 := x1 + EpaisseurTube;
  y2 := Height - marge - 1.5 * EpaisseurTube;
  rxy := EpaisseurTube / 3;
  Canvas.DrawRect(TRectF.Create(x1, y1, x2, y2), rxy, rxy, [TCorner.TopLeft, TCorner.TopRight], 1);
  x1 := Width / 2 - EpaisseurTube * 1.5;
  y1 := Height - marge - 3 * EpaisseurTube;
  x2 := x1 + 3 * EpaisseurTube;
  y2 := Height - marge;
  Canvas.FillEllipse(TRectF.Create(x1, y1, x2, y2), 1, Fill);
  Canvas.DrawEllipse(TRectF.Create(x1, y1, x2, y2), 1);
  // Dessin de l'étiquette
  if (Etiquette <> '') or (Unite<>'') then
  begin
    BmpEtq := TBitmap.Create;
    st:=Etiquette;
    if Unite<>'' then
     st:=st+' ('+Unite+')';
    Htxt := Canvas.TextHeight(st);
    Wtxt := Canvas.TextWidth(st);
    RectEcr.left := 0;
    RectEcr.top := 0;
    RectEcr.right := Wtxt;
    RectEcr.bottom := Htxt;
    BmpEtq.Width := round(Wtxt);
    BmpEtq.Height := round(Htxt);

    //Ecriture
    BmpEtq.Canvas.BeginScene;
    br := TBrush.Create(TBrushKind.Solid, 0);
    rect := TRectF.Create(0, 0, WTxt, HTxt);
    BmpEtq.Canvas.FillRect(rect, 0, 0, AllCorners, 100, br);
    br.Free;
    BmpEtq.Canvas.Fill.Color:=$FF000000;
    BmpEtq.Canvas.FillText(RectEcr, st, false, 1, [], TTextAlign.Center, TTextAlign.Center);
    BmpEtq.Canvas.EndScene;

    // Rotation
    BmpEtq90 := TBitmap.Create;
    BmpEtq90.Width := round(Htxt);
    BmpEtq90.Height := round(Wtxt);
    if BmpEtq90.Map(TMapAccess.Write,dta90) and BmpEtq.Map(TMapAccess.Read,dta00) then
     begin
      for  i:=0 to BmpEtq.Width-1 do
       begin
         for j := 0 to BmpEtq.Height-1 do
           dta90.SetPixel(j,BmpEtq.Width-i-1,dta00.GetPixel(i,j));
       end;
       bmpEtq90.Unmap(dta90);
       bmpEtq.Unmap(dta00);
       x1:=Width / 2 + marge + EpaisseurTube / 2+LongueurGraduationMajeure;
       x2:=x1+BmpEtq90.Width;
       y1:=(Height-3*EpaisseurTube)/2-WTxt/2;
       y2:=y1+WTxt;
       rect:=TRectF.Create(x1,y1,x2,y2);
       rectetq:=TRectF.Create(0,0,HTxt,WTxt);
       Canvas.DrawBitmap(BmpEtq90,rectetq,rect,1);
     end;
  end;
  Canvas.EndScene;
end;

constructor TThermometre.Create(AOwner: TComponent);
begin
  inherited;
  FEtiquette := '';
  FFormatValeurs := '%2.1f';
  FMaxi := 100;
  FMini := 0;
  FValeur := 50;
  FSeuil_A := 25;
  FSeuil_B := 75;
  FGraduationMajeure := 10;
  FGraduationMineure := 2;
  FMontreGraduationMajeure := true;
  FMontreGraduationMineure := true;
  FMontreValeurs := true;
  FMontreSeuils := true;
  FEpaisseurTube := 16;
  FLgGrdMaj := 16;
  FLgGrdMin := 8;

end;

function TThermometre.HorsLimites: integer;
begin
  if FValeur < FSeuil_A then
    result := -1
  else if FValeur > FSeuil_B then
    result := 1
  else
    result := 0;
end;

procedure TThermometre.SetFormatValeurs(Value: String);
begin
  FFormatValeurs := Value;
end;

procedure TThermometre.SetEpaisseurTube(Value: integer);
begin
  FEpaisseurTube := Value;
end;

procedure TThermometre.SetEtiquette(Value: String);
begin
  FEtiquette := Value;
end;

procedure TThermometre.SetMaxi(Value: double);
begin
  FMaxi := Value;
end;

procedure TThermometre.SetMini(Value: double);
begin
  FMini := Value;
end;

procedure TThermometre.SetValeur(Value: double);
begin
  if FValeur <> Value then
  begin
    FValeur := Value;
    Repaint;
  end;
end;

procedure TThermometre.SetSeuil_A(Value: double);
begin
  FSeuil_A := Value;
end;

procedure TThermometre.SetSeuil_B(Value: double);
begin
  FSeuil_B := Value;
end;

procedure TThermometre.SetUnite(Value: String);
begin
FUnite:=Value;
end;

procedure TThermometre.SetGraduationMajeure(Value: double);
begin
  FGraduationMajeure := Value;
end;

procedure TThermometre.SetGraduationMineure(Value: double);
begin
  FGraduationMineure := Value;
end;

procedure TThermometre.SetLgGrdMaj(Value: integer);
begin
  FLgGrdMaj := Value;
end;

procedure TThermometre.SetLgGrdMin(Value: integer);
begin
  FLgGrdMin := Value;
end;

procedure TThermometre.SetMontreGraduationMajeure(Value: boolean);
begin
  FMontreGraduationMajeure := Value;
end;

procedure TThermometre.SetMontreGraduationMineure(Value: boolean);
begin
  FMontreGraduationMineure := Value;
end;

procedure TThermometre.SetMontreValeurs(Value: boolean);
begin
  FMontreValeurs := Value;
end;

procedure TThermometre.SetMontreSeuils(Value: boolean);
begin
  FMontreSeuils := Value;
end;

// initialization

// FMX.Types.GlobalUseGPUCanvas := true;

end.
