unit GraphicXYdeT;


interface

uses
  System.SysUtils, Couleurs, TextControlTextSettings, System.Classes,
  System.Types, FMX.Types, FMX.Controls,
  FMX.Graphics, FMX.Objects, System.UITypes, System.UIConsts, PointInteret;

type

  TGraphicXYdeT = class(TRectangle)
  private
    FcoulDerPts: TCouls;
    FcoulAxe: TCouls;
    FcoulZone: TCouls;
    FcoulGrille: TCouls;
    FcoulZI: TCouls;
    FUniteX, FUniteY: String;
    FFormatY, FFormatX: String;
    FTitre: String;
    ValeursX: array [0 .. 199] of double;
    ValeursY: array [0 .. 199] of double;
    FNbValeurs: integer;
    FEchelleAuto: boolean;
    FZone: boolean;
    FMontrePI: boolean;
    FCarre: boolean;
    FAxe: boolean;
    FGrille: boolean;
    FminY, FmaxY: double;
    FminX, FmaxX: double;
    ZminY, ZmaxY: double;
    ZminX, ZmaxX: double;
    FGraduationMajeure, FGraduationMineure: double;
    FLgGrdMaj, FLgGrdMin: integer;
    FTextSettingsInfo: TTextSettingsInfo;
    Fsurligne: integer;
    listePI: TList;

    procedure SetUniteX(Value: String);
    procedure SetUniteY(Value: String);
    procedure SetFormatX(Value: String);
    procedure SetFormatY(Value: String);
    procedure SetMiniX(Value: double);
    procedure SetMaxiX(Value: double);
    procedure SetMiniY(Value: double);
    procedure SetMaxiY(Value: double);
    procedure SetMiniZX(Value: double);
    procedure SetMaxiZX(Value: double);
    procedure SetMiniZY(Value: double);
    procedure SetMaxiZY(Value: double);
    procedure SetEchelleAuto(Value: boolean);
    procedure SetCarre(Value: boolean);
    procedure SetGrille(Value: boolean);
    procedure SetAxe(Value: boolean);
    procedure SetTitre(Value: String);
    procedure SetZone(Value: boolean);
    procedure SetNbValeurs(nb: integer);
    procedure SetSurlignage(num: integer);
    procedure SetGraduationMajeure(Value: double);
    procedure SetGraduationMineure(Value: double);
    procedure SetLgGrdMaj(Value: integer);
    procedure SetLgGrdMin(Value: integer);
    function GetDefaultTextSettings: TTextSettings;
    function GetTextSettings: TTextSettings;
    function GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass;
    procedure SetTextSettings(const Value: TTextSettings);
  protected
    { Déclarations protégées }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure AjouteValeur(valx, valy: double; redessine: boolean); overload;
    procedure AjoutePI(xPi: TPi; redessine: boolean);
    procedure EffacePI(redessine: boolean);
  published
    property CouleurDernierPoint: TCouls read FcoulDerPts write FcoulDerPts;
    property CouleurAxe: TCouls read FcoulAxe write FcoulAxe;
    property CouleurGrille: TCouls read FcoulGrille write FcoulGrille;
    property CouleurZone: TCouls read FcoulZone write FcoulZone;
    property UniteX: String read FUniteX write SetUniteX;
    property UniteY: String read FUniteY write SetUniteY;
    property FormatX: String read FFormatX write SetFormatX;
    property FormatY: String read FFormatY write SetFormatY;
    property NbValeurs: integer read FNbValeurs write SetNbValeurs;
    property EchelleMaxX: double read FmaxX write SetMaxiX;
    property EchelleMinX: double read FminX write SetMiniX;
    property EchelleMaxY: double read FmaxY write SetMaxiY;
    property EchelleMinY: double read FminY write SetMiniY;

    property ZoneMaxX: double read ZmaxX write SetMaxiZX;
    property ZoneMinX: double read ZminX write SetMiniZX;
    property ZoneMaxY: double read ZmaxY write SetMaxiZY;
    property ZoneMinY: double read ZminY write SetMiniZY;

    property EchelleAuto: boolean read FEchelleAuto write SetEchelleAuto;
    property Proportionel: boolean read FCarre write SetCarre;
    property AfficheAxe: boolean read FAxe write SetAxe;
    property AfficheGrille: boolean read FGrille write SetGrille;
    property AfficheZone: boolean read FZone write SetZone;
    property AffichePI: boolean read FMontrePI write FMontrePI;
    property Titre: String read FTitre write SetTitre;
    property GraduationMajeure: double read FGraduationMajeure
      write SetGraduationMajeure;
    property GraduationMineure: double read FGraduationMineure
      write SetGraduationMineure;
    property LongueurGraduationMajeure: integer read FLgGrdMaj
      write SetLgGrdMaj;
    property LongueurGraduationMineure: integer read FLgGrdMin
      write SetLgGrdMin;
    property TextSettings: TTextSettings read GetTextSettings
      write SetTextSettings;
    property Surlignage: integer read Fsurligne write SetSurlignage;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TGraphicXYdeT]);
end;

constructor TGraphicXYdeT.Create(AOwner: TComponent);
var
  i: integer;

begin
  inherited;
  FUniteX := '';
  FUniteY := '';
  FFormatX := '%3.2f';
  FFormatY := '%3.2f';
  FEchelleAuto := true;
  FMontrePI := false;
  FminX := -1;
  FmaxX := 1;
  FminY := -1;
  FmaxY := 1;
  ZminX := -0.5;
  ZmaxX := 0.5;
  ZminY := -0.5;
  ZmaxY := 0.5;

  FCarre := false;

  FcoulAxe := Orange;
  FcoulZone := Vert;
  FcoulGrille := Bleu;
  FGraduationMajeure := 0.5;
  FGraduationMineure := 0.1;
  FLgGrdMaj := 16;
  FLgGrdMin := 8;
  listePI := TList.Create;
  FTextSettingsInfo := TTextSettingsInfo.Create(Self, GetTextSettingsClass);
  FSurligne:=-1;
  FNbValeurs := 200;
  for i := 0 to FNbValeurs - 1 do
  begin
    ValeursX[i] := i * 0.1 - 10 + (random(100) - 50) / 200;
    ValeursY[i] := i * 0.1 - 10 + (random(100) - 50) / 200;
  end;
end;

procedure TGraphicXYdeT.EffacePI(redessine: boolean);
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

procedure TGraphicXYdeT.Paint;
const
  marge = 4;
var
  i, j: integer;
  stYmin, stYmax, stXmin, stXmax, st: String;
  wt,ht,HXtxt, LYtxt: Single;
  HYtxt1, HYtxt0, LXtxt1, LXtxt0: Single;
  Ay, Ax, Ox, Oy, By, Bx, Gx, Gy: Single;
  x0, y0, x1, y1: Single;
  dx, dy: Single;
  rect: TRectF;
  p0, p1: TPointF;
  Xc, Yc, xPi, ypi: Single;
  a, b, c: integer;
  lePI: TPi;


  procedure ValToPoint(v_x, v_y: Single; var px, py: Single);
  var
    vx, vy: Single;
  begin
    vx := v_x;
    vy := v_y;
    if vy < FminY then
      vy := FminY;
    if vy > FmaxY then
      vy := FmaxY;
    if vx < FminX then
      vx := FminX;
    if vx > FmaxX then
      vx := FmaxX;
    py := dy * (vy - FminY) + Oy;
    px := dx * (vx - FminX) + Ox;
  end;

begin
  p0 := TPointF.Create(0, 0);
  p1 := TPointF.Create(0, 0);
  rect := TRectF.Create(0, 0, 1, 1);

  stXmin := Format(FormatX, [FminX]) + UniteX;
  stXmax := Format(FormatX, [FmaxX]) + UniteX;
  stYmin := Format(FormatY, [FminY]) + UniteY;
  stYmax := Format(FormatY, [FmaxY]) + UniteY;

  Canvas.Font.Family := TextSettings.Font.Family;
  Canvas.Font.Size := TextSettings.Font.Size;
  Canvas.Font.Style := TextSettings.Font.Style;

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
  dx := (Bx - Ox) / (FmaxX - FminX);
  dy := (Ay - Oy) / (FmaxY - FminY);

  if FCarre then
  begin
    if abs(dx) > abs(dy) then
    begin
      dx := -dy;
      Bx := Ox + dx * (FmaxX - FminX);
    end
    else
    begin
      dy := -dx;
      Ay := dy * (FmaxY - FminY) + Oy;
    end;
  end;

  Canvas.Fill := Fill;
  rect.Left := Ax;
  rect.Top := Ay;
  rect.Right := Bx;
  rect.Bottom := By;
  Canvas.FillRect(rect, 0, 0, AllCorners, 100);

  Canvas.Stroke.Color := Stroke.Color;
  Canvas.Stroke.Thickness := 2;
  ValToPoint(ValeursX[0], ValeursY[0], x0, y0);
  p0.SetLocation(x0, y0);
  for i := 1 to FNbValeurs - 1 do
  begin
    ValToPoint(ValeursX[i], ValeursY[i], x1, y1);
    p1.SetLocation(x1, y1);
    Canvas.DrawLine(p0, p1, 1);
    p0.SetLocation(x1, y1);
  end;
  Canvas.Stroke.Color := setCoul(FcoulDerPts);
  Canvas.DrawEllipse(TRectF.Create(p0.X - 4, p0.Y - 4, p0.X + 4, p0.Y + 4), 1);
  // Point centrale de la grille ou de l'axe
  if (FmaxX > 0) and (FminX < 0) then
    Xc := 0
  else
    Xc := (FmaxX + FminX) / 2;
  if (FmaxY > 0) and (FminY < 0) then
    Yc := 0
  else
    Yc := (FmaxY + FminY) / 2;
  ValToPoint(Xc, Yc, Gx, Gy);
  // Tracé de la grille
  if FGrille and (FGraduationMajeure > 0) then
  begin
    Canvas.Stroke.Color := setCoul(FcoulGrille);
    Canvas.Stroke.Thickness := 1;
    a := trunc((FminX - Xc) / FGraduationMajeure);
    b := trunc((FmaxX - Xc) / FGraduationMajeure);
    for i := a to b do
    begin
      ValToPoint(Xc + i * FGraduationMajeure, Yc, x1, y1);
      Canvas.DrawLine(TPointF.Create(x1, Ay), TPointF.Create(x1, Oy), 1);
    end;
    a := trunc((FminY - Yc) / FGraduationMajeure);
    b := trunc((FmaxY - Yc) / FGraduationMajeure);
    for i := a to b do
    begin
      ValToPoint(Xc, Yc + i * FGraduationMajeure, x1, y1);
      Canvas.DrawLine(TPointF.Create(Ox, y1), TPointF.Create(Bx, y1), 1);
    end;
  end;

  // tracé de l'axe
  if FAxe then
  begin
    Canvas.Stroke.Color := setCoul(FcoulAxe);
    Canvas.Stroke.Thickness := 1;
    Canvas.DrawLine(TPointF.Create(Gx, Ay), TPointF.Create(Gx, Oy), 1);
    Canvas.DrawLine(TPointF.Create(Ox, Gy), TPointF.Create(Bx, Gy), 1);
    if FGraduationMajeure > 0 then
    begin
      if (FGraduationMajeure > FGraduationMineure) and (FGraduationMineure > 0)
      then
        c := trunc(FGraduationMajeure / FGraduationMineure)
      else
        c := 0;
      ValToPoint(Xc, Yc, Gx, Gy);

      // axe horizontal
      a := trunc((FminX - Xc) / FGraduationMajeure);
      b := trunc((FmaxX - Xc) / FGraduationMajeure);
      for i := a to b do
      begin
        ValToPoint(Xc + i * FGraduationMajeure, Yc, x1, y1);
        Canvas.DrawLine(TPointF.Create(x1, Gy - FLgGrdMaj / 2),
          TPointF.Create(x1, Gy + FLgGrdMaj / 2), 1);
        for j := 1 to c do
        begin
          ValToPoint(Xc + i * FGraduationMajeure + j * FGraduationMineure,
            Yc, x1, y1);
          Canvas.DrawLine(TPointF.Create(x1, Gy - FLgGrdMin / 2),
            TPointF.Create(x1, Gy + FLgGrdMin / 2), 1);
        end;
      end;

      // axe vertical
      a := trunc((FminY - Yc) / FGraduationMajeure);
      b := trunc((FmaxY - Yc) / FGraduationMajeure);
      for i := a to b do
      begin
        ValToPoint(Xc, Yc + i * FGraduationMajeure, x1, y1);
        Canvas.DrawLine(TPointF.Create(Gx - FLgGrdMaj / 2, y1),
          TPointF.Create(Gx + FLgGrdMaj / 2, y1), 1);
        for j := 1 to c do
        begin
          ValToPoint(Xc, Yc + i * FGraduationMajeure + j *
            FGraduationMineure, x1, y1);
          Canvas.DrawLine(TPointF.Create(Gx - FLgGrdMin / 2, y1),
            TPointF.Create(Gx + FLgGrdMin / 2, y1), 1);
        end;
      end;
    end;
  end;
  if FZone then
  begin
    Canvas.Stroke.Color := setCoul(FcoulZone);
    Canvas.Stroke.Thickness := 1;
    ValToPoint(ZminX, ZminY, x1, y1);
    ValToPoint(ZmaxX, ZminY, x0, y0);
    Canvas.DrawLine(TPointF.Create(x0, y0), TPointF.Create(x1, y1), 1);
    ValToPoint(ZmaxX, ZmaxY, x1, y1);
    Canvas.DrawLine(TPointF.Create(x0, y0), TPointF.Create(x1, y1), 1);
    ValToPoint(ZminX, ZmaxY, x0, y0);
    Canvas.DrawLine(TPointF.Create(x0, y0), TPointF.Create(x1, y1), 1);
    ValToPoint(ZminX, ZminY, x1, y1);
    Canvas.DrawLine(TPointF.Create(x0, y0), TPointF.Create(x1, y1), 1);
  end;
  if FMontrePI then
  begin
    for i:=0 to listePI.Count-1 do
    begin
      lePI:=TPi(listePI[i]);
      st:=lePI.montexte;
      wt:=Canvas.TextWidth(st);
      ht:=Canvas.TextHeight(st);
      ValToPoint(lePI.X, lePI.Y, xPi, ypi);
      Canvas.Fill.Color:=lePI.Couleur;
      Canvas.FillText(TRectF.Create(xpi+lePi.taille,ypi-lePi.taille,xpi+wt+lePi.taille,ypi+ht-lePi.taille), st, false, 1, [], TTextAlign.Center,
    TTextAlign.Center);

      Canvas.Stroke.Color := lePI.Couleur;
      case lePI.maForme of
        FrmCarre:
          Canvas.DrawRect(TRectF.Create(xPi - lePI.taille / 2,
            ypi - lePI.taille / 2, xPi + lePI.taille / 2, ypi + lePI.taille / 2),
            0, 0, AllCorners, 1);
        FrmCercle:
          begin
            Canvas.DrawEllipse(TRectF.Create(xPi - lePI.taille / 2,
              ypi - lePI.taille / 2, xPi + lePI.taille / 2,
              ypi + lePI.taille / 2), 1);
            if Fsurligne=i then
              begin
              Canvas.FillEllipse(TRectF.Create(xPi - lePI.taille / 2,
              ypi - lePI.taille / 2, xPi + lePI.taille / 2,
              ypi + lePI.taille / 2), 1);
              end;
          end;
        FrmX:
          begin

          end;
        FrmCroix:
          begin
          end;

      end;
    end;
  end;
  // Ecriture des textes
  rect.Left := marge;
  rect.Top := Ay - HYtxt0 / 2;
  rect.Right := rect.Left + LYtxt;
  rect.Bottom := rect.Top + HYtxt1;
  Canvas.Fill.Color := TextSettings.FontColor;

  Canvas.FillText(rect, stYmax, false, 1, [], TTextAlign.Center,
    TTextAlign.Center);

  rect.SetLocation(marge, Oy - HYtxt0 / 2);
  Canvas.FillText(rect, stYmin, false, 1, [], TTextAlign.Leading,
    TTextAlign.Center);

  rect.SetLocation(Ox - LXtxt0 / 2, Oy + marge);
  rect.Width := LXtxt0;
  Canvas.FillText(rect, stXmin, false, 1, [], TTextAlign.Center,
    TTextAlign.Center);
  rect.SetLocation(Bx - LXtxt1 / 2, Oy + marge);
  rect.Width := LXtxt1;
  Canvas.FillText(rect, stXmax, false, 1, [], TTextAlign.Center,
    TTextAlign.Center);
  st := FTitre + '[' + Format(FormatX, [ValeursX[FNbValeurs - 1]]) + UniteX +
    ' , ' + Format(FormatY, [ValeursY[FNbValeurs - 1]]) + UniteY + ']';
  rect.Left := (Ox + Bx) / 2 - Canvas.TextWidth(st) / 2;
  rect.Right := rect.Left + Canvas.TextWidth(st);
  Canvas.FillText(rect, st, false, 1, [], TTextAlign.Center, TTextAlign.Center);

  Canvas.EndScene();
end;

// ---------------------------------------------------------------------------
procedure TGraphicXYdeT.SetAxe(Value: boolean);
begin
  FAxe := Value;
end;

procedure TGraphicXYdeT.SetCarre(Value: boolean);
begin
  FCarre := Value;
end;

procedure TGraphicXYdeT.SetEchelleAuto(Value: boolean);
begin
  FEchelleAuto := Value;
end;

procedure TGraphicXYdeT.SetFormatX(Value: String);
begin
  FFormatX := Value;
end;

procedure TGraphicXYdeT.SetFormatY(Value: String);
begin
  FFormatY := Value;
end;

procedure TGraphicXYdeT.SetGraduationMajeure(Value: double);
begin
  FGraduationMajeure := Value;
end;

procedure TGraphicXYdeT.SetGraduationMineure(Value: double);
begin
  FGraduationMineure := Value;
end;

procedure TGraphicXYdeT.SetGrille(Value: boolean);
begin
  FGrille := Value;
end;

procedure TGraphicXYdeT.SetZone(Value: boolean);
begin
  FZone := Value;
end;

// ---------------------------------------------------------------------------
procedure TGraphicXYdeT.SetTitre(Value: String);
begin
  FTitre := Value;
end;

procedure TGraphicXYdeT.SetUniteX(Value: String);
begin
  FUniteX := Value;
end;

procedure TGraphicXYdeT.SetUniteY(Value: String);
begin
  FUniteY := Value;
end;

// ---------------------------------------------------------------------------
procedure TGraphicXYdeT.SetMaxiX(Value: double);
begin
  FmaxX := Value;
end;

procedure TGraphicXYdeT.SetMiniX(Value: double);
begin
  FminX := Value;
end;

// ---------------------------------------------------------------------------
procedure TGraphicXYdeT.SetMaxiY(Value: double);
begin
  FmaxY := Value;
end;

procedure TGraphicXYdeT.SetMiniY(Value: double);
begin
  FminY := Value;
end;

// ---------------------------------------------------------------------------
procedure TGraphicXYdeT.SetMaxiZX(Value: double);
begin
  ZmaxX := Value;
end;

procedure TGraphicXYdeT.SetMiniZX(Value: double);
begin
  ZminX := Value;
end;

// ---------------------------------------------------------------------------
procedure TGraphicXYdeT.SetMaxiZY(Value: double);
begin
  ZmaxY := Value;
end;

procedure TGraphicXYdeT.SetMiniZY(Value: double);
begin
  ZminY := Value;
end;

// ---------------------------------------------------------------------------

procedure TGraphicXYdeT.SetNbValeurs(nb: integer);
begin
  if (nb > 0) and (nb <= 200) then
    FNbValeurs := nb;
end;

procedure TGraphicXYdeT.SetSurlignage(num: integer);
begin
  if (num >= 0) and (num < listePI.Count) then
    Fsurligne := num
  else
    Fsurligne := -1;
  Repaint;
end;

procedure TGraphicXYdeT.SetLgGrdMaj(Value: integer);
begin
  FLgGrdMaj := Value;
end;

procedure TGraphicXYdeT.SetLgGrdMin(Value: integer);
begin
  FLgGrdMin := Value;
end;

// ---------------------------------------------------------------------------
procedure TGraphicXYdeT.AjoutePI(xPi: TPi; redessine: boolean);
begin
  listePI.Add(xPi);
  if redessine then
    Repaint;
end;

procedure TGraphicXYdeT.AjouteValeur(valx, valy: double; redessine: boolean);
var
  i: integer;
  maxix, maxiy, minix, miniy: double;
begin
  if FEchelleAuto then
  begin
    maxix := valx;
    minix := valx;
    maxiy := valy;
    miniy := valy;
    for i := 1 to FNbValeurs - 1 do
    begin
      if ValeursX[i] < minix then
        minix := ValeursX[i];
      if ValeursX[i] > maxix then
        maxix := ValeursX[i];
      if ValeursY[i] < miniy then
        miniy := ValeursY[i];
      if ValeursY[i] > maxiy then
        maxiy := ValeursY[i];
    end;
    if (minix = maxix) then
    begin
      if minix = 0 then
      begin
        minix := -1;
        maxix := 1;
      end
      else
      begin
        minix := 0.9 * minix;
        maxix := 1.1 * maxix;
      end;
    end;
    if (miniy = maxiy) then
    begin
      if miniy = 0 then
      begin
        miniy := -1;
        maxiy := 1;
      end
      else
      begin
        miniy := 0.9 * miniy;
        maxiy := 1.1 * maxiy;
      end;
    end;
    SetMaxiX(maxix);
    SetMiniX(minix);
    SetMaxiY(maxiy);
    SetMiniY(miniy);
  end;
  for i := 0 to FNbValeurs - 2 do
  begin
    ValeursX[i] := ValeursX[i + 1];
    ValeursY[i] := ValeursY[i + 1];
  end;
  ValeursX[FNbValeurs - 1] := valx;
  ValeursY[FNbValeurs - 1] := valy;
  if redessine then
    Repaint;
end;

function TGraphicXYdeT.GetDefaultTextSettings: TTextSettings;
begin
  result := FTextSettingsInfo.DefaultTextSettings;
end;

function TGraphicXYdeT.GetTextSettings: TTextSettings;
begin
  result := FTextSettingsInfo.TextSettings;
end;

procedure TGraphicXYdeT.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettingsInfo.TextSettings.Assign(Value);
end;

function TGraphicXYdeT.GetTextSettingsClass
  : TTextSettingsInfo.TCustomTextSettingsClass;
begin
  result := TTextControlTextSettings;
end;

end.

