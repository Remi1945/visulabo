unit Selecteur;

interface

uses
  System.SysUtils, Couleurs, System.Classes, System.Types, System.UIConsts, FMX.Types, FMX.Controls, FMX.Objects,
  FMX.Graphics, System.UITypes, System.Math.Vectors, TextControlTextSettings;

type
  TGenre = (Circulaire, Horizontal, Vertical);

  TSelecteur = class(TRectangle)
  private
    dangle, angle0, origine, pas: Single;
    zoneSensible: TRectF;
    // deltaI: integer;
    Rg1: Single;
    Rg0: Single;
    Rbtn: Single;
    nouvsel: integer;
    FHCurseur: integer;
    FLCurseur: integer;
    FHtGlissiere: integer;
    FEpGlissiere: integer;
    FLngGrad: integer;
    FAngleTexte: integer;
    FIndex: integer;
    FGenre: TGenre;
    FChoix: TStringList;
    FTextSettingsInfo: TTextSettingsInfo;
    function GetDefaultTextSettings: TTextSettings;
    function GetTextSettings: TTextSettings;
    function GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass;
    procedure SetTextSettings(const Value: TTextSettings);
    function getIndex(x, y: Single): integer;
    procedure setChoix(valeur: TStringList);
    procedure setAngleTexte(const Value: integer);
    procedure setEpGlissiere(const Value: integer);
    procedure setHCurseur(const Value: integer);
    procedure setHtGlissiere(const Value: integer);
    procedure setLCurseur(const Value: integer);
    procedure setLngGrad(const Value: integer);
    procedure setIndex(const Value: integer);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    /// <summary>Stores a TTextSettings type object keeping the default values of the text representation properties</summary>
    property DefaultTextSettings: TTextSettings read GetDefaultTextSettings;
  published
    property Genre: TGenre read FGenre write FGenre;
    property Choix: TStringList read FChoix write setChoix;
    property HauteurCurseur: integer read FHCurseur write setHCurseur;
    property LargeuCurseur: integer read FLCurseur write setLCurseur;
    property HauteurGlissiere: integer read FHtGlissiere write setHtGlissiere;
    property EpaisseurGlissiere: integer read FEpGlissiere write setEpGlissiere;
    property AngleTexte: integer read FAngleTexte write setAngleTexte;
    property LongueurGraduation: integer read FLngGrad write setLngGrad;
    property Selection: integer read FIndex write setIndex;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TSelecteur]);
end;

constructor TSelecteur.Create(AOwner: TComponent);
begin
  inherited;
  FIndex := -1;
  FGenre := Horizontal;
  FChoix := TStringList.Create;
  FHCurseur := 16;
  FLCurseur := 8;
  FHtGlissiere := 16;
  FEpGlissiere := 4;
  FAngleTexte := 45;
  FLngGrad := 10;
  FTextSettingsInfo := TTextSettingsInfo.Create(Self, GetTextSettingsClass);
end;

function TSelecteur.getIndex(x, y: Single): integer;
var
  i, imin: integer;
  d, dmin, alpha: double;
  xg, yg: Single;
begin
  imin := -1;
  if FGenre = Circulaire then
  begin
    dmin := 1E39;
    for i := 0 to FChoix.Count - 1 do
    begin
      alpha := (angle0 + i * dangle) / 180 * PI;
      xg := (Rg0 + Rg1) / 2 * cos(alpha) + Width / 2;
      yg := (Rg0 + Rg1) / 2 * sin(alpha) + Height / 2;
      d := sqrt((x - xg) * (x - xg) + (y - yg) * (y - yg));
      if d < dmin then
      begin
        dmin := d;
        imin := i;
      end;
    end;
    if dmin > FLngGrad then
      imin := -1;
  end;
  if FGenre = Horizontal then
  begin
    if zoneSensible.Contains(TPointF.Create(x, y)) then
    begin
      dmin := 1E39;
      for i := 0 to FChoix.Count - 1 do
      begin
        xg := origine + i * pas;
        yg := (zoneSensible.Top + zoneSensible.Bottom) / 2;
        d := (x - xg) * (x - xg) + (y - yg) * (y - yg);
        if d < dmin then
        begin
          dmin := d;
          imin := i;
        end;
      end;
    end;
  end;
  if FGenre = Vertical then
  begin
    if zoneSensible.Contains(TPointF.Create(x, y)) then
    begin
      dmin := 1E39;
      for i := 0 to FChoix.Count - 1 do
      begin
        xg := (zoneSensible.Left + zoneSensible.Right) / 2;
        yg := origine + i * pas;
        d := (x - xg) * (x - xg) + (y - yg) * (y - yg);
        if d < dmin then
        begin
          dmin := d;
          imin := i;
        end;
      end;
    end;
  end;
  result := imin;
end;

procedure TSelecteur.MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: Single);
begin
  if Button = TMouseButton.mbLeft then
  begin
    nouvsel := getIndex(x, y);
  end;
end;

procedure TSelecteur.MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Single);
var
  nindx: integer;
begin
  if (Button = TMouseButton.mbLeft) and (nouvsel >= 0) then
  begin
    nindx := getIndex(x, y);
    if (nindx <> FIndex) and (nindx <> -1) then
    begin
      FIndex := nindx;
      Repaint;
    end;
  end;

end;

procedure TSelecteur.Paint;
var
  rect: TRectF;

  i: integer;
  xg, yg, Yc, Xc, dxg, dyg: Single;
  st: string;
  curseur: TPolygon;
  centre: Single;
  pointe: Single;
  bmpEcr: TBitmap;
  M: TMatrix;
  Htxt, Wtxt: Single;
  br: TBrush;
  Xe: Single;
  Ye: Single;
  decr: double;
  Recr: Single;
  alpha: double;
  xecr: Single;
  yecr: Single;
  RectEcr: TRectF;
  xr, xl, yh, yb: Single;
begin
  setlength(curseur, 6);
  Canvas.BeginScene;
  Canvas.Fill.Color := 0;
  Canvas.FillRect(TRectF.Create(0, 0, Width, Height), 0, 0, AllCorners, 1, TCornerType.Round);
  if FGenre = Vertical then
  begin
    if FChoix.Count > 1 then
    begin
      if (FAngleTexte < 60) then
      begin
        dxg := cos(FAngleTexte / 180 * PI);
        dyg := sin(FAngleTexte / 180 * PI);
        Wtxt := Canvas.TextWidth(Choix[0]);
        Htxt := Canvas.TextHeight(Choix[0]);
        yh := FLngGrad * dxg + Htxt * dyg;
        if yh < 0 then
          origine := -yh
        else
          origine := 5 + FEpGlissiere + FLCurseur / 2;
        Wtxt := Canvas.TextWidth(Choix[FChoix.Count - 1]);
        Htxt := Canvas.TextHeight(Choix[FChoix.Count - 1]);
        yb := dyg * (FLngGrad + Wtxt) + Htxt * dxg;
      end
      else
      begin
        dxg := 1;
        dyg := 0;
        st := Choix[0];
        if Canvas.TextHeight(st) / 2 + 5 > 5 + FEpGlissiere + FLCurseur / 2 then
          origine := Canvas.TextHeight(st) / 2 + 5
        else
          origine := 5 + FEpGlissiere + FLCurseur / 2;
        st := Choix[FChoix.Count - 1];
        yb := Canvas.TextHeight(st) / 2 + 5;
      end;
      pas := (Height - origine - yb) / (FChoix.Count - 1);
      Xc := FLCurseur / 2 + FHCurseur;
      if FHtGlissiere / 2 > Xc then
        Xc := FHtGlissiere / 2;
      // dessin de la glissière
      Canvas.Fill := Fill;
      zoneSensible := TRectF.Create(5 + Xc - FHtGlissiere / 2, 0, 5 + Xc + FHtGlissiere / 2, Height);
      Canvas.FillRect(zoneSensible, 0, 0, AllCorners, 1, TCornerType.Round);
      Canvas.Fill.Color := claBlack; // A modifier par une couleur utilisateur
      Canvas.FillRect(TRectF.Create(zoneSensible.Left + FEpGlissiere, zoneSensible.Top + FEpGlissiere,
        zoneSensible.Right - FEpGlissiere, zoneSensible.Bottom - FEpGlissiere), 0, 0, AllCorners, 1, TCornerType.Round);
      // dessin des graduation

      for i := 0 to FChoix.Count - 1 do
      begin

        xg := 5 + Xc + FHtGlissiere / 2;
        yg := origine + i * pas;
        // écriture du texte
        st := Choix[i];
        Wtxt := Canvas.TextWidth(st);
        if (FAngleTexte < 60) then
        begin
          bmpEcr := TBitmap.Create;
          bmpEcr.Width := Round(Wtxt * 2);
          bmpEcr.Height := Round(Wtxt * 2);
          bmpEcr.Canvas.BeginScene;
          br := TBrush.Create(TBrushKind.Solid, 0);
          bmpEcr.Canvas.Fill.Color := TextSettings.FontColor;
          bmpEcr.Canvas.Font.Family := TextSettings.Font.Family;
          bmpEcr.Canvas.Font.Size := TextSettings.Font.Size;
          bmpEcr.Canvas.Font.Style := TextSettings.Font.Style;
          bmpEcr.Canvas.FillRect(TRectF.Create(0, 0, bmpEcr.Width, bmpEcr.Height), 0, 0, AllCorners, 100, br);
          FreeAndNil(br);
          M := bmpEcr.Canvas.Matrix;
          M := M * TMatrix.CreateTranslation(-Wtxt, -Wtxt);
          M := M * TMatrix.CreateRotation(FAngleTexte / 180 * PI);
          M := M * TMatrix.CreateTranslation(Wtxt, Wtxt);
          bmpEcr.Canvas.SetMatrix(M);

          bmpEcr.Canvas.FillText(TRectF.Create(0, 0, 2 * Wtxt, 2 * Wtxt), st, false, 1, [], TTextAlign.Center,
            TTextAlign.Center);
          bmpEcr.Canvas.EndScene;

          Xe := xg + dxg * (FLngGrad + Wtxt / 2);
          Ye := yg + dyg * (FLngGrad + Wtxt / 2);
          Canvas.DrawBitmap(bmpEcr, TRectF.Create(0, 0, bmpEcr.Width, bmpEcr.Height),
            TRectF.Create(Xe - bmpEcr.Width / 2, Ye - bmpEcr.Height / 2, Xe + bmpEcr.Width / 2, Ye + bmpEcr.Height / 2),
            1, false);
          FreeAndNil(bmpEcr);
        end
        else
        begin
          Canvas.Fill.Color := TextSettings.FontColor;
          Canvas.Font.Family := TextSettings.Font.Family;
          Canvas.Font.Size := TextSettings.Font.Size;
          Canvas.Font.Style := TextSettings.Font.Style;
          Wtxt := Canvas.TextWidth(st);
          Htxt := Canvas.TextHeight(st);
          Xe := xg + FLngGrad;
          Ye := yg - Htxt / 2;
          Canvas.FillText(TRectF.Create(Xe, Ye, Xe + Wtxt, Ye + Htxt), st, false, 1, [], TTextAlign.Center,
            TTextAlign.Center);
        end;
        Canvas.Stroke.Color := TextSettings.FontColor;
        Canvas.Stroke.Kind := TBrushKind.Solid;
        Canvas.DrawLine(TPointF.Create(xg, yg), TPointF.Create(xg + FLngGrad * dxg, yg + FLngGrad * dyg), 1);
      end;
    end;
    // dessin du curseur
    if (FIndex >= 0) and (FIndex < FChoix.Count) then
    begin
      xg := Xc + FHtGlissiere / 2;
      yg := origine + FIndex * pas;
      pointe := FLCurseur / 2;

      curseur[0] := TPointF.Create(xg, yg);
      curseur[1] := TPointF.Create(Xc, yg - pointe);
      curseur[2] := TPointF.Create(Xc - FHCurseur, yg - pointe);
      curseur[3] := TPointF.Create(Xc  - FHCurseur, yg + pointe);
      curseur[4] := TPointF.Create(Xc , yg + pointe);
      curseur[5] := curseur[0];
      Canvas.Fill.Color := Stroke.Color;
      Canvas.Fill.Kind := Stroke.Kind;
      Canvas.Fill.Gradient := Stroke.Gradient;
      Canvas.FillPolygon(curseur, 1);
    end;
  end;
  if FGenre = Circulaire then
  begin
    // les angles sont placé comme ceci
    // ....-90
    // 180.....0
    // .....90
    // avec x=cos(angle) et y =sin(angle)
    if FChoix.Count > 1 then
    begin
      dangle := 0;
      angle0 := 0;
      if FChoix.Count = 2 then
      begin
        dangle := 90;
        angle0 := -135;
      end;
      if FChoix.Count = 3 then
      begin
        dangle := 90;
        angle0 := -180;
      end;
      if FChoix.Count > 3 then
      begin
        angle0 := -225;
        dangle := 270 / (FChoix.Count - 1);
      end;
      // Calcul de la hauteur et de la largeur max des étiquettes
      Htxt := 0;
      Wtxt := 0;
      for st in FChoix do
      begin
        if Canvas.TextHeight(st) > Htxt then
          Htxt := Canvas.TextHeight(st);
        if Canvas.TextWidth(st) > Wtxt then
          Wtxt := Canvas.TextWidth(st);
      end;
      decr := sqrt(Wtxt * Wtxt + Htxt * Htxt) / 2;
      if Width > Height then
        Recr := Height / 2 - decr
      else
        Recr := Width / 2 - decr;
      Rg1 := Recr - decr - 4;
      Rg0 := Rg1 - FLngGrad;
      Rbtn := Rg0 - 4;
      Canvas.Fill := Fill;
      Canvas.FillEllipse(TRectF.Create(Width / 2 - Rbtn, Height / 2 - Rbtn, Width / 2 + Rbtn, Height / 2 + Rbtn), 1);
      // affichage des écritures
      Canvas.Fill.Color := TextSettings.FontColor;
      Canvas.Fill.Kind := TBrushKind.Solid;
      Canvas.Stroke.Color := TextSettings.FontColor;
      Canvas.Stroke.Kind := TBrushKind.Solid;
      for i := 0 to FChoix.Count - 1 do
      begin
        alpha := (angle0 + i * dangle) / 180 * PI;
        xecr := Recr * cos(alpha) + Width / 2;
        yecr := Recr * sin(alpha) + Height / 2;
        RectEcr.Left := xecr - Wtxt / 2;
        RectEcr.Top := yecr - Htxt / 2;
        RectEcr.Right := xecr + Wtxt / 2;
        RectEcr.Bottom := yecr + Htxt / 2;
        Canvas.FillText(RectEcr, FChoix[i], false, 1, [], TTextAlign.Center, TTextAlign.Center);
        Canvas.DrawLine(TPointF.Create(Width / 2 + Rg0 * cos(alpha), Height / 2 + Rg0 * sin(alpha)),
          TPointF.Create(Width / 2 + Rg1 * cos(alpha), Height / 2 + Rg1 * sin(alpha)), 1);
      end;
      // affichage du curseur
      if (FIndex >= 0) and (FIndex < FChoix.Count) then
      begin
        alpha := (angle0 + FIndex * dangle) / 180 * PI;
        Xe := Rbtn * cos(alpha) + Width / 2;
        Ye := Rbtn * sin(alpha) + Height / 2;
        curseur[0] := TPointF.Create(Xe, Ye);
        Xe := (Rbtn - FLCurseur) * cos(alpha) - FLCurseur * sin(alpha) + Width / 2;
        Ye := (Rbtn - FLCurseur) * sin(alpha) + FLCurseur * cos(alpha) + Height / 2;
        curseur[1] := TPointF.Create(Xe, Ye);
        Xe := -FLCurseur * cos(alpha) - FLCurseur * sin(alpha) + Width / 2;
        Ye := -FLCurseur * sin(alpha) + FLCurseur * cos(alpha) + Height / 2;
        curseur[2] := TPointF.Create(Xe, Ye);
        Xe := -FLCurseur * cos(alpha) + FLCurseur * sin(alpha) + Width / 2;
        Ye := -FLCurseur * sin(alpha) - FLCurseur * cos(alpha) + Height / 2;
        curseur[3] := TPointF.Create(Xe, Ye);
        Xe := (Rbtn - FLCurseur) * cos(alpha) + FLCurseur * sin(alpha) + Width / 2;
        Ye := (Rbtn - FLCurseur) * sin(alpha) - FLCurseur * cos(alpha) + Height / 2;
        curseur[4] := TPointF.Create(Xe, Ye);
        curseur[5] := curseur[0];
        Canvas.Fill.Color := Stroke.Color;
        Canvas.Fill.Kind := Stroke.Kind;
        Canvas.Fill.Gradient := Stroke.Gradient;
        Canvas.FillPolygon(curseur, 1);
      end;
    end;
  end;
  if FGenre = Horizontal then
  begin
    if FChoix.Count > 1 then
    begin
      if (FAngleTexte > 30) then
      begin
        dxg := cos(FAngleTexte / 180 * PI);
        dyg := -sin(FAngleTexte / 180 * PI);
        Wtxt := Canvas.TextWidth(Choix[0]);
        Htxt := Canvas.TextHeight(Choix[0]);
        xl := FLngGrad * dxg + Htxt * dyg;
        if xl < 0 then
          origine := -xl
        else
          origine := 5 + FEpGlissiere + FLCurseur / 2;
        Wtxt := Canvas.TextWidth(Choix[FChoix.Count - 1]);
        Htxt := Canvas.TextHeight(Choix[FChoix.Count - 1]);
        xr := dxg * (FLngGrad + Wtxt) - Htxt * dyg;
      end
      else
      begin
        dxg := 0;
        dyg := -1;
        st := Choix[0];
        if Canvas.TextWidth(st) / 2 + 5 > 5 + FEpGlissiere + FLCurseur / 2 then
          origine := Canvas.TextWidth(st) / 2 + 5
        else
          origine := 5 + FEpGlissiere + FLCurseur / 2;
        st := Choix[FChoix.Count - 1];
        xr := Canvas.TextWidth(st) / 2 + 5;
      end;
      pas := (Width - origine - xr) / (FChoix.Count - 1);
      Yc := FLCurseur / 2 + FHCurseur;
      if FHtGlissiere / 2 > Yc then
        Yc := FHtGlissiere / 2;
      // dessin de la glissière
      Canvas.Fill := Fill;
      zoneSensible := TRectF.Create(0, Height - 5 - Yc - FHtGlissiere / 2, Width, Height - 5 - Yc + FHtGlissiere / 2);
      Canvas.FillRect(zoneSensible, 0, 0, AllCorners, 1, TCornerType.Round);
      Canvas.Fill.Color := claBlack; // A modifier par une couleur utilisateur
      Canvas.FillRect(TRectF.Create(EpaisseurGlissiere, Height - 5 - Yc - FHtGlissiere / 2 + EpaisseurGlissiere,
        Width - EpaisseurGlissiere, Height - 5 - Yc + FHtGlissiere / 2 - EpaisseurGlissiere), 0, 0, AllCorners, 1,
        TCornerType.Round);
      // dessin des graduation

      for i := 0 to FChoix.Count - 1 do
      begin
        xg := origine + i * pas;
        yg := Height - 5 - Yc - FHtGlissiere / 2;
        // écriture du texte
        st := Choix[i];
        Wtxt := Canvas.TextWidth(st);
        if (FAngleTexte > 30) then
        begin
          bmpEcr := TBitmap.Create;
          bmpEcr.Width := Round(Wtxt * 2);
          bmpEcr.Height := Round(Wtxt * 2);
          bmpEcr.Canvas.BeginScene;
          br := TBrush.Create(TBrushKind.Solid, 0);
          bmpEcr.Canvas.Fill.Color := TextSettings.FontColor;
          bmpEcr.Canvas.Font.Family := TextSettings.Font.Family;
          bmpEcr.Canvas.Font.Size := TextSettings.Font.Size;
          bmpEcr.Canvas.Font.Style := TextSettings.Font.Style;
          bmpEcr.Canvas.FillRect(TRectF.Create(0, 0, bmpEcr.Width, bmpEcr.Height), 0, 0, AllCorners, 100, br);
          FreeAndNil(br);
          M := bmpEcr.Canvas.Matrix;
          M := M * TMatrix.CreateTranslation(-Wtxt, -Wtxt);
          M := M * TMatrix.CreateRotation((-FAngleTexte) / 180 * PI);
          M := M * TMatrix.CreateTranslation(Wtxt, Wtxt);
          bmpEcr.Canvas.SetMatrix(M);

          bmpEcr.Canvas.FillText(TRectF.Create(0, 0, 2 * Wtxt, 2 * Wtxt), st, false, 1, [], TTextAlign.Center,
            TTextAlign.Center);
          bmpEcr.Canvas.EndScene;

          Xe := xg + dxg * (FLngGrad + Wtxt / 2);
          Ye := yg + dyg * (FLngGrad + Wtxt / 2);
          Canvas.DrawBitmap(bmpEcr, TRectF.Create(0, 0, bmpEcr.Width, bmpEcr.Height),
            TRectF.Create(Xe - bmpEcr.Width / 2, Ye - bmpEcr.Height / 2, Xe + bmpEcr.Width / 2, Ye + bmpEcr.Height / 2),
            1, false);
          FreeAndNil(bmpEcr);
        end
        else
        begin
          Canvas.Fill.Color := TextSettings.FontColor;
          Canvas.Font.Family := TextSettings.Font.Family;
          Canvas.Font.Size := TextSettings.Font.Size;
          Canvas.Font.Style := TextSettings.Font.Style;
          Wtxt := Canvas.TextWidth(st);
          Htxt := Canvas.TextHeight(st);
          Xe := xg - Wtxt / 2;;
          Ye := yg - (FLngGrad + Htxt);
          Canvas.FillText(TRectF.Create(Xe, Ye, Xe + Wtxt, Ye + Htxt), st, false, 1, [], TTextAlign.Center,
            TTextAlign.Center);
        end;
        Canvas.Stroke.Color := TextSettings.FontColor;
        Canvas.Stroke.Kind := TBrushKind.Solid;
        Canvas.DrawLine(TPointF.Create(xg, yg), TPointF.Create(xg + FLngGrad * dxg, yg + FLngGrad * dyg), 1);
      end;
    end;
    // dessin du curseur
    if (FIndex >= 0) and (FIndex < FChoix.Count) then
    begin
      xg := origine + FIndex * pas;
      yg := Height - 5 - Yc;
      pointe := FLCurseur / 2;
      centre := (FHCurseur + pointe) / 2;
      curseur[0] := TPointF.Create(xg, yg - centre);
      curseur[1] := TPointF.Create(xg + FLCurseur / 2, yg - centre + pointe);
      curseur[2] := TPointF.Create(xg + FLCurseur / 2, yg + centre);
      curseur[3] := TPointF.Create(xg - FLCurseur / 2, yg + centre);
      curseur[4] := TPointF.Create(xg - FLCurseur / 2, yg - centre + pointe);
      curseur[5] := curseur[0];
      Canvas.Fill.Color := Stroke.Color;
      Canvas.Fill.Kind := Stroke.Kind;
      Canvas.Fill.Gradient := Stroke.Gradient;
      Canvas.FillPolygon(curseur, 1);
    end;
  end;
  Canvas.EndScene;
end;

procedure TSelecteur.setAngleTexte(const Value: integer);
begin
  FAngleTexte := Value;
  Repaint;
end;

procedure TSelecteur.setChoix(valeur: TStringList);
begin
  FChoix.Assign(valeur);
  Repaint;
end;

procedure TSelecteur.setEpGlissiere(const Value: integer);
begin
  FEpGlissiere := Value;
  Repaint;
end;

procedure TSelecteur.setHCurseur(const Value: integer);
begin
  FHCurseur := Value;
  Repaint;
end;

procedure TSelecteur.setHtGlissiere(const Value: integer);
begin
  FHtGlissiere := Value;
  Repaint;
end;

procedure TSelecteur.setIndex(const Value: integer);
begin
  FIndex := Value;
  Repaint;
end;

procedure TSelecteur.setLCurseur(const Value: integer);
begin
  FLCurseur := Value;
  Repaint;
end;

procedure TSelecteur.setLngGrad(const Value: integer);
begin
  FLngGrad := Value;
  Repaint;
end;

function TSelecteur.GetDefaultTextSettings: TTextSettings;
begin
  result := FTextSettingsInfo.DefaultTextSettings;
end;

function TSelecteur.GetTextSettings: TTextSettings;
begin
  result := FTextSettingsInfo.TextSettings;
end;

procedure TSelecteur.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettingsInfo.TextSettings.Assign(Value);
end;

function TSelecteur.GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass;
begin
  result := TTextControlTextSettings;
end;

end.
