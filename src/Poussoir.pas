unit Poussoir;

interface

uses
  System.SysUtils, Couleurs, System.Classes, System.Types, System.UIConsts, FMX.Types, FMX.Controls, FMX.Objects,
  FMX.Graphics, System.Math.Vectors, System.UITypes, TextControlTextSettings;

type
  TPoussoir = class(TRectangle)
  private
    FTexte: String;
    Fon: boolean;
    FcoulON, FcoulOFF: TCouls;
    FEpaisseurBordure: integer;
    FTextSettingsInfoON: TTextSettingsInfo;
    FTextSettingsInfoOFF: TTextSettingsInfo;

    procedure SetEpaisseurBordure(Value: integer);
    procedure setOnOff(valeur: boolean);

    function GetDefaultTextSettingsON: TTextSettings;
    function GetTextSettingsON: TTextSettings;
    function GetTextSettingsClassON: TTextSettingsInfo.TCustomTextSettingsClass;
    procedure SetTextSettingsON(const Value: TTextSettings);

    function GetDefaultTextSettingsOFF: TTextSettings;
    function GetTextSettingsOFF: TTextSettings;
    function GetTextSettingsClassOFF: TTextSettingsInfo.TCustomTextSettingsClass;
    procedure SetTextSettingsOFF(const Value: TTextSettings);

  protected
    { Déclarations protégées }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    /// <summary>Stores a TTextSettings type object keeping the default values of the text representation properties</summary>
    property DefaultTextSettingsON: TTextSettings read GetDefaultTextSettingsON;
    property DefaultTextSettingsOFF: TTextSettings read GetDefaultTextSettingsOFF;

  published
    property Contact: boolean read Fon write setOnOff;
    property Texte: string read FTexte write FTexte;

    property CouleurON: TCouls read FcoulON write FcoulON;
    property CouleurOFF: TCouls read FcoulOFF write FcoulOFF;
    property EpaisseurBordure: integer read FEpaisseurBordure write SetEpaisseurBordure;
    property TextSettingsON: TTextSettings read GetTextSettingsON write SetTextSettingsON;
    property TextSettingsOFF: TTextSettings read GetTextSettingsOFF write SetTextSettingsOFF;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TPoussoir]);
end;

constructor TPoussoir.Create(AOwner: TComponent);
begin
  inherited;
  Fon := true;
  FTexte := 'label';
  FcoulON := Vert;
  FcoulOFF := Gris;
  FEpaisseurBordure := 8;
  FTextSettingsInfoON := TTextSettingsInfo.Create(Self, GetTextSettingsClassON);
  FTextSettingsInfoOFF := TTextSettingsInfo.Create(Self, GetTextSettingsClassOFF);
end;

procedure TPoussoir.Paint;
var
  rect: TRectF;
  bmp: TBitmap;
  dta: TBitmapData;
  x, y: integer;
  coul: TAlphaColor;
  Ax, Ay, Bx, By: Single;
  r0, g0, b0, r1, g1, b1, r, g, b, d, dmax: Single;
  ir, ib, ig: integer;
  cadre: TPolygon;
begin
  Canvas.BeginScene;
  rect := TRectF.Create(0, 0, Width, Height);
  if Fon then
  begin
    Ax := Width / 4;
    Ay := Height / 2;
    Bx := 3 * Width / 4;
    By := Height / 2;
    bmp := TBitmap.Create(round(Width), round(Height));

    // dmax := sqrt(Bx * Bx + By * By);
    dmax := sqrt(Width * Width + Height * Height) / 3;
    if bmp.Map(TMapAccess.Write, dta) then
    begin
      r0 := 1;
      g0 := 1;
      b0 := 1;
      getRGBCoul(FcoulON, r1, g1, b1);
      for y := 0 to bmp.Height - 1 do
      begin
        for x := 0 to bmp.Width - 1 do
        begin
          d := sqrt((Ax - x) * (Ax - x) + (Ay - y) * (Ay - y)) / dmax;
          if d > 1 then
            d := 1;
          r := (r1 - r0) * d + r0;
          g := (g1 - g0) * d + g0;
          b := (b1 - b0) * d + b0;
          d := sqrt((Bx - x) * (Bx - x) + (By - y) * (By - y)) / dmax;
          if d > 1 then
            d := 1;

          ir := round(255 * ((r + (r1 - r0) * d + r0) / 2));
          if ir > 255 then
            ir := 255;
          if ir < 0 then
            ir := 0;
          ig := round(255 * ((g + (g1 - g0) * d + g0) / 2));
          if ig > 255 then
            ig := 255;
          if ig < 0 then
            ig := 0;

          ib := round(255 * ((b + (b1 - b0) * d + b0) / 2));
          if ib > 255 then
            ib := 255;
          if ib < 0 then
            ib := 0;
          coul := $FF000000 + ib + ig shl 8 + ir shl 16;
          dta.SetPixel(x, y, coul);
        end;
      end;
      bmp.Unmap(dta);
    end;
    Canvas.DrawBitmap(bmp, rect, rect, 1);
    Canvas.Font.Family := TextSettingsON.Font.Family;
    Canvas.Font.Size := TextSettingsON.Font.Size;
    Canvas.Fill.Color := TextSettingsON.FontColor;
    Canvas.Font.Style := TextSettingsON.Font.Style;

  end
  else
  begin
    Canvas.Fill.Color := setCoul(FcoulOFF);
    Canvas.FillRect(rect, 0, 0, AllCorners, 1);
    Canvas.Font.Family := TextSettingsOFF.Font.Family;
    Canvas.Font.Size := TextSettingsOFF.Font.Size;
    Canvas.Fill.Color := TextSettingsOFF.FontColor;
    Canvas.Font.Style := TextSettingsOFF.Font.Style;

  end;

  Canvas.FillText(rect, FTexte, true, 1, [], TTextAlign.Center, TTextAlign.Center);
  setLength(cadre, 11);
  cadre[0] := TPointF.Create(0, 0);
  cadre[1] := TPointF.Create(Width, 0);
  cadre[2] := TPointF.Create(Width, Height);
  cadre[3] := TPointF.Create(0, Height);
  cadre[4] := cadre[0];
  cadre[5] := TPointF.Create(FEpaisseurBordure, FEpaisseurBordure);
  cadre[6] := TPointF.Create(FEpaisseurBordure, Height - FEpaisseurBordure);
  cadre[7] := TPointF.Create(Width - FEpaisseurBordure, Height - FEpaisseurBordure);
  cadre[8] := TPointF.Create(Width - FEpaisseurBordure, FEpaisseurBordure);
  cadre[9] := cadre[5];
  cadre[10] := cadre[0];
  // Canvas.Fill.Color := setCoul(FcoulBORD);
  Canvas.Fill := Fill;
  Canvas.FillPolygon(cadre, 1);
  Canvas.EndScene;
end;

procedure TPoussoir.SetEpaisseurBordure(Value: integer);
begin
  FEpaisseurBordure := Value;
  Repaint;
end;

procedure TPoussoir.setOnOff(valeur: boolean);
begin
  Fon := valeur;
  Repaint;
end;

function TPoussoir.GetDefaultTextSettingsON: TTextSettings;
begin
  Result := FTextSettingsInfoON.DefaultTextSettings;
end;

function TPoussoir.GetTextSettingsON: TTextSettings;
begin
  Result := FTextSettingsInfoON.TextSettings;
end;

procedure TPoussoir.SetTextSettingsON(const Value: TTextSettings);
begin
  FTextSettingsInfoON.TextSettings.Assign(Value);
end;

function TPoussoir.GetTextSettingsClassON: TTextSettingsInfo.TCustomTextSettingsClass;
begin
  Result := TTextControlTextSettings;
end;

function TPoussoir.GetDefaultTextSettingsOFF: TTextSettings;
begin
  Result := FTextSettingsInfoOFF.DefaultTextSettings;
end;

function TPoussoir.GetTextSettingsOFF: TTextSettings;
begin
  Result := FTextSettingsInfoOFF.TextSettings;
end;

procedure TPoussoir.SetTextSettingsOFF(const Value: TTextSettings);
begin
  FTextSettingsInfoOFF.TextSettings.Assign(Value);
end;

function TPoussoir.GetTextSettingsClassOFF: TTextSettingsInfo.TCustomTextSettingsClass;
begin
  Result := TTextControlTextSettings;
end;

end.
