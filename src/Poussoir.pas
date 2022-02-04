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
    FcoulBORD, FcoulTEXTEON, FcoulTEXTEOFF, FcoulON, FcoulOFF: TCouls;
    FEpaisseurBordure: integer;
    FTextSettingsInfo: TTextSettingsInfo;

    procedure SetEpaisseurBordure(Value: integer);
    procedure setOnOff(valeur: boolean);
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
    property Contact: boolean read Fon write setOnOff;
    property Texte: string read FTexte write FTexte;
    property CouleurBORD: TCouls read FcoulBORD write FcoulBORD;
    property CouleurON: TCouls read FcoulON write FcoulON;
    property CouleurOFF: TCouls read FcoulOFF write FcoulOFF;
    property CouleurTexteON: TCouls read FcoulTEXTEON write FcoulTEXTEON;
    property CouleurTexteOFF: TCouls read FcoulTEXTEOFF write FcoulTEXTEOFF;
    property EpaisseurBordure: integer read FEpaisseurBordure write SetEpaisseurBordure;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
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
  FTexte := '';
  FcoulBORD := Noir;
  FcoulTEXTEON := Noir;
  FcoulTEXTEOFF := Blanc;
  FcoulON := Vert;
  FcoulOFF := Gris;
  FEpaisseurBordure := 8;
  FTextSettingsInfo := TTextSettingsInfo.Create(Self, GetTextSettingsClass);
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
    dmax := Width / 2;
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
          r := (r1 - r0) * d + r0;
          g := (g1 - g0) * d + g0;
          b := (b1 - b0) * d + b0;
          d := sqrt((Bx - x) * (Bx - x) + (By - y) * (By - y)) / dmax;
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
    Canvas.Fill.Color := setCoul(FcoulTEXTEON);;
  end
  else
  begin
    Canvas.Fill.Color := setCoul(FcoulOFF);
    Canvas.FillRect(rect, 0, 0, AllCorners, 1);
    Canvas.Fill.Color := setCoul(FcoulTEXTEOFF);
  end;
  Canvas.Font.Family:=TextSettings.Font.Family;
  Canvas.Font.Size:=TextSettings.Font.Size;
  Canvas.Fill.Color :=TextSettings.FontColor;
  Canvas.Font.Style:=TextSettings.Font.Style;

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
  Canvas.Fill.Color := setCoul(FcoulBORD);
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

function TPoussoir.GetDefaultTextSettings: TTextSettings;
begin
  Result := FTextSettingsInfo.DefaultTextSettings;
end;

function TPoussoir.GetTextSettings: TTextSettings;
begin
  Result := FTextSettingsInfo.TextSettings;
end;

procedure TPoussoir.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettingsInfo.TextSettings.Assign(Value);
end;

function TPoussoir.GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass;
begin
  Result := TTextControlTextSettings;
end;

end.
