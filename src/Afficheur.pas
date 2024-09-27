unit Afficheur;

interface

uses
  System.SysUtils, System.Classes, System.Types, FMX.Types, FMX.Controls,
  FMX.Objects, FMX.Graphics, System.UITypes,
  System.UIConsts, System.Math.Vectors;

type
  TGenre = (Horizontale, Vertcale);
  TCadran = (Blanc, DegradeNoirClair, DegradeNoirFonce, Custom);

  TAfficheur = class(TRectangle)
  private

    Findex: integer;
    FGenre: TGenre;
    FFactReduc: integer;
    FNbImg: integer;
    FBmpLst: TList;
    FCadreCentral: boolean;

    procedure SetIndex(Value: integer);
    function GetIndex: integer;
    procedure SetFactReduc(Value: integer);
    procedure SetNbImagettes(Value: integer);
    procedure setCadreCentral(Value: boolean);
  protected
    { Déclarations protégées }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure setListe(lst: TList);

  published

    property Genre: TGenre read FGenre write FGenre;
    property CadreCentral: boolean read FCadreCentral write setCadreCentral;
    property ReductionImages: integer read FFactReduc write SetFactReduc;

    property NombreImagettes: integer read FNbImg write SetNbImagettes;
    property Index: integer read GetIndex write SetIndex;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TAfficheur]);
end;

function TAfficheur.GetIndex: integer;
begin
  if (FBmpLst = nil) or (FBmpLst.Count = 0) then
    result := 0
  else
    result := Findex;
end;

procedure TAfficheur.Paint;

var
  rect, RectEcr: TRectF;
  Xc, Yc: Single;
  br: TBrush;
  pen: TStrokeBrush;
  i, j: integer;
  x1, x2, y1, y2: Single;
  mx, Himg, Wimg, HimgR, WimgR: Single;
  Bmp: TBitmap;
  indx: integer;
begin

  Canvas.BeginScene;
  // Fond transparent
  br := TBrush.Create(TBrushKind.Solid, 0);
  rect := TRectF.Create(0, 0, Width, Height);
  Canvas.FillRect(rect, 0, 0, AllCorners, 100, br);
  br.Free;
  Canvas.DrawRect(rect, 0, 0, AllCorners, 100, TCornerType.Round);
  Xc := Width / 2;
  Yc := Height / 2;
  if (FBmpLst = nil) or (FBmpLst.Count = 0) then
  begin

    Himg := Height - 8;
    Wimg := Himg;
    mx := (Width - (2 * FNbImg * FFactReduc / 100 + 1) * Wimg) / (FNbImg * 2 + 1 + 1);
    if mx < 0 then
    begin
      mx := 4;
      Wimg := (Width - mx * (FNbImg * 2 + 1 + 1)) / (2 * FNbImg * FFactReduc / 100 + 1);
    end;
    y1 := Yc - Himg * FFactReduc / 100 / 2;
    y2 := Yc + Himg * FFactReduc / 100 / 2;
    x1 := mx;
    Canvas.Fill.Color := claLightGray;
    for i := 1 to 2 * FNbImg + 1 do
    begin
      if i <> FNbImg + 1 then
      begin
        x2 := x1 + Wimg * FFactReduc / 100;
        Canvas.FillRect(TRectF.Create(x1, y1, x2, y2), 0, 0, AllCorners, 100);
        x1 := x1 + Wimg * FFactReduc / 100 + mx;
      end
      else
        x1 := x1 + Wimg + mx;
    end;
    y1 := Yc - Himg / 2;
    y2 := Yc + Himg / 2;
    x1 := Xc - Wimg / 2;
    x2 := x1 + Wimg;
    Canvas.FillRect(TRectF.Create(x1, y1, x2, y2), 0, 0, AllCorners, 100);
    if FCadreCentral then
      Canvas.DrawRect(TRectF.Create(x1, y1, x2, y2), Wimg / 8, Himg / 8, AllCorners, 100);
  end
  else
  begin
    Bmp := FBmpLst.First;
    Himg := Bmp.Height;
    HimgR := Himg * FFactReduc / 100;
    Wimg := Bmp.Width;
    WimgR := Wimg * FFactReduc / 100;
    mx := (Width - (2 * FNbImg * FFactReduc / 100 + 1) * Wimg) / (FNbImg * 2 + 1 + 1);
    y1 := Yc - HimgR / 2;
    y2 := Yc + HimgR / 2;
    x1 := mx;
    indx := Findex - FNbImg;
    for i := 1 to 2 * FNbImg + 1 do
    begin
      if (indx >= 0) and (indx < FBmpLst.Count) then
        Bmp := FBmpLst.Items[indx]
      else
        Bmp := nil;

      if i <> FNbImg + 1 then
      begin
        if Bmp <> nil then
          Canvas.DrawBitmap(Bmp, TRectF.Create(0, 0, Wimg, Himg), TRectF.Create(x1, y1, x1 + WimgR, y2), 1, false);
        x1 := x1 + Wimg * FFactReduc / 100 + mx;
      end
      else
      begin
        if Bmp <> nil then
          Canvas.DrawBitmap(Bmp, TRectF.Create(0, 0, Wimg, Himg), TRectF.Create(x1, Yc - Himg / 2, x1 + Wimg,
            Yc + Himg / 2), 1, false);
        if FCadreCentral then
          Canvas.DrawRect(TRectF.Create(x1, Yc - Himg / 2, x1 + Wimg, Yc + Himg / 2), Wimg / 8, Himg / 8,
            AllCorners, 100);
        x1 := x1 + Wimg + mx;
      end;
      inc(indx);
    end;
  end;

  Canvas.EndScene;
end;

procedure TAfficheur.setCadreCentral(Value: boolean);
begin
  FCadreCentral := Value;
  Repaint;
end;

constructor TAfficheur.Create(AOwner: TComponent);
begin
  inherited;
  FGenre := Horizontale;
  FFactReduc := 70;
  Findex := 0;
  FNbImg := 2;
  FBmpLst := nil;
  FCadreCentral := false;
end;

procedure TAfficheur.SetFactReduc(Value: integer);
begin
  if Value < 0 then
    FFactReduc := 0
  else if Value > 100 then
    FFactReduc := 0
  else
    FFactReduc := Value;
end;

procedure TAfficheur.SetIndex(Value: integer);
begin
  if Findex <> Value then
  begin
    if (FBmpLst <> nil) and (Value >= 0) and (Value < FBmpLst.Count) then
      Findex := Value;
    Repaint;
  end;

end;

procedure TAfficheur.setListe(lst: TList);
begin
  FBmpLst := lst;
  Findex := 0;
  Repaint;
end;

procedure TAfficheur.SetNbImagettes(Value: integer);
begin
  if (Value >= 0) and (Value <= 5) then
    FNbImg := Value;
end;

// initialization

// FMX.Types.GlobalUseGPUCanvas := true;

end.
