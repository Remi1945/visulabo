unit Afficheur;

interface

uses
  System.SysUtils, System.Classes, System.Types, FMX.Types, FMX.Controls,
  FMX.Objects, FMX.Graphics, System.UITypes,
  System.UIConsts, System.Math.Vectors;

const
  Lcurs = 8;
  Lcurs2 = 4;

type
  TGenre = (Horizontale, Verticale);
  TCadran = (Blanc, DegradeNoirClair, DegradeNoirFonce, Custom);

  TAfficheur = class(TRectangle)
  private

    Findex: integer;
    FGenre: TGenre;
    FFactReduc: integer;
    FNbImg: integer;
    FBmpLst: TList;
    FCadreCentral: boolean;
    FCurseur: boolean;

    procedure SetIndex(Value: integer);
    function GetIndex: integer;
    procedure SetFactReduc(Value: integer);
    procedure SetNbImagettes(Value: integer);
    procedure setCadreCentral(Value: boolean);
    procedure setGliciereCurseur(Value: boolean);
    function getPosCurseur: Single;
    function getValCurseur(x, y: Single): integer;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Single); override;
    procedure MouseMove(Shift: TShiftState; x, y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure setListe(lst: TList);

  published

    property Genre: TGenre read FGenre write FGenre;
    property CadreCentral: boolean read FCadreCentral write setCadreCentral;
    property GliciereCurseur: boolean read FCurseur write setGliciereCurseur;
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

function TAfficheur.getPosCurseur: Single;

var
  pc, L: Single;
begin
  case Genre of
    Horizontale:
      L := Width - 2 * Lcurs;
    Verticale:
      L := Height - 2 * Lcurs;
  end;
  if (FBmpLst <> nil) and (FBmpLst.Count > 0) then

    pc := Lcurs + L * Findex / FBmpLst.Count
  else
    pc := Lcurs;

  result := pc;
end;

procedure TAfficheur.MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: Single);
begin
  inherited;

end;

procedure TAfficheur.Paint;

var
  rect, RectEcr: TRectF;
  Xc, Yc: Single;
  br: TBrush;
  pen: TStrokeBrush;
  i, j: integer;
  x1, x2, y1, y2: Single;
  y0curs, y1curs: Single;
  mx, Himg, Wimg, HimgR, WimgR, Hcurs: Single;
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
  if (FBmpLst = nil) or (FBmpLst.Count = 0) then
  begin
    Himg := Height - 8;
    Yc := Height / 2;
    if FCurseur then
    begin
      Himg := Height - 12 - 16;
      Yc := Himg / 2 + 4;
    end;
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
    if FCurseur then
    begin
      Himg := Height - 12 - 16;
      Yc := Himg / 2 + 4;
    end;
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
          Canvas.DrawBitmap(Bmp, TRectF.Create(0, 0, Wimg, Himg), TRectF.Create(x1, Yc - Himg / 2, x1 + Wimg, Yc + Himg / 2),
            1, false);
        if FCadreCentral then
          Canvas.DrawRect(TRectF.Create(x1, Yc - Himg / 2, x1 + Wimg, Yc + Himg / 2), Wimg / 8, Himg / 8, AllCorners, 100);
        x1 := x1 + Wimg + mx;
      end;
      inc(indx);
    end;
  end;
  if FCurseur then
  begin
    if FGenre = Verticale then
    begin
      // Fond de glissière
      Canvas.Fill.Color := ClaGray;
      Canvas.FillRect(TRectF.Create(Width / 4, 0, 3 * Width / 4, Height), 0, 0, AllCorners, 1, TCornerType.Round);
      Canvas.Fill.Color := ClaBlack;
      Canvas.FillRect(TRectF.Create(Width * (1 / 4 + 1 / 6), Lcurs, Width * (3 / 4 - 1 / 6), Height - Lcurs), 0, 0, AllCorners, 1,
        TCornerType.Round);
      // Dessin des curseurs
      Yc := getPosCurseur;
      Canvas.Fill.Color := ClaGray;
      Canvas.FillRect(TRectF.Create(0, Yc - Lcurs, Width, Yc + Lcurs), 0, 0, AllCorners, 1, TCornerType.Round);
      Canvas.Fill.Color := ClaBlue;
      Canvas.FillRect(TRectF.Create(Lcurs2, Yc - Lcurs2, Width - Lcurs2, Yc + Lcurs2), 0, 0, AllCorners, 1, TCornerType.Round);
    end;

    if FGenre = Horizontale then
    begin
      Hcurs := 16;
      // Fond de glissière
      y0curs := 4 + Himg + 4+Hcurs / 4;
      y1curs := y0curs +  Hcurs / 2;
      Canvas.Fill.Color := ClaGray;
      Canvas.FillRect(TRectF.Create(0, y0curs, Width, y1curs), 0, 0, AllCorners, 1, TCornerType.Round);
      Canvas.Fill.Color := ClaBlack;
      y0curs := y0curs + Hcurs / 6;
      y1curs := y0curs +  Hcurs / 6;
      Canvas.FillRect(TRectF.Create(Lcurs, y0curs, Width - Lcurs, y1curs), 0, 0,AllCorners, 1, TCornerType.Round);
      // Dessin des curseurs
      Xc := getPosCurseur;
      Canvas.Fill.Color := ClaGray;
      y0curs := 4 + Himg+4 ;
      y1curs := y0curs + Hcurs;
      Canvas.FillRect(TRectF.Create(Xc - Lcurs, y0curs, Xc + Lcurs, y1curs), 0, 0, AllCorners, 1, TCornerType.Round);
      Canvas.Fill.Color := ClaBlue;
      Canvas.FillRect(TRectF.Create(Xc - Lcurs2, y0curs + Lcurs2, Xc + Lcurs2, y1curs - Lcurs2), 0, 0, AllCorners, 1,
        TCornerType.Round);
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
  FCurseur := false;
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

procedure TAfficheur.setGliciereCurseur(Value: boolean);
begin
  FCurseur := Value;
  Repaint;
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

function TAfficheur.getValCurseur(x, y: Single): integer;
var
  pc, L: Single;
  n: integer;
begin
  case Genre of
    Horizontale:
      begin
        L := Width - 2 * Lcurs;
        pc := x;
      end;
    Verticale:
      begin
        L := Height - 2 * Lcurs;
        pc := y;
      end;
  end;
  if (FBmpLst <> nil) and (FBmpLst.Count > 0) then
    n := Round((pc - Lcurs) / L * FBmpLst.Count)
  else
    n := 0;
  result := n;
end;

procedure TAfficheur.MouseMove(Shift: TShiftState; x, y: Single);

begin
  if ssLeft in Shift then
    Index := getValCurseur(x, y);
end;

procedure TAfficheur.MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Single);

begin
  Index := getValCurseur(x, y);
end;


// initialization

// FMX.Types.GlobalUseGPUCanvas := true;

end.
