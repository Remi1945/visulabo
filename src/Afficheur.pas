unit Afficheur;

interface

uses
  System.SysUtils, System.Classes, System.Types, FMX.Types, FMX.Controls,
  FMX.Objects, FMX.Graphics, System.UITypes, System.Generics.Collections,
  System.UIConsts, System.Math.Vectors;

const
  Lcurs = 8;
  Lcurs2 = 4;

type
  TGenre = (Horizontale, Verticale);
  TCadran = (Blanc, DegradeNoirClair, DegradeNoirFonce, Custom);
  TLstAvecRef = TDictionary<integer, TBitmap>;

  TAfficheur = class(TRectangle)
  private
    lstindex: array of integer;
    Findex: integer;
    FGenre: TGenre;
    FFactReduc: integer;
    FNbImg: integer;
    FLstAvecRef: boolean;
    FBmpLst: TList;
    FRefBmpLst: TLstAvecRef;
    FCadreCentral: boolean;
    FCurseur: boolean;
    y0curs, y1curs: Single;

    procedure SetIndex(Value: integer);
    function GetIndex: integer;
    function getRef: integer;
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
    procedure setListe(lst: TList); overload;
    procedure setListe(lst: TLstAvecRef); overload;
    procedure videListe;
    function getImage(n: integer): TBitmap; overload;
    function getImage: TBitmap; overload;
    function getNbTotalImage: integer;
    function ajouteImg(bmp: TBitmap): boolean; overload;
    function ajouteImg(ref: integer; bmp: TBitmap): boolean; overload;

  published

    property Genre: TGenre read FGenre write FGenre;
    property CadreCentral: boolean read FCadreCentral write setCadreCentral;
    property GliciereCurseur: boolean read FCurseur write setGliciereCurseur;
    property ReductionImages: integer read FFactReduc write SetFactReduc;
    property ListeAvecReference: boolean read FLstAvecRef write FLstAvecRef;
    property NombreImagettes: integer read FNbImg write SetNbImagettes;
    property Index: integer read GetIndex write SetIndex;
    property Reference: integer read getRef;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TAfficheur]);
end;

function TAfficheur.getImage(n: integer): TBitmap;
var
  bmp: TBitmap;
begin
  if FLstAvecRef then
  begin
    if (FRefBmpLst.Keys.Count = 0) then
      result := nil
    else
    begin
      if (n >= 0) and (n < FRefBmpLst.Keys.Count) and FRefBmpLst.TryGetValue(lstindex[n], bmp) then
        result := bmp
      else
        result := nil;
    end;
  end
  else
  begin
    if (FBmpLst.Count = 0) or (n < 0) or (n >= FBmpLst.Count) then
      result := nil
    else
      result := FBmpLst[n];
  end;
end;

function TAfficheur.GetIndex: integer;
begin
  if FLstAvecRef then
  begin
    if (FRefBmpLst = nil) or (FRefBmpLst.Keys.Count = 0) then
      result := 0
    else
      result := Findex;
  end
  else
  begin
    if (FBmpLst = nil) or (FBmpLst.Count = 0) then
      result := 0
    else
      result := Findex;
  end;
end;

function TAfficheur.getNbTotalImage: integer;
begin
  if FLstAvecRef then
    result := FRefBmpLst.Keys.Count
  else
    result := FBmpLst.Count;
end;

function TAfficheur.getPosCurseur: Single;
var
  pc, L: Single;
  nbelem, indx: integer;
begin
  case Genre of
    Horizontale:
      L := Width - 2 * Lcurs;
    Verticale:
      L := Height - 2 * Lcurs;
  end;
  if FLstAvecRef then
  begin
    if (FRefBmpLst.Keys.Count > 0) then
      pc := Lcurs + L * Findex / (FRefBmpLst.Keys.Count - 1)
    else
      pc := Lcurs;
  end
  else
  begin
    if (FBmpLst.Count > 0) then
      pc := Lcurs + L * Findex / (FBmpLst.Count - 1)
    else
      pc := Lcurs;
  end;

  result := pc;
end;

function TAfficheur.getRef: integer;
begin
  if FLstAvecRef and (FRefBmpLst.Keys.Count > 0) then
  begin
    result := lstindex[Findex];
  end
  else
    result := -1;
end;

procedure TAfficheur.Paint;

var
  rect, RectEcr: TRectF;
  Xc, Yc: Single;
  br: TBrush;
  pen: TStrokeBrush;
  i, j: integer;
  x1, x2, y1, y2: Single;

  mx, Himg, Wimg, HimgR, WimgR, Hcurs: Single;
  bmp: TBitmap;
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
  if (not(FLstAvecRef) and ((FBmpLst = nil) or (FBmpLst.Count = 0))) or
    (FLstAvecRef and ((FRefBmpLst = nil) or (FRefBmpLst.Keys.Count = 0))) then
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
    bmp := getImage(0);
    Himg := bmp.Height;
    if FCurseur then
    begin
      Himg := Height - 12 - 16;
      Yc := Himg / 2 + 4;
    end
    else
      Yc := Himg / 2 + 4;
    HimgR := Himg * FFactReduc / 100;
    Wimg := bmp.Width;
    WimgR := Wimg * FFactReduc / 100;
    mx := (Width - (2 * FNbImg * FFactReduc / 100 + 1) * Wimg) / (FNbImg * 2 + 1 + 1);
    y1 := Yc - HimgR / 2;
    y2 := Yc + HimgR / 2;
    x1 := mx;
    indx := Findex - FNbImg;
    for i := 1 to 2 * FNbImg + 1 do
    begin
      bmp := getImage(indx);
      if i <> FNbImg + 1 then
      begin
        if bmp <> nil then
          Canvas.DrawBitmap(bmp, TRectF.Create(0, 0, Wimg, Himg), TRectF.Create(x1, y1, x1 + WimgR, y2), 1, false);
        x1 := x1 + Wimg * FFactReduc / 100 + mx;
      end
      else
      begin
        if bmp <> nil then
          Canvas.DrawBitmap(bmp, TRectF.Create(0, 0, Wimg, Himg), TRectF.Create(x1, Yc - Himg / 2, x1 + Wimg, Yc + Himg / 2),
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
      y0curs := 4 + Himg + 4 + Hcurs / 4;
      y1curs := y0curs + Hcurs / 2;
      Canvas.Fill.Color := ClaGray;
      Canvas.FillRect(TRectF.Create(0, y0curs, Width, y1curs), 0, 0, AllCorners, 1, TCornerType.Round);
      Canvas.Fill.Color := ClaBlack;
      y0curs := y0curs + Hcurs / 6;
      y1curs := y0curs + Hcurs / 6;
      Canvas.FillRect(TRectF.Create(Lcurs, y0curs, Width - Lcurs, y1curs), 0, 0, AllCorners, 1, TCornerType.Round);
      // Dessin des curseurs
      Xc := getPosCurseur;
      Canvas.Fill.Color := ClaGray;
      y0curs := 4 + Himg + 4;
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

function TAfficheur.ajouteImg(ref: integer; bmp: TBitmap): boolean;
var
  L: integer;
begin
  if FLstAvecRef and FRefBmpLst.TryAdd(ref, bmp) then
  begin
    L := length(lstindex);
    setlength(lstindex, L + 1);
    lstindex[L] := ref;
    Repaint;
    result := true;
  end
  else
    result := false;

end;

function TAfficheur.ajouteImg(bmp: TBitmap): boolean;
begin
  if not(FLstAvecRef) then
  begin
    FBmpLst.Add(bmp);
    Repaint;
    result := true;
  end
  else
    result := false;
end;

constructor TAfficheur.Create(AOwner: TComponent);
begin
  inherited;
  FGenre := Horizontale;
  FFactReduc := 70;
  Findex := 0;
  FNbImg := 2;
  FBmpLst := TList.Create;
  FLstAvecRef := false;
  FRefBmpLst := TLstAvecRef.Create;
  FCadreCentral := false;
  setlength(lstindex, 0);
  FCurseur := false;
end;

function TAfficheur.getImage: TBitmap;
begin
  result := getImage(Findex);
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
    if FLstAvecRef then
    begin
      if (Value >= 0) and (Value < FRefBmpLst.Keys.Count) then
        Findex := Value;
    end
    else
    begin
      if (Value >= 0) and (Value < FBmpLst.Count) then
        Findex := Value;
    end;
    Repaint;
  end;

end;

procedure TAfficheur.setListe(lst: TLstAvecRef);
var
  n, i: integer;
  bmp: TBitmap;
begin
  FRefBmpLst.Clear;
  if lst <> nil then
  begin
    setlength(lstindex, lst.Keys.Count);
    n := 0;
    for i in lst.Keys do
    begin
      if lst.TryGetValue(i, bmp) then
      begin
        FRefBmpLst.Add(i, bmp);
        lstindex[n] := i;
        inc(n);
      end;
    end;
  end;
  Findex := 0;
  Repaint;
end;

procedure TAfficheur.setListe(lst: TList);
var
  i: integer;
begin
  FBmpLst.Clear;
  if lst <> nil then
  begin
    for i := 0 to lst.Count - 1 do
    begin
      FBmpLst.Add(lst[i]);
    end;
  end;
  Findex := 0;
  Repaint;
end;

procedure TAfficheur.videListe;
begin
  FBmpLst.Clear;
  FRefBmpLst.Clear;
  setlength(lstindex, 0);
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
  n, nbmax: integer;
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
  nbmax := 0;
  if not(FLstAvecRef) and (FBmpLst.Count > 0) then
    nbmax := FBmpLst.Count;
  if FLstAvecRef and (FRefBmpLst.Keys.Count > 0) then
    nbmax := FRefBmpLst.Keys.Count;
  if nbmax > 0 then
    n := Round((pc - Lcurs) / L * nbmax)
  else
    n := 0;
  result := n;
end;

procedure TAfficheur.MouseMove(Shift: TShiftState; x, y: Single);

begin
  if FCurseur and (ssLeft in Shift) then
    Index := getValCurseur(x, y);
end;

procedure TAfficheur.MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Single);

begin
  if FCurseur and (y >= y0curs) and (y <= y1curs) then
    Index := getValCurseur(x, y);
end;

procedure TAfficheur.MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: Single);
var
  dx: Single;
  n: integer;
begin
  if (FCurseur and (y < y0curs)) or not(FCurseur) then
  begin
    dx := Width / (2 * FNbImg + 1);
    n := trunc(x / dx) - FNbImg;
    Index := Index + n;
  end;
end;

// initialization

// FMX.Types.GlobalUseGPUCanvas := true;

end.
