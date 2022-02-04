unit testmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  JaugeCir, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    vue: TImage;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    JaugeCir1: TJaugeCir;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);

var
  path, FRAME_OUTERFRAME: TPathData;
begin
  path := TPathData.Create;
  {
    path.MoveTo(TPointF.Create(10, 100));
    path.CurveTo(TPointF.Create(100, 10), TPointF.Create(150, 150),
    TPointF.Create(200, 100));
  }
  path.moveTo(TPointF.Create(vue.Position.X + vue.Width * 0.9158878504672897,
    vue.Position.Y + vue.Height * 0.9158878504672897));
  path.curveTo(TPointF.Create(vue.Position.X + vue.Width * 0.9158878504672897,
    vue.Position.Y + vue.Height * 0.9158878504672897),
    TPointF.Create(vue.Position.X + vue.Width * 0.9158878504672897,
    vue.Position.Y + vue.Height * 0.08411214953271028),
    TPointF.Create(vue.Position.X + vue.Width * 0.9158878504672897,
    vue.Position.Y + vue.Height * 0.08411214953271028));
  path.curveTo(TPointF.Create(vue.Position.X + vue.Width * 0.6401869158878505,
    vue.Position.Y + vue.Height * 0.08411214953271028),
    TPointF.Create(vue.Position.X + vue.Width * 0.46261682242990654,
    vue.Position.Y + vue.Height * 0.1588785046728972),
    TPointF.Create(vue.Position.X + vue.Width * 0.29439252336448596,
    vue.Position.Y + vue.Height * 0.32242990654205606));
  path.curveTo(TPointF.Create(vue.Position.X + vue.Width * 0.17289719626168223,
    vue.Position.Y + vue.Height * 0.4439252336448598),
    TPointF.Create(vue.Position.X + vue.Width * 0.08411214953271028,
    vue.Position.Y + vue.Height * 0.6635514018691588),
    TPointF.Create(vue.Position.X + vue.Width * 0.08411214953271028,
    vue.Position.Y + vue.Height * 0.9158878504672897));
  path.curveTo(TPointF.Create(vue.Position.X + vue.Width * 0.08411214953271028,
    vue.Position.Y + vue.Height * 0.9158878504672897),
    TPointF.Create(vue.Position.X + vue.Width * 0.9158878504672897,
    vue.Position.Y + vue.Height * 0.9158878504672897),
    TPointF.Create(vue.Position.X + vue.Width * 0.9158878504672897,
    vue.Position.Y + vue.Height * 0.9158878504672897));
  path.closePath();
  FRAME_OUTERFRAME := TPathData.Create;

  FRAME_OUTERFRAME.moveTo(TPointF.Create(vue.Position.X + vue.Width * 1.0,
    vue.Position.Y + vue.Height * 1.0));
  FRAME_OUTERFRAME.curveTo(TPointF.Create(vue.Position.X + vue.Width * 1.0,
    vue.Position.Y + vue.Height * 1.0),
    TPointF.Create(vue.Position.X + vue.Width * 1.0, vue.Position.Y + vue.Height
    * 0.0), TPointF.Create(vue.Position.X + vue.Width * 1.0,
    vue.Position.Y + vue.Height * 0.0));
  FRAME_OUTERFRAME.curveTo(TPointF.Create(vue.Position.X + vue.Width *
    0.3644859813084112, vue.Position.Y + vue.Height * 0.0),
    TPointF.Create(vue.Position.X + vue.Width * 0.0, vue.Position.Y + vue.Height
    * 0.308411214953271), TPointF.Create(vue.Position.X + vue.Width * 0.0,
    vue.Position.Y + vue.Height * 1.0));
  FRAME_OUTERFRAME.curveTo(TPointF.Create(vue.Position.X + vue.Width * 0.0,
    vue.Position.Y + vue.Height * 1.0),
    TPointF.Create(vue.Position.X + vue.Width * 1.0, vue.Position.Y + vue.Height
    * 1.0), TPointF.Create(vue.Position.X + vue.Width * 1.0,
    vue.Position.Y + vue.Height * 1.0));
  FRAME_OUTERFRAME.closePath();
  vue.Canvas.Stroke.Thickness := 2;
  vue.Canvas.Stroke.Color := $FFFF0000;
  vue.Canvas.BeginScene;
  vue.Canvas.Stroke.Color := $FFFF0000;
  vue.Canvas.DrawPath(path, 1.0);
  vue.Canvas.Stroke.Color := $FF0000FF;
  vue.Canvas.DrawPath(FRAME_OUTERFRAME, 1.0);
  vue.Canvas.EndScene;

  path.Free;

end;

procedure TForm1.Button2Click(Sender: TObject);
var
  bmp: TBitmap;
  X, Y: integer;
  dta: TBitmapData;
  r, g, b: array of integer;
  coul: TAlphaColor;
  fic: Text;
begin
  bmp := TBitmap.Create;
  bmp.LoadFromFile('D:\gouyon\avirer\laiton.bmp');
  setLength(r, bmp.Width);
  setLength(g, bmp.Width);
  setLength(b, bmp.Width);
  if bmp.Map(TMapAccess.Read, dta) then
  begin
    for X := 0 to bmp.Width - 1 do
    begin

      r[X] := 0;
      g[X] := 0;
      b[X] := 0;
      for Y := 0 to 9 do
      begin
        coul := dta.GetPixel(X, Y * bmp.Height div 10);
        r[X] := r[X] + TAlphaColorRec(coul).r;
        g[X] := g[X] + TAlphaColorRec(coul).g;
        b[X] := b[X] + TAlphaColorRec(coul).b;
      end;
      r[X] := r[X] div 10;
      g[X] := g[X] div 10;
      b[X] := b[X] div 10;
    end;
    bmp.Unmap(dta);
  end;
  AssignFile(fic, 'D:\gouyon\avirer\coullaiton.csv');
  rewrite(fic);
  for X := 0 to bmp.Width - 1 do
    writeln(fic, Format('%d;%d;%d', [r[X], g[X], b[X]]));
  CloseFile(fic);
end;

procedure TForm1.Button3Click(Sender: TObject);
const
  vr: array [0 .. 8] of byte = (136, 124, 231, 216, 203, 142, 149, 243, 131);
  vg: array [0 .. 8] of byte = (108, 99, 194, 179, 165, 112, 120, 209, 102);
  vb: array [0 .. 8] of byte = (45, 35, 124, 111, 103, 40, 50, 137, 32);
  fracs: array [0 .. 8] of Single = (0, 0.139116203, 0.291325696, 0.340425532,
    0.425531915, 0.602291326, 0.669394435, 0.818330606, 1);
var
  bmp: TBitmap;
  X, Y, n: integer;
  dta: TBitmapData;
  coul: TAlphaColor;
  xc, yc, d, dmax, dc: Single;
  rr, gg, bb: Single;
  periode: integer;
  Lperiode: Single;
begin
  bmp := TBitmap.Create(round(vue.Width), round(vue.Height));
  periode := 1;
  xc := vue.Width / 4;
  yc := vue.Height / 3;
  dmax := xc * xc + yc * yc;
  d := (xc - vue.Width) * (xc - vue.Width) + yc * yc;
  if d > dmax then
    dmax := d;
  d := (xc - vue.Width) * (xc - vue.Width) + (yc - vue.Height) *
    (yc - vue.Height);
  if d > dmax then
    dmax := d;
  d := xc * xc + (yc - vue.Height) * (yc - vue.Height);
  if d > dmax then
    dmax := d;
  dmax := sqrt(dmax);
  Lperiode := dmax / periode;
  randomize;
  if bmp.Map(TMapAccess.Write, dta) then
  begin
    for Y := 0 to bmp.Height - 1 do
    begin
      for X := 0 to bmp.Width - 1 do
      begin
        d := sqrt((X - xc) * (X - xc) + (Y - yc) * (Y - yc));
        if periode = 1 then
          d := d / dmax
        else
        begin
          while (d > Lperiode) do
            d := d - Lperiode;
          d := d / Lperiode;
        end;
        n := 0;
        while (d > fracs[n]) and (n < 9) do
          inc(n);
        dc := (d - fracs[n - 1]) / (fracs[n] - fracs[n - 1]);
        dc:=sin(pi/2*dc);
        rr := (vr[n] - vr[n - 1]) * dc + vr[n - 1];
        rr := rr + 5 - random(11);
        gg := (vg[n] - vg[n - 1]) * dc + vg[n - 1];
        gg := gg + 5 - random(11);
        bb := (vb[n] - vb[n - 1]) * dc + vb[n - 1];
        bb := bb + 5 - random(11);
        coul := $FF000000 + round(rr) shl 16 + round(gg) shl 8 + round(bb);
        dta.SetPixel(X, Y, coul);
      end;
    end;
    bmp.Unmap(dta);
  end;
  vue.Bitmap.Assign(bmp);
  bmp.Free;
end;

procedure TForm1.Button4Click(Sender: TObject);
const
  vr: array [0 .. 8] of byte = (136, 124, 231, 216, 203, 142, 149, 243, 131);
  vg: array [0 .. 8] of byte = (108, 99, 194, 179, 165, 112, 120, 209, 102);
  vb: array [0 .. 8] of byte = (45, 35, 124, 111, 103, 40, 50, 137, 32);
  fracs: array [0 .. 8] of Single = (0, 0.139116203, 0.291325696, 0.340425532,
    0.425531915, 0.602291326, 0.669394435, 0.818330606, 1);
var
  bmp: TBitmap;
  X, Y, n: integer;
  dta: TBitmapData;
  coul: TAlphaColor;
  xc, yc, dc: Single;
  dx, dy, alpha: Single;
  rr, gg, bb: Single;


begin
  alpha := arctan(-1);
  bmp := TBitmap.Create(round(vue.Width), round(vue.Height));
  xc := vue.Width / 4;
  yc := vue.Height / 3;

  randomize;
  if bmp.Map(TMapAccess.Write, dta) then
  begin
    for Y := 0 to bmp.Height - 1 do
    begin
      for X := 0 to bmp.Width - 1 do
      begin
        dx := X - xc;
        dy := Y - yc;
        if dx = 0 then
        begin
          if dy < 0 then
            alpha := 3 * PI / 4
          else
            alpha := PI / 4;
        end
        else
        begin
          alpha := arctan(dy / dx);
          if dx < 0 then
            alpha := alpha + PI;
          if (dx > 0) and (dy < 0) then
            alpha := alpha + 2 * PI;
        end;
        alpha := alpha / 2 / PI;

        n := 0;
        while (alpha > fracs[n]) and (n < 9) do
          inc(n);
        dc := (alpha - fracs[n - 1]) / (fracs[n] - fracs[n - 1]);
        dc:=sin(pi/2*dc);
        rr := (vr[n] - vr[n - 1]) * dc + vr[n - 1];
        rr := rr + 5 - random(11);
        gg := (vg[n] - vg[n - 1]) * dc + vg[n - 1];
        gg := gg + 5 - random(11);
        bb := (vb[n] - vb[n - 1]) * dc + vb[n - 1];
        bb := bb + 5 - random(11);
        coul := $FF000000 + round(rr) shl 16 + round(gg) shl 8 + round(bb);
        dta.SetPixel(X, Y, coul);
      end;
    end;
    bmp.Unmap(dta);
  end;
  vue.Bitmap.Assign(bmp);
  bmp.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  dx, dy, a: double;
begin
  dx := -2;
  dy := -3;
  a := arctan(dy / dx) * 180 / PI;

end;

end.
