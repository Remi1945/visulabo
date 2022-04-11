unit Couleurs;

interface

uses System.UITypes, System.SysUtils, System.Classes, System.Types;

const

  brass_vr: array [0 .. 8] of byte = (136, 124, 231, 216, 203, 142, 149, 243, 131);
  brass_vg: array [0 .. 8] of byte = (108, 99, 194, 179, 165, 112, 120, 209, 102);
  brass_vb: array [0 .. 8] of byte = (45, 35, 124, 111, 103, 40, 50, 137, 32);
  brass_fracs: array [0 .. 8] of Single = (0, 0.139116203, 0.291325696, 0.340425532, 0.425531915, 0.602291326,
    0.669394435, 0.818330606, 1);

  chrome_vr: array [0 .. 16] of byte = (255, 255, 136, 164, 158, 112, 221, 155, 156, 254, 255, 156, 198, 246, 204,
    164, 255);
  chrome_vg: array [0 .. 16] of byte = (255, 255, 136, 185, 179, 112, 227, 176, 176, 255, 255, 180, 209, 248, 216,
    188, 255);
  chrome_vb: array [0 .. 16] of byte = (255, 255, 138, 190, 182, 112, 227, 179, 177, 255, 255, 180, 211, 247, 216,
    191, 255);
  chrome_fracs: array [0 .. 16] of Single = (0, 0.09, 0.12, 0.16, 0.25, 0.29, 0.33, 0.38, 0.48, 0.52, 0.63, 0.68, 0.8,
    0.83, 0.87, 0.97, 1);

  blackmetal_vr: array [0 .. 6] of byte = (254, 0, 153, 0, 153, 0, 254);
  blackmetal_vg: array [0 .. 6] of byte = (254, 0, 153, 0, 153, 0, 254);
  blackmetal_vb: array [0 .. 6] of byte = (254, 0, 153, 0, 153, 0, 254);
  blackmetal_fracs: array [0 .. 6] of Single = (0, 0.125, 0.3472, 0.5, 0.680555, 0.875, 1);

  anthracite_vr: array [0 .. 3] of byte = (118, 74, 50, 97);
  anthracite_vg: array [0 .. 3] of byte = (117, 74, 50, 97);
  anthracite_vb: array [0 .. 3] of byte = (135, 82, 54, 108);
  anthracite_fracs: array [0 .. 3] of Single = (0, 0.06, 0.12, 1);

type
  TCouls = (Rouge, Jaune, Vert, Orange, Bleu, Gris, GrisClair, GrisFonce, Blanc, Noir, Transparent, Custom);
  TMatiere = (MAT_SANS, MAT_BRASS, MAT_CHROME, MAT_BLACKMETAL, MAT_ANTHRACITE);

function setCoul(tc: TCouls): TAlphaColor;
procedure getRGBCoul(tc: TCouls; var r, g, b: Single);

function getCoulMatiere(mat: TMatiere; L: Single): TAlphaColor;

implementation

function setCoul(tc: TCouls): TAlphaColor;
begin
  case tc of
    Rouge:
      result := $FFFF0000;
    Vert:
      result := $FF00FF00;
    Orange:
      result := $FFFF8000;
    Jaune:
      result := $FFFFFF00;
    Bleu:
      result := $FF0000FF;
    GrisFonce:
      result := $FF101010;
    Gris:
      result := $FF7F7F7F;
    GrisClair:
      result := $FFFAFAFA;
    Blanc:
      result := $FFFFFFFF;
    Noir:
      result := $FF000000;
    Transparent:
      result := $00000000;
  end;
end;

procedure getRGBCoul(tc: TCouls; var r, g, b: Single);
begin
  case tc of
    Rouge:
      begin
        r := 1;
        g := 0;
        b := 0;
      end;
    Vert:
      begin
        r := 0;
        g := 1;
        b := 0;
      end;
    Orange:
      begin
        r := 1;
        g := 0.5;
        b := 0;
      end;
    Jaune:
      begin
        r := 1;
        g := 1;
        b := 0;
      end;
    Bleu:
      begin
        r := 0;
        g := 0;
        b := 1;
      end;
    Gris:
      begin
        r := 0.627;
        g := 0.627;
        b := 0.627;
      end;
    Blanc:
      begin
        r := 1;
        g := 1;
        b := 1;
      end;
    Noir:
      begin
        r := 0;
        g := 0;
        b := 0;
      end;
    Transparent:
      begin
        r := 0;
        g := 0;
        b := 0;
      end;
  end;
end;

function getCoulMatiere(mat: TMatiere; L: Single): TAlphaColor;

var
  n: integer;
  coul: TAlphaColor;
  dc, rr, gg, bb: Single;

begin

  coul := $FF000000;
  if L < 0 then
  begin
    case mat of
      MAT_BRASS:
        coul := $FF000000 + brass_vr[0] shl 16 + brass_vg[0] shl 8 + brass_vb[0];
      MAT_CHROME:
        coul := $FF000000 + chrome_vr[0] shl 16 + chrome_vg[0] shl 8 + chrome_vb[0];
      MAT_BLACKMETAL:
        coul := $FF000000 + blackmetal_vr[0] shl 16 + blackmetal_vg[0] shl 8 + blackmetal_vb[0];
      MAT_ANTHRACITE:
        coul := $FF000000 + anthracite_vr[0] shl 16 + anthracite_vg[0] shl 8 + anthracite_vb[0];
    end;

  end
  else
  begin
    if L > 1 then
    begin
      case mat of

        MAT_BRASS:
          coul := $FF000000 + brass_vr[8] shl 16 + brass_vg[8] shl 8 + brass_vb[8];
        MAT_CHROME:
          coul := $FF000000 + chrome_vr[16] shl 16 + chrome_vg[16] shl 8 + chrome_vb[16];
        MAT_BLACKMETAL:
          coul := $FF000000 + blackmetal_vr[6] shl 16 + blackmetal_vg[6] shl 8 + blackmetal_vb[6];
        MAT_ANTHRACITE:
          coul := $FF000000 + anthracite_vr[3] shl 16 + anthracite_vg[3] shl 8 + anthracite_vb[3];
      end;

    end
    else
    begin
      n := 0;
      case mat of
        MAT_SANS:
          coul := $FF000000;

        MAT_BRASS:
          begin
            while (L > brass_fracs[n]) and (n < 9) do
              inc(n);
            dc := (L - brass_fracs[n - 1]) / (brass_fracs[n] - brass_fracs[n - 1]);
            dc := sin(PI / 2 * dc);
            rr := (brass_vr[n] - brass_vr[n - 1]) * dc + brass_vr[n - 1];
            gg := (brass_vg[n] - brass_vg[n - 1]) * dc + brass_vg[n - 1];
            bb := (brass_vb[n] - brass_vb[n - 1]) * dc + brass_vb[n - 1];
          end;
        MAT_CHROME:
          begin
            while (L > chrome_fracs[n]) and (n < 17) do
              inc(n);
            dc := (L - chrome_fracs[n - 1]) / (chrome_fracs[n] - chrome_fracs[n - 1]);
            dc := sin(PI / 2 * dc);
            rr := (chrome_vr[n] - chrome_vr[n - 1]) * dc + chrome_vr[n - 1];
            gg := (chrome_vg[n] - chrome_vg[n - 1]) * dc + chrome_vg[n - 1];
            bb := (chrome_vb[n] - chrome_vb[n - 1]) * dc + chrome_vb[n - 1];
          end;
        MAT_BLACKMETAL:
          begin
            while (L > blackmetal_fracs[n]) and (n < 7) do
              inc(n);
            dc := (L - blackmetal_fracs[n - 1]) / (blackmetal_fracs[n] - blackmetal_fracs[n - 1]);
            dc := sin(PI / 2 * dc);
            rr := (blackmetal_vr[n] - blackmetal_vr[n - 1]) * dc + blackmetal_vr[n - 1];
            gg := (blackmetal_vg[n] - blackmetal_vg[n - 1]) * dc + blackmetal_vg[n - 1];
            bb := (blackmetal_vb[n] - blackmetal_vb[n - 1]) * dc + blackmetal_vb[n - 1];
          end;
        MAT_ANTHRACITE:
          begin
            while (L > anthracite_fracs[n]) and (n < 4) do
              inc(n);
            dc := (L - anthracite_fracs[n - 1]) / (anthracite_fracs[n] - anthracite_fracs[n - 1]);
            dc := sin(PI / 2 * dc);
            rr := (anthracite_vr[n] - anthracite_vr[n - 1]) * dc + anthracite_vr[n - 1];
            gg := (anthracite_vg[n] - anthracite_vg[n - 1]) * dc + anthracite_vg[n - 1];
            bb := (anthracite_vb[n] - anthracite_vb[n - 1]) * dc + anthracite_vb[n - 1];
          end;
      end;

      coul := $FF000000 + round(rr) shl 16 + round(gg) shl 8 + round(bb);
    end;
  end;
  result := coul;
end;

end.
