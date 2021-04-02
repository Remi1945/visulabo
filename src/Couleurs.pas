unit Couleurs;

interface

uses System.UITypes;

type
  TCouls = (Rouge, Jaune, Vert, Orange, Bleu, Gris, GrisClair, GrisFonce, Blanc, Noir, Transparent, Custom);

function setCoul(tc: TCouls): TAlphaColor;
procedure getRGBCoul(tc: TCouls; var r, g, b: Single);

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

end.
