unit Couleurs;

interface

uses System.UITypes;

type
  TCouls = (Rouge, Jaune, Vert, Orange, Bleu, Gris, Blanc, Noir, Transparent, Custom);
function setCoul(tc: TCouls): TAlphaColor;

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
    Gris:
      result := $FF101010;
    Blanc:
      result := $FFFFFFFF;
    Noir:
      result := $FF000000;
    Transparent:
      result := $00000000;
  end;
end;

end.
