unit Interrupteur;

interface

uses
  System.SysUtils, Couleurs, System.Classes, System.Types, System.UIConsts, FMX.Types, FMX.Controls, FMX.Objects,
  FMX.Graphics, System.Math.Vectors;

type
  TGenre = (Circulaire, Horizontal, Vertical);

  TInterrupteur = class(TRectangle)
  private
    FGenre: TGenre;
    Fon: boolean;

  protected
    { Déclarations protégées }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property Genre: TGenre read FGenre write FGenre;
    property Contacte: boolean read Fon write Fon;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TInterrupteur]);
end;

constructor TInterrupteur.Create(AOwner: TComponent);
begin
  inherited;
  FGenre := Horizontal;

end;

procedure TInterrupteur.Paint;
var
  rect: TRectF;
  pas: Single;
  pol: TPolygon;
  L1, L2: Single;
begin
  setLength(pol, 5);
  Canvas.BeginScene;
  if FGenre = Horizontal then
  begin
    pas := Width / 16;
    if Fon then
    begin
      L1 := -7 * pas;
      L2 := -7.5 * pas
    end
    else
    begin
      L1 := 7 * pas;
      L2 := 7.5 * pas
    end;
    Canvas.Fill.Color := claBlack;
    pol[0] := TPointF.Create(Width / 2 + L1, Height / 2 - 3 * pas);
    pol[1] := TPointF.Create(Width / 2 + L2, Height / 2 - 2.5 * pas);
    pol[2] := TPointF.Create(Width / 2 + L2, Height / 2 + 2.5 * pas);
    pol[3] := TPointF.Create(Width / 2 + L1, Height / 2 + 3 * pas);
    pol[4] := pol[0];
    Canvas.FillPolygon(pol, 1);
    pol[0] := TPointF.Create(Width / 2, Height / 2 - pas);
    pol[1] := TPointF.Create(Width / 2 + L1, Height / 2 - 3 * pas);
    pol[2] := TPointF.Create(Width / 2 + L1, Height / 2 + 3 * pas);
    pol[3] := TPointF.Create(Width / 2, Height / 2 + pas);
    pol[4] := pol[0];
    Canvas.Fill.Gradient.Color1 := claWhite;
    Canvas.Fill.Gradient.Color := claBlack;
    Canvas.Fill.Gradient.Style := TGradientStyle.Linear;
    Canvas.Fill.Gradient.RadialTransform.RotationCenter.X := 0.1;
    Canvas.Fill.Gradient.RadialTransform.RotationCenter.Y := 0.2;
    Canvas.Fill.Gradient.RadialTransform.RotationAngle := 90;
    Canvas.Fill.Kind := TBrushKind.Gradient;
    Canvas.FillPolygon(pol, 1);

  end;
  Canvas.EndScene;
end;

end.
