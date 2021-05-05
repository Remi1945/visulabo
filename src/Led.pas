unit Led;

interface

uses
  System.SysUtils,Couleurs, System.Classes, System.Types, System.UIConsts,
  FMX.Types, FMX.Controls, FMX.Objects, FMX.Graphics,
  Data.Bind.Components,System.Bindings.Outputs, System.rtti;

type

  [ObservableMember('LedOn')]
  TLed = class(TCircle)
  private
    FcoulON: TCouls;
    FLedON: boolean;
    procedure setLedON(value: boolean);
  protected
    { Déclarations protégées }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property CouleurON: TCouls read FcoulON write FcoulON;
    property ledON: boolean read FLedON write setLedON default true;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TLed]);
end;

constructor TLed.Create(AOwner: TComponent);
begin
  inherited;
  FcoulON := Rouge;
  FLedON := false;
end;

procedure TLed.Paint;
var
  rayon: Single;
  rect: TRectF;
begin
  rayon := Width;
  if Height < rayon then
    rayon := Height;
  rayon := rayon / 2;
  Canvas.BeginScene;
  Canvas.Stroke.Thickness := rayon / 10;
  Fill.Kind := TBrushKind.Gradient;
  Fill.Gradient.Style := TGradientStyle.Radial;
  if ledON then
  begin
    Fill.Gradient.Color := setCoul(CouleurON);
  end
  else
    Fill.Gradient.Color := claGray;
  Fill.Gradient.Color1 := claWhite;
  Fill.Gradient.RadialTransform.RotationCenter.X := 0.7;
  Fill.Gradient.RadialTransform.RotationCenter.Y := 0.2;
  rect.Left := Width / 2 - rayon;
  rect.Right := Width / 2 + rayon;
  rect.Top := Height / 2 - rayon;
  rect.Bottom := Height / 2 + rayon;

  Canvas.FillEllipse(rect, 1, Fill);
  Canvas.DrawEllipse(rect, 1);
  Canvas.EndScene;
end;

procedure TLed.setLedON(value: boolean);
begin
  FLedON := value;
  Repaint;
end;

initialization
// FMX.Types.GlobalUseGPUCanvas := true;

 Data.Bind.Components.RegisterObservableMember(TArray<TClass>.Create(TLed), 'LedOn', 'FMX');

finalization
 Data.Bind.Components.UnregisterObservableMember(TArray<TClass>.Create(TLed));

end.
