unit TestFill;

interface

uses
  System.SysUtils, System.Classes, System.Types, FMX.Types, FMX.Controls, FMX.Objects, FMX.Graphics, System.UITypes,
  System.UIConsts, System.Math.Vectors;

type

  TTestFill = class(TRectangle)
  private
    FFillA: TBrush;
    FFillB: TBrush;

    procedure SetFillA(const Value: TBrush);
    procedure SetFillB(const Value: TBrush);

  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property FillA: TBrush read FFillA write SetFillA;
    property FillB: TBrush read FFillB write SetFillB;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TTestFill]);
end;

procedure TTestFill.Paint;

begin

  Canvas.BeginScene;
  Canvas.FillRect(TRectF.Create(0, 0, Width / 2, Height), 0, 0, AllCorners, 1, FillA);
  Canvas.FillRect(TRectF.Create(Width / 2, 0, Width, Height), 0, 0, AllCorners, 1, FillB);
  Canvas.EndScene;
end;

procedure TTestFill.SetFillA(const Value: TBrush);
begin
  FFillA := Value;
end;

procedure TTestFill.SetFillB(const Value: TBrush);
begin
  FFillB := Value;
end;

constructor TTestFill.Create(AOwner: TComponent);
begin
  inherited;
  FFillA := TBrush.Create(TBrushKind.Solid, $FFE0E0E0);
  FFillB := TBrush.Create(TBrushKind.Solid, $FFE0FFE0);
end;

end.
