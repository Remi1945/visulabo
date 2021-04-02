unit Selecteur;

interface

uses
  System.SysUtils, Couleurs, System.Classes, System.Types, System.UIConsts, FMX.Types, FMX.Controls, FMX.Objects,
  FMX.Graphics;

type
  TGenre = (Circulaire, Horizontal, Vertical);

  TSelecteur = class(TRectangle)
  private
    FGenre: TGenre;
    FItems: TStrings;
    procedure SetItems(const Value: TStrings);
  protected
    { Déclarations protégées }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property Genre: TGenre read FGenre write FGenre;
    property Items: TStrings read FItems write SetItems;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TSelecteur]);
end;

constructor TSelecteur.Create(AOwner: TComponent);
begin
  inherited;
  FGenre := Horizontal;
  FItems:= TStrings.Create;
end;

procedure TSelecteur.Paint;
var
  rect: TRectF;
begin
  Canvas.BeginScene;
  if FGenre = Horizontal then
  begin
    Canvas.DrawLine(TPointF.Create(0, Height / 2), TPointF.Create(Width, Height / 2), 1);
  end;
  Canvas.EndScene;
end;

procedure TSelecteur.SetItems(const Value: TStrings);
begin
 Items.Assign(Value);
end;
end.
