unit EditDG;

interface

uses
  System.SysUtils, System.Classes, System.Types, FMX.Types, FMX.Controls,
  FMX.Objects, FMX.Edit;

type
  TSensEcr = (ecrGaucheDroite, ecrDroiteGauche);

  TEditDG = class(TEdit)
  private
    FsensECR: TSensEcr;

  protected
    { Déclarations protégées }
  public
    constructor Create(AOwner: TComponent); override;

  published
    property SensEcriture: TSensEcr read FsensECR write FsensECR;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisuLabo', [TEditDG]);
end;

{ TEditDG }

constructor TEditDG.Create(AOwner: TComponent);
begin
  inherited;
  FsensECR := ecrDroiteGauche;

end;

end.
