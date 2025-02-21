﻿unit testvisu;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, HorizonArtificiel;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    HorizonArtificiel1: THorizonArtificiel;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
    sensT, sensR: integer;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  sensT := 1;
  sensR := -1;
end;

procedure TForm1.Timer1Timer(Sender: TObject);

begin
  if HorizonArtificiel1.Tangage > 12 then
    sensT := -1;
  if HorizonArtificiel1.Tangage < -14 then
    sensT := 1;

  if HorizonArtificiel1.Roulis > 24 then
    sensR := -1;
  if HorizonArtificiel1.Roulis < -35 then
    sensR := 1;
  HorizonArtificiel1.setRoulisTangage(HorizonArtificiel1.Roulis + sensR, HorizonArtificiel1.Tangage + sensT);
end;

end.
