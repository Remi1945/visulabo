program test;

uses
  System.StartUpCopy,
  FMX.Forms,
  testmain in 'testmain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
