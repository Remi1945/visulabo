﻿program apptest;

uses
  System.StartUpCopy,
  FMX.Forms,
  testvisu in 'testvisu.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
