program TestApp;

uses
  Vcl.Forms,
  MyListViewUnit in 'MyListViewUnit.pas',
  TestUnit in 'TestUnit.pas' {Form1},
  MyToolbarUnit in 'MyToolbarUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
