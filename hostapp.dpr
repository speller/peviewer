program hostapp;

{$IFDEF WIN64}
  {$LIBSUFFIX '_64'}
{$ENDIF}

uses
  Vcl.Forms,
  HostAppFormUnit in 'forms\HostAppFormUnit.pas' {HostAppForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(THostAppForm, HostAppForm);
  Application.Run;
end.
