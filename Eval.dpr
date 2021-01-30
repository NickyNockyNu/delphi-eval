program Eval;

uses
  Vcl.Forms,
  Form in 'Form.pas' {EvalForm},
  Vcl.Themes,
  Vcl.Styles,
  EvalLib in 'EvalLib.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Win10IDE_Dark');
  Application.CreateForm(TEvalForm, EvalForm);
  Application.Run;
end.

