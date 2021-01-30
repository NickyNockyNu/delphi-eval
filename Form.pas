unit Form;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Rtti,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,

  EvalLib;

type
  TEvalForm = class(TForm)
    ExpressionLabel: TLabel;
    ExpressionEdit: TComboBox;
    Label2: TLabel;
    OutputMemo: TMemo;
    EvalButton: TButton;
    ClearButton: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure EvalButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
  private
    Evaluator:    TEvaluator;
    Variables:    TVariableStore;
    ClassFactory: TClassFactory;
  public
    { Public declarations }
  end;

var
  EvalForm: TEvalForm;

implementation

{$R *.dfm}

procedure TEvalForm.FormCreate(Sender: TObject);
begin
  Evaluator := TEvaluator.Create;

  Variables := TVariableStore.Create;
  Variables.AutoDeclare := True;
  Variables.Declare('Window', TObject(Self));
  Variables.Declare('Globals', TObject(Variables));
  Variables.Declare('True',  True);
  Variables.Declare('False', False);
  Evaluator.Push(Variables);

  ClassFactory := TClassFactory.Create;
  Evaluator.Declare('ClassFactory', ClassFactory);
end;

procedure TEvalForm.FormDestroy(Sender: TObject);
begin
  {}
end;

procedure TEvalForm.FormActivate(Sender: TObject);
begin
  ExpressionEdit.SetFocus;
end;

procedure TEvalForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if not ExpressionEdit.Focused then
  begin
    ExpressionEdit.SetFocus;
    ExpressionEdit.SelText := Key;
  end;
end;

procedure TEvalForm.EvalButtonClick(Sender: TObject);
var
  ResultValue:  TValue;
  ResultString: String;
begin
  ExpressionEdit.SelectAll;

  try
    ResultValue  := Evaluator.Eval(ExpressionEdit.Text);
    ResultString := ExpressionEdit.Text + ': ' + ResultValue.HumanReadable;

    if Length(ResultString) > 0 then
      Variables.Declare('Result', ResultValue);
  except on E:Exception do
    ResultString := E.ToString;
  end;

  if Length(ResultString) > 0 then
    OutputMemo.Lines.Add(ResultString);

  OutputMemo.SelStart := Length(OutputMemo.Lines.Text);
end;

procedure TEvalForm.ClearButtonClick(Sender: TObject);
begin
  OutputMemo.Lines.Clear;
end;

end.

