program test;

uses
  Forms,
  main in 'main.pas' {Form1},
  ClipboardViewerForm in '..\ClipboardViewerForm.pas',
  clipbrdFunctions in '..\clipbrdFunctions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
