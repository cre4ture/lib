program ThreadSocketTester;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  TThreadSocketSplitter in '..\TThreadSocketSplitter.pas',
  TCPSocket in '..\TCPSocket.pas',
  StatusThread in '..\StatusThread.pas',
  SplitSocket in '..\SplitSocket.pas',
  MergeSocket in '..\MergeSocket.pas',
  MultiplexList in '..\MultiplexList.pas',
  Unit2 in 'Unit2.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
  Form2.Free;
  Form1.Free;
end.
