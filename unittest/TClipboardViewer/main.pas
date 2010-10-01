unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ClipboardViewerForm, StdCtrls;

type
  TForm1 = class(TClipboardViewer)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
Memo1.Clear;
Memo1.PasteFromClipboard;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OnClipboardContentChanged := Button1Click;
  Start;
end;

end.
