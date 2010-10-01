unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, notifywindow;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  sleep(3000);
  ShowNotifyWindow('test_Titel', 'test_Nachricht', Button2Click);
  sleep(1000);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShowMessage('test_callback');
end;

end.
