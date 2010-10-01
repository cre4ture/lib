unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DateUtils, utc_localtz;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    procedure Edit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;



implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Edit1.Text := IntToStr(DateTimeToUnix(LocalDateTimeToUtc(now())));
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  Edit2.Text := DateTimeToStr(UtcToLocalDateTime(UnixToDateTime(StrToInt64(Edit1.Text))));
end;

end.
