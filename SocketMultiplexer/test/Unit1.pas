unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TThreadSocketSplitter, TCPSocket, XPMan, SplitSocket,
  ExtCtrls, MergeSocket, IdBaseComponent, IdThreadComponent, MultiplexList,
  Spin, Buttons, StatusThread, shellapi;

type
  TRandText = packed record
    c: char;
    pos: cardinal;
  end;
  TLine = packed record
    x1,y1,x2,y2: Integer;
    color: Tcolor;
    width: Integer;
  end;
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    CheckBox1: TCheckBox;
    XPManifest1: TXPManifest;
    SimpleServerComponent1: TSimpleServerComponent;
    Image1: TImage;
    LineMerge: TMergeSocketComponent;
    Memo1: TMemo;
    TextMerge: TMergeSocketComponent;
    ColorBox1: TColorBox;
    SpinEdit1: TSpinEdit;
    Button2: TButton;
    Label1: TLabel;
    Panel1: TPanel;
    SB_Linie: TSpeedButton;
    SB_Kreis: TSpeedButton;
    SB_Rechteck: TSpeedButton;
    SB_Text: TSpeedButton;
    PaintBox1: TPaintBox;
    EllipseMerge: TMergeSocketComponent;
    RechteckMerge: TMergeSocketComponent;
    Timer1: TTimer;
    Button3: TButton;
    SocketMultiplexList1: TSocketMultiplexListComponent;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SimpleServerComponent1ClientConnect(Sender: TObject;
      newSocket: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure LineMergeNewPacket_ReadThread(Sender: TObject;
      Socket: TSplitSocket);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TextMergeNewPacket_ReadThread(Sender: TObject;
      Socket: TSplitSocket);
    procedure Memo1KeyPress(Sender: TObject; var Key: Char);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Memo1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Button2Click(Sender: TObject);
    procedure EllipseMergeNewPacket_ReadThread(Sender: TObject;
      Socket: TSplitSocket);
    procedure RechteckMergeNewPacket_ReadThread(Sender: TObject;
      Socket: TSplitSocket);
    procedure Timer1Timer(Sender: TObject);
    procedure SocketMultiplexList1SocketOpenedTunnel(
      Sender: TObject; SMulti: TSocketMultiplex; Tunnel: TSplitSocket);
    procedure Button3Click(Sender: TObject);
  private
    last: TPoint;
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  CheckBox1Click(self);
  Image1.Picture.Bitmap.Width := Image1.Width;
  Image1.Picture.Bitmap.Height := Image1.Height;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    SimpleServerComponent1.SimpleServer.Start(StrToInt(Edit2.text))
  else SimpleServerComponent1.SimpleServer.Stop;
end;

procedure TForm1.Button1Click(Sender: TObject);
var TCPSocket: TTCPSocket;
    multi: TSocketMultiplex;
begin
  TCPSocket := TTCPSocket.Create;
  TCPSocket.Connect(Edit1.Text,StrToInt(Edit2.Text));
  multi := TSocketMultiplex.Create(TCPSocket,False);
  SocketMultiplexList1.SocketList.AddSocketMultiplex(multi);
end;

procedure TForm1.SimpleServerComponent1ClientConnect(Sender: TObject;
  newSocket: Integer);
var multi: TSocketMultiplex;
    tunnel: TSplitSocket;
begin
  multi := SocketMultiplexList1.SocketList.AddSocketMultiplex(newSocket, True);
  multi.OpenTunnel(55,tunnel);
  multi.OpenTunnel(56,tunnel);
  multi.OpenTunnel(57,tunnel);
  multi.OpenTunnel(40,tunnel);

  {sleep(500);
  for i := length(Memo1.Lines.Text) downto 1 do
  begin
    rt.c := Memo1.Lines.Text[i];
    rt.pos := 0;
    if rt.c <> #10 then
      multi.SendBuf(rt,sizeof(rt),40);
  end;  }
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var line: TLine;
begin
  if TComponent(Sender).Tag = 1 then
  begin
    Image1.Canvas.Pen.Color := ColorBox1.Selected;
    Image1.Canvas.Pen.Width := SpinEdit1.Value;
    Image1.Canvas.Brush.Color := ColorBox1.Selected;
    PaintBox1.Canvas.Pen.Color := ColorBox1.Selected;
    PaintBox1.Canvas.Pen.Width := SpinEdit1.Value;
    PaintBox1.Canvas.Brush.Color := ColorBox1.Selected;
    PaintBox1.Color := ColorBox1.Selected;
    line.color := ColorBox1.Selected;
    line.width := SpinEdit1.Value;
    line.x1 := last.X;
    line.y1 := last.y;
    line.x2 := x;
    line.y2 := y;
    if SB_Linie.Down then
    begin
      Image1.Canvas.MoveTo(last.x,last.y);
      Image1.Canvas.LineTo(x,y);
      LineMerge.MergeSocket.SendPacket(Line,sizeof(Line));
      last.X := x;
      last.y := y;
    end;
    if SB_Kreis.Down then
    begin
      Image1.Refresh;
      PaintBox1.Canvas.Ellipse(last.X,last.y,x,y);
    end;
    if SB_Rechteck.Down then
    begin
      Image1.Refresh;
      PaintBox1.Canvas.Rectangle(last.X,last.y,x,y);
    end;
  end;
end;

procedure TForm1.LineMergeNewPacket_ReadThread(Sender: TObject;
  Socket: TSplitSocket);
var p: pointer;
    s: word;
    sock: TSplitSocket;
begin
  while LineMerge.MergeSocket.GetPacket(p,s,sock) do
  begin
    LineMerge.MergeSocket.SendPacket(p^,s,sock);
    with TLine(p^) do
    begin
      Image1.Canvas.Lock;
      Image1.Canvas.Pen.Color := Color;
      Image1.Canvas.Pen.Width := width;
      Image1.Canvas.MoveTo(x1,y1);
      Image1.Canvas.LineTo(x2,y2);
      Image1.Canvas.Unlock;
    end;

    FreeMem(p);
  end;
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TComponent(Sender).Tag := 1;
  last.X := x;
  last.y := y;
end;

procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var line: TLine;
begin
  Image1.Canvas.Pen.Color := ColorBox1.Selected;
  Image1.Canvas.Pen.Width := SpinEdit1.Value;
  Image1.Canvas.Brush.Color := ColorBox1.Selected;
  line.color := ColorBox1.Selected;
  line.width := SpinEdit1.Value;
  line.x1 := last.X;
  line.y1 := last.y;
  line.x2 := x;
  line.y2 := y;

  if SB_Linie.Down then Image1MouseMove(Sender,Shift,x,y);
  if SB_Kreis.Down then
  begin
    Image1.Canvas.Ellipse(line.x1,line.y1,line.x2,line.y2);
    EllipseMerge.MergeSocket.SendPacket(line,sizeof(line));
  end;
  if SB_Rechteck.Down then
  begin
    Image1.Canvas.Rectangle(line.x1,line.y1,line.x2,line.y2);
    RechteckMerge.MergeSocket.SendPacket(line,sizeof(line));
  end;
  TComponent(Sender).Tag := 0;
end;

procedure TForm1.TextMergeNewPacket_ReadThread(Sender: TObject;
  Socket: TSplitSocket);
var p: pointer;
    s: word;
    text, buf: string;
    sock: TSplitSocket;
    op: cardinal;
begin
  while TextMerge.MergeSocket.GetPacket(p,s,sock) do
  begin
    TextMerge.MergeSocket.SendPacket(p^,s,sock);
    op := Memo1.SelStart;
    with TRandText(p^) do
    begin
      case c of
      #8:
        begin
          text := Memo1.Lines.Text;
          if text[pos] = #13 then
          buf := copy(text,pos+2,high(integer)) else buf := copy(text,pos+1,high(integer));
          if text[pos] = #10 then
          SetLength(text,pos-2) else SetLength(text,pos-1);
          text := text + buf;
          Memo1.Lines.Text := text;

          if op >= pos then dec(op);
        end;
      else
        text := Memo1.Lines.Text;
        buf := copy(text,pos+1,high(integer));
        SetLength(text,pos);
        text := text +c+ buf;
        Memo1.Lines.Text := text;

        if op >= pos then inc(op);
      end;
    end;
    Memo1.SelStart := op;

    FreeMem(p);
  end;
end;

procedure TForm1.Memo1KeyPress(Sender: TObject; var Key: Char);
var rt: TRandText;
begin
  rt.c := Key;
  rt.pos := Memo1.SelStart;
  TextMerge.MergeSocket.SendPacket(rt,sizeof(rt));
end;

procedure TForm1.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var rt: TRandText;
begin
  Memo1.SelLength := 0;
  case Key of
  46: begin  //Entf
        rt.c := #8;
        rt.pos := Memo1.SelStart+1;
        TextMerge.MergeSocket.SendPacket(rt,sizeof(rt));
      end;
  end;
end;

procedure TForm1.Memo1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Memo1.SelLength := 0;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  form2.Show;
end;

procedure TForm1.EllipseMergeNewPacket_ReadThread(Sender: TObject;
  Socket: TSplitSocket);
var p: pointer;
    s: word;
    sock: TSplitSocket;
begin
  while EllipseMerge.MergeSocket.GetPacket(p,s,sock) do
  begin
    EllipseMerge.MergeSocket.SendPacket(p^,s,sock);
    with TLine(p^) do
    begin
      Image1.Canvas.Lock;
      Image1.Canvas.Pen.Color := Color;
      Image1.Canvas.Pen.Width := width;
      Image1.Canvas.Brush.Color := Color;
      Image1.Canvas.Ellipse(x1,y1,x2,y2);
      Image1.Canvas.Unlock;
    end;

    FreeMem(p);
  end;
end;

procedure TForm1.RechteckMergeNewPacket_ReadThread(Sender: TObject;
  Socket: TSplitSocket);
var p: pointer;
    s: word;
    sock: TSplitSocket;
begin
  while RechteckMerge.MergeSocket.GetPacket(p,s,sock) do
  begin
    RechteckMerge.MergeSocket.SendPacket(p^,s,sock);
    with TLine(p^) do
    begin
      Image1.Canvas.Lock;
      Image1.Canvas.Pen.Color := Color;
      Image1.Canvas.Pen.Width := width;
      Image1.Canvas.Brush.Color := Color;
      Image1.Canvas.Rectangle(x1,y1,x2,y2);
      Image1.Canvas.Unlock;
    end;

    FreeMem(p);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  with Form2 do
  begin
    AddObjectToWatch(Form1.SocketMultiplexList1.SocketList,'SocketMultiplexList1');
    AddObjectToWatch(Form1.LineMerge.MergeSocket,'LineMerge');
    AddObjectToWatch(Form1.EllipseMerge.MergeSocket,'EllipseMerge');
    AddObjectToWatch(Form1.RechteckMerge.MergeSocket,'RechteckMerge');
    AddObjectToWatch(Form1.TextMerge.MergeSocket,'TextMerge');
  end;
end;

procedure TForm1.SocketMultiplexList1SocketOpenedTunnel(
  Sender: TObject; SMulti: TSocketMultiplex; Tunnel: TSplitSocket);
begin
  OutputDebugString(PChar('ThreadID: ' + IntToStr(gThreadID) + ' OpenedTunnel_start: '+IntToStr(Tunnel.WorkProcessIndex)));
  case Tunnel.WorkProcessIndex of
    55: LineMerge.MergeSocket.AddSocket(Tunnel);
    56: EllipseMerge.MergeSocket.AddSocket(Tunnel);
    57: RechteckMerge.MergeSocket.AddSocket(Tunnel);
    40: TextMerge.MergeSocket.AddSocket(Tunnel);
  else

  end;
  OutputDebugString(PChar('ThreadID: ' + IntToStr(gThreadID) + ' OpenedTunnel_ende: '+IntToStr(Tunnel.WorkProcessIndex)));
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ShellExecute(Handle,'Open',PChar(ExtractFileName(Application.ExeName))
    ,'',PChar(ExtractFilePath(Application.ExeName)),0);
end;

end.
