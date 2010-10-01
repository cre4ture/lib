unit sim_hid_io;

{
################################################################################

Author: Ulrich Hornung
Date: März 2008

Funktion:

Simulieren von Benutzereingaben über die "Standart"-Delphi Möglichkeiten
-> Steuerung von Metin2 (nur Tastatureingaben) Funktioniert leider nicht so toll
-> Aushilfe: Synergy (siehe synergy_server_sim)

################################################################################
}

interface

uses
  Controls, Types, Windows;

procedure sim_io_MouseMove(x, y: Integer);
procedure sim_io_MouseDown(mousebutton: TMouseButton = mbleft);
procedure sim_io_MouseUp(mousebutton: TMouseButton = mbleft);
procedure sim_io_KeyDown(C: Char);
procedure sim_io_KeyUp(C: Char);
procedure sim_io_TypeString(S: string);
procedure sim_io_MouseClick(p: TPoint; mousebutton: TMouseButton);

implementation

procedure sim_io_MouseMove(x, y: Integer);
begin
  Mouse.CursorPos := Point((x),(y));
end;

procedure sim_io_MouseDown(mousebutton: TMouseButton = mbleft);
begin
  case mousebutton of
    mbLeft:
      mouse_event(MOUSEEVENTF_LEFTDOWN,0,0,0,0);
    mbRight:
      mouse_event(MOUSEEVENTF_RIGHTDOWN,0,0,0,0);
    mbMiddle:
      mouse_event(MOUSEEVENTF_MIDDLEDOWN,0,0,0,0);
  end;
end;

procedure sim_io_MouseUp(mousebutton: TMouseButton = mbleft);
begin
  case mousebutton of
    mbLeft:
      mouse_event(MOUSEEVENTF_LEFTUP,0,0,0,0);
    mbRight:
      mouse_event(MOUSEEVENTF_RIGHTUP,0,0,0,0);
    mbMiddle:
      mouse_event(MOUSEEVENTF_MIDDLEUP,0,0,0,0);
  end;
end;

procedure sim_io_KeyDown(C: Char);
begin
  keybd_event(Byte(C),0,0,0);
end;

procedure sim_io_KeyUp(C: Char);
begin
  keybd_event(Byte(C),0,KEYEVENTF_KEYUP,0);
end;

procedure sim_io_TypeString(S: string);
var i: integer;
    c: char;
    w: word;
begin
  for i := 1 to length(s) do
  begin
    w := Word(VkKeyScan(s[i]));
    c := char(w);
    if (w and $0100 > 0) then
      sim_io_KeyDown(char(VK_SHIFT));   //SHIFT
    if (w and $0200 > 0) then
      sim_io_KeyDown(char(VK_CONTROL)); //CTRL
    if (w and $0400 > 0) then
      sim_io_KeyDown(char(VK_MENU)); //ALT

    sim_io_KeyDown(c);
    sim_io_KeyUp(c);

    if (w and $0100 > 0) then
      sim_io_KeyUp(char(VK_SHIFT));
    if (w and $0200 > 0) then
      sim_io_KeyUp(char(VK_CONTROL));
    if (w and $0400 > 0) then
      sim_io_KeyUp(char(VK_MENU));
  end;
end;

procedure sim_io_MouseClick(p: Tpoint; mousebutton: TMouseButton);
begin
  sim_io_MouseMove(p.X, p.Y);
  sim_io_MouseDown(mousebutton);
  sim_io_MouseUp(mousebutton);
end;

end.
