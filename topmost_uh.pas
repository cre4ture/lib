unit topmost_uh;

interface

uses
  Forms;

procedure SetTopMost(activate: boolean; form: TForm);

implementation

uses Windows;

procedure SetTopMost(activate: boolean; form: TForm);
begin
  if activate
      then SetWindowPos(form.Handle,HWND_TOPMOST,0,0,0,0,SWP_NOMOVE OR SWP_NOSIZE)
      else SetWindowPos(form.Handle,HWND_NOTOPMOST,0,0,0,0,SWP_NOMOVE OR SWP_NOSIZE);
end;

end.
