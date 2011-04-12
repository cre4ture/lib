unit notifywindow;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, sliding_window;

type
  TNotify_frm_grp = class;
  Tfrm_notify_class = class of Tfrm_notify;
  Tfrm_notify = class(TSlidingForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  protected
    group_owner: TNotify_frm_grp;
  public
    priority: integer;
    PROCEDURE CreateParams(VAR Params: TCreateParams); OVERRIDE;
    destructor Destroy; override;
    constructor Create(AOwner: TComponent; aGroup: TNotify_frm_grp); reintroduce;
  end;
  TNotify_frm_grp = class
  private
    owner: TComponent;
    window_list: TList;
    procedure Sort;
  protected
    procedure RemoveNotifyWindow(frm: Tfrm_notify);
  public
    procedure refresh_positions;
    constructor Create(aOwner: TComponent);
    destructor Destroy; override;
    function NewNotifyWindow: Tfrm_notify; overload;
    function NewNotifyWindow(aClass: Tfrm_notify_class): Tfrm_notify; overload;
    function Count: integer;
    function window(index: integer): Tfrm_notify;
  end;


function WindowListCompare(Item1, Item2: Pointer): Integer;


implementation

{$R *.dfm}

constructor Tfrm_notify.Create(AOwner: TComponent; aGroup: TNotify_frm_grp);
begin
  group_owner := aGroup;
  inherited Create(AOwner);
end;

PROCEDURE Tfrm_notify.CreateParams(VAR Params: TCreateParams);
begin
  INHERITED;

  begin
    (*Params.ExStyle := Params.ExStyle
      //AND (not WS_EX_APPWINDOW)
      //OR WS_EX_TOOLWINDOW
      ;*)
    Params.WndParent := 0;//Application.Handle;
  end;
end;

function TNotify_frm_grp.Count: integer;
begin
  Result := window_list.Count;
end;

constructor TNotify_frm_grp.Create(aOwner: TComponent);
begin
  inherited Create;
  window_list := TList.Create;
  owner := aOwner;
end;

destructor TNotify_frm_grp.Destroy;
begin
  while window_list.Count > 0 do
    window(0).free;
    
  window_list.free;
  inherited;
end;

function TNotify_frm_grp.NewNotifyWindow: Tfrm_notify;
begin
  Result := NewNotifyWindow(Tfrm_notify);
end;

function TNotify_frm_grp.NewNotifyWindow(aClass: Tfrm_notify_class): Tfrm_notify;
var frm: Tfrm_notify;
begin
  frm := aClass.Create(nil, Self);
  window_list.Add(frm);
  frm.Left := (Screen.Width - frm.Width) div 2;
  frm.Top := (Screen.Height - frm.Height) div 2;
  refresh_positions;
  
  Result := frm;
end;

procedure TNotify_frm_grp.refresh_positions;
var i: integer;
    height: integer;
    frm: Tfrm_notify;
begin
  Sort;

  height := 0;
  for i := window_list.Count-1 downto 0 do
  begin
    frm := Tfrm_notify(window_list.Items[i]);
    height := height + frm.Height+5;

    frm.SlideTo(Screen.Width - frm.Width, Screen.Height - height - 35);
  end;
end;

procedure TNotify_frm_grp.RemoveNotifyWindow(frm: Tfrm_notify);
begin
  window_list.Remove(frm);
  refresh_positions;
end;

procedure TNotify_frm_grp.Sort;
begin
  window_list.Sort(WindowListCompare);
end;

function TNotify_frm_grp.window(index: integer): Tfrm_notify;
begin
  Result := Tfrm_notify(window_list[index]);
end;

destructor Tfrm_notify.Destroy;
begin
  if group_owner <> nil then
    group_owner.RemoveNotifyWindow(self);

  inherited;
end;

procedure Tfrm_notify.FormCreate(Sender: TObject);
begin
  ShowWindow(Handle, SW_HIDE) ;
  SetWindowLong(Handle, GWL_EXSTYLE,
    getWindowLong(Handle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW) ;
  ShowWindow(Handle, SW_SHOW) ;
  DoubleBuffered := True;
end;

function WindowListCompare(Item1, Item2: Pointer): Integer;
begin
  Result := Tfrm_notify(Item1).priority - Tfrm_notify(Item2).priority;
end;

end.
