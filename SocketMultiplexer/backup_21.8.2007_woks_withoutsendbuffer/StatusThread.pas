unit StatusThread;

interface

uses
  Classes, SysUtils, Messages, Controls, Windows{$ifdef Lazarus}, LMessages, WSControls, InterfaceBase, LCLType{$endif};

type
  TNotifyDataFunction = function(Sender: TObject; Data: pointer): Integer of object;
  TExceptionEvent = procedure(Sender: TThread; E: Exception; msg: String) of object;
  TThreadStatusEvent = procedure(Sender: TThread; ABezeichner, AStatus: String) of object;
  TSThread = class(TThread)
  protected
    FStatus: String;
    FOnThreadStatus: TThreadStatusEvent;
    Sync_NotifyEvent: TNotifyEvent;
    Sync_NotifyEvent_Object: TObject;
    Sync_NotifyDFunk: TNotifyDataFunction;
    Sync_NotifyDFunk_Result: Integer;
    Sync_NotifyDFunk_data: pointer;
    procedure SetStatus(S: String);
    procedure SetOnThreadStatus(Event: TThreadStatusEvent);
    procedure DoSync_NotifyEvent;
    procedure DoSync_NotifyDFunk;
  public
    OnException: TExceptionEvent;
    OnOnThreadStatusChange: TNotifyEvent;
    Name: String;
    property OnThreadStatus: TThreadStatusEvent read FOnThreadStatus write SetOnThreadStatus;
    property Status: String read FStatus write SetStatus;
    destructor Destroy; override;
    constructor Create(CreateSuspended: Boolean);
    {$ifndef Lazarus}
    procedure SynchronizeNotifyEvent(Proc: TNotifyEvent; Sender: TObject);
    function SynchroniseNotifyDataFunction(Proc: TNotifyDataFunction;
      Sender: TObject; Data: pointer): Integer;
    {$endif}
    procedure Execute; override;
  end;
  TToMainThreadOnSyncData = procedure(Sender: TObject; Index: Byte;
    Data: Pointer; Size: Integer) of object;
  TToMainThread = class(TComponent)
  private
    FHandle: THandle;
    FOnSyncData: TToMainThreadOnSyncData;
    {$ifdef Lazarus}
    procedure WndProc(var Msg: TLMessage);
    {$else}
    procedure WndProc(var Msg: TMessage);
    {$endif}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SyncData(const Index: Byte; Data: pointer; Size: Integer);
  published
    property OnSyncData: TToMainThreadOnSyncData read FOnSyncData
      write FOnSyncData;
  end;

var
  ThreadStatusActive: Boolean;

procedure Register;

threadvar
  gThreadID: Cardinal;

implementation

procedure Register;
begin
  RegisterComponents('nice things', [TToMainThread]);
end;

procedure TSThread.DoSync_NotifyEvent;
begin
  Sync_NotifyEvent(Sync_NotifyEvent_Object);
end;

procedure TSThread.DoSync_NotifyDFunk;
begin
  Sync_NotifyDFunk_Result := Sync_NotifyDFunk(Sync_NotifyEvent_Object,
                                              Sync_NotifyDFunk_data);
end;

{$ifndef Lazarus}
procedure TSThread.SynchronizeNotifyEvent(Proc: TNotifyEvent; Sender: TObject);
begin
  Sync_NotifyEvent := Proc;
  Sync_NotifyEvent_Object := Sender;
  Synchronize(DoSync_NotifyEvent);
end;

function TSThread.SynchroniseNotifyDataFunction(Proc: TNotifyDataFunction;
  Sender: TObject; Data: pointer): Integer;
begin
  Sync_NotifyEvent_Object := Sender;
  Sync_NotifyDFunk := Proc;
  Sync_NotifyDFunk_data := Data;
  Synchronize(DoSync_NotifyDFunk);
  Result := Sync_NotifyDFunk_Result;
end;
{$endif}

constructor TSThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  Name := ClassName;
end;

destructor TSThread.Destroy;
begin
  inherited Destroy;
end;

procedure TSThread.SetOnThreadStatus(Event: TThreadStatusEvent);
begin
  FOnThreadStatus := Event;
  if Assigned(OnOnThreadStatusChange) then
    OnOnThreadStatusChange(self);
  
  Status := 'SetOnThreadStatus';
end;

procedure TSThread.SetStatus(S: String);
begin
  FStatus := S;
  OutputDebugString(PChar('"RunThreadID: ' + IntToStr(GetCurrentThreadId) +
    ' ThreadObj(ID): ' + Name + '(' + IntToStr(ThreadID) +
    ') notify: ' + FStatus + '"'));

  if ThreadStatusActive and Assigned(OnThreadStatus) then
    OnThreadStatus(Self,Name,FStatus);
end;

procedure TSThread.Execute;
begin
  gThreadID := ThreadID;
  {$ifndef Lazarus}
  Inherited;
  {$endif}
end;

constructor TToMainThread.Create(AOwner: TComponent);
var cp: TCreateParams;
begin
  inherited;
  {$ifdef Lazarus}
  FHandle := TWSWinControl(WidgetSet).CreateHandle(Self, cp);
  {$else}
  FHandle := AllocateHWnd(WndProc);
  {$endif}
end;

destructor TToMainThread.Destroy;
begin
  {$ifdef Lazarus}
  TWSWinControl(WidgetSet).DestroyHandle(Self);
  {$else}
  DeallocateHWnd(FHandle);
  {$endif}
  inherited;
end;

{$ifdef Lazarus}
procedure TToMainThread.WndProc(var Msg: TLMessage);
{$else}
procedure TToMainThread.WndProc(var Msg: TMessage);
{$endif}
begin
  if (Msg.Msg >= WM_APP) and
     (Msg.Msg <= WM_APP + high(byte)) then
  begin
    if Assigned(FOnSyncData) then
      FOnSyncData(Self,Msg.Msg - WM_APP,pointer(Msg.WParam),
        Msg.LParam);
  end;
end;

procedure TToMainThread.SyncData(const Index: Byte; Data: pointer;
  Size: Integer);
begin
  {$ifdef Lazarus}
  WidgetSet.PostMessage(FHandle,WM_APP + Index,Integer(Data),Size);
  {$else}
  PostMessage(FHandle,WM_APP + Index,Integer(Data),Size);
  {$endif}
end;

end.
