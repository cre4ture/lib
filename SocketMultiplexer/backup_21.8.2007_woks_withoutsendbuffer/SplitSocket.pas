unit SplitSocket;

interface

uses
  {$ifdef Lazarus}TCPSocket_Lazarus{$else}TCPSocket{$endif}, StatusThread, SysUtils, Classes,
    Windows, Contnrs, SyncObjs;

type
  ETimeOut = class(Exception);
  TProcessIndex = Word;
  TSocketMultiplexHeader = packed record
    ProcessIndex: TProcessIndex;
    Size: Word;
  end;
  PSplitSocketPacket = ^TSplitSocketPacket;
  TSplitSocketPacket = record
    Header: TSocketMultiplexHeader;
    Data: pointer;
  end;
  TThreadDataSocket = class
  private
    FData: pointer;
    FDataCriticalSection: TCriticalSection;
  public
    function LockData: pointer;
    procedure UnlockData;
    procedure SetData_locked_(const Data: pointer);
    constructor Create;
    destructor Destroy; override;
  end;
  TSplitSocket = class(TThreadDataSocket)
  private
    FData: pointer;
    PacketQueue: TQueue;
    LockPacketQueue: TEvent;
    BufferedPackets: TEvent;
    FHostSocket: TObject;
    FMaster: Boolean;
    procedure NewPacket(Sender: TObject; Header: TSocketMultiplexHeader;
      Data: pointer; WorkAt: Integer);
  public
    WorkProcessIndex: TProcessIndex;
    OnNewPacket_ReadThread: TNotifyEvent;
    OnDestroy: TNotifyEvent;
    property Master: Boolean read FMaster;
    property HostSocket: TObject read FHostSocket;
    constructor Create(AHostSocket: TObject; AMaster: Boolean);
    destructor Destroy; override;
    procedure SendPacket(var aData; aSize: Word);
    procedure RecvPacket(var Data: pointer; var Size: Word; const TimeOut: Cardinal = high(cardinal));
    function GetPacket(var Data: pointer; var Size: Word): Boolean;
  end;
  TSocketMultiplexOpeningTunnel = procedure(Sender: TObject; Tunnel: TSplitSocket; var Accept: Boolean) of Object;
  TSocketMultiplexOpenedTunnel = procedure(Sender: TObject; Tunnel: TSplitSocket) of object;
  TSocketMultiplex = class(TSThread)
  private
    ThreadStopp: TEvent;
    CanWrite: TEvent;
    FTunnel: TThreadList;
    Socket: TTCPSocket;
    FMaster: Boolean;
    FStopped: Boolean;
    FData: TThreadDataSocket;
    procedure SendPacketAtSplitSocket(Header: TSocketMultiplexHeader;
      Data: pointer; WorkAt: Integer; ASocket: TSplitSocket);
    procedure AddTunnel(tunnel: TSplitSocket);
    procedure DoUnusedPacket(Header: TSocketMultiplexHeader; Data: pointer);
    procedure ProcessPacket;
    procedure Sync_OnOpenedTunnel_Event(Sender: TObject);
  public
    OnThreadExit: TNotifyEvent;
    OnOpeningTunnel_ReadThread: TSocketMultiplexOpeningTunnel;
    OnOpenedTunnel: TSocketMultiplexOpenedTunnel;
    constructor Create(ASocket: TTCPSocket; AMaster: Boolean);
    procedure Execute; override;
    destructor Destroy; override;
    procedure SendBuf(var Buf; Size, ProcessIndex: Word);
    function OpenTunnel(PI: Integer; var Tunnel: TSplitSocket): Boolean;
    function AddTunnelEnd(PI: TProcessIndex): TSplitSocket;
    procedure Close;
    function ____SplitSocketList: TThreadList;
    property Master: Boolean read FMaster;
    function RemoteHost: TTCPHost;
    function ____FindTunnel(PI: Word): TSplitSocket;
    procedure StartWorking;
    function LockData: pointer;
    procedure SetData_locked_(const Data: pointer);
    procedure UnlockData;
  end;

const TimeOut = 3000;

implementation

uses DateUtils, ComObj, Math;

procedure TSocketMultiplex.SendBuf(var Buf; Size, ProcessIndex: Word);
var Header: TSocketMultiplexHeader;
begin
  Header.Size := Size;
  Header.ProcessIndex := ProcessIndex;

  if (CanWrite.WaitFor(TimeOut) = wrSignaled) then
    try
      while not Socket.SelectWrite(1000) do
        sleep(100);

      //if Terminated then raise Exception.Create('API: SendBuf, ReadThread wurde terminiert!');

      Socket.SendBuf(Header,sizeof(Header));
      if Size > 0 then Socket.SendBuf(Buf,Size);

    finally
      CanWrite.SetEvent;
    end else raise Exception.Create('API: SendBuf, Timeout beim Warten auf Schreiberlaubnis');
end;

constructor TSocketMultiplex.Create(ASocket: TTCPSocket; AMaster: Boolean);
begin
  FData := TThreadDataSocket.Create;
  inherited Create(True);
  FMaster := AMaster;
  Socket := ASocket;
  FTunnel := TThreadList.Create;
  CanWrite := TEvent.Create(nil,False,True,'');
  ThreadStopp := TEvent.Create(nil,True,False,'');

  FreeOnTerminate := True;
end;

destructor TSocketMultiplex.Destroy;
var i: integer;
begin
  FreeOnTerminate := False;
  Terminate;
  Socket.Close;
  Resume;
  if ReturnValue = STILL_ACTIVE then
    WaitFor;

  with FTunnel.LockList do
  try
    for i := 0 to Count-1 do
    begin
      TSplitSocket(Items[i]).Free;
    end;
  finally
    FTunnel.UnlockList;
  end;

  ThreadStopp.Free;
  CanWrite.Free;
  Socket.Free;
  FTunnel.Free;
  inherited;
  FData.Free;
end;

procedure TSocketMultiplex.Execute;
begin
  inherited;

  ReturnValue := STILL_ACTIVE;
  try
    while (not Terminated)and(Socket.Connected) do
    begin
      ProcessPacket;
    end;
  except
    on E: Exception do
    begin
      Status := 'Exception in ' + Name + ':' + E.Message + ' (' + E.ClassName + ')';
      If Assigned(OnException) then OnException(Self,E,Status);
    end;
  end;
  
  if Assigned(OnThreadExit) then
    SynchronizeNotifyEvent(OnThreadExit,Self);

  ReturnValue := 0;
end;

constructor TSplitSocket.Create(AHostSocket: TObject; AMaster: Boolean);
begin
  inherited Create;
  FMaster := AMaster;
  FHostSocket := AHostSocket;
  PacketQueue := TQueue.Create;
  LockPacketQueue := TEvent.Create(nil,False,True,'');
  BufferedPackets := TEvent.Create(nil,True,False,'');
end;

destructor TSplitSocket.Destroy;
var p: pointer;
    s: word;
begin
  if Assigned(OnDestroy) then
    OnDestroy(Self);

  while GetPacket(p,s) do //übrige Packete Freigeben!!
    FreeMem(p);

  LockPacketQueue.Free;
  PacketQueue.Free;
  inherited;
end;

{###############################################################################
 TSplitSocket.GetPacket

 Liest, falls vorhanden, das nächste Packet. Wenn kein Packet mehr im "Buffer"
 ist, gibt die Funktion False zurück!

 Diese Funktion Blockiert nicht!

 Verwende RecvPacket für blockierenden Aufruf!
###############################################################################}
function TSplitSocket.GetPacket(var Data: pointer; var Size: Word): Boolean;
var Packet: PSplitSocketPacket;
begin
  OutputDebugString(PChar('tid' + inttostr(GetCurrentThreadID) + ': TSplitSocket.GetPacket'));
  if LockPacketQueue.WaitFor(high(cardinal)) = wrSignaled then       //warten bis queue freigegeben, sollte normal nicht lange dauern! (timeout zwecklos!) evtl. critical section!?
  begin
    try
      Result := PacketQueue.Count > 0;     //checken ob Packet vohanden!
      if Result then
      begin
        Packet := PacketQueue.Pop;
        Size := Packet^.Header.Size;
        Data := Packet^.Data;
        FreeMem(Packet);
      end;
    finally
      LockPacketQueue.SetEvent;
    end;
  end
  else raise Exception.Create('TSplitSocket.GetPacket: ERROR while waiting for LockPacketQueue');
end;

procedure TSplitSocket.NewPacket(Sender: TObject;
  Header: TSocketMultiplexHeader; Data: pointer; WorkAt: Integer);
var Packet: PSplitSocketPacket;
begin
  GetMem(Packet,Sizeof(Packet));
  Packet^.Header := Header;
  Packet^.Data := Data;

  OutputDebugString('TSplitSocket.NewPacket LockPacketQueue.WaitFor');
  if LockPacketQueue.WaitFor(TimeOut) = wrSignaled then
  begin
    try
      PacketQueue.Push(Packet);
      BufferedPackets.SetEvent;                 //Melden, das wieder Packete gibt!
    finally
      LockPacketQueue.SetEvent;
      OutputDebugString('TSplitSocket.NewPacket LockPacketQueue.SetEvent');
    end;

    if Assigned(OnNewPacket_ReadThread) then
      OnNewPacket_ReadThread(Self);
      
  end
  else raise Exception.Create('TSplitSocket.NewPacket: Timeout waiting for LockPacketQueue');
end;

procedure TSocketMultiplex.SendPacketAtSplitSocket(Header: TSocketMultiplexHeader;
  Data: pointer; WorkAt: Integer; ASocket: TSplitSocket);
begin
  try
    ASocket.NewPacket(Self,Header,Data,WorkAt);
  except
    on E: Exception do
    begin
      Status := 'Exception in ' + Name + ':' + E.Message + ' (' + E.ClassName + ')';
      If Assigned(OnException) then OnException(Self,E,Status);
    end;
  end;
end;

{###############################################################################
 TSplitSocket.RecvPacket

 Liest das nächste Packet. Wenn nötig wird solange blockiert bis ein Packet
 vorhanden ist!
 Is quasi der Blockierende Socketaufruf.

 Verwende GetPacket für nicht blockierenden aufruf!
###############################################################################}
procedure TSplitSocket.RecvPacket(var Data: pointer; var Size: Word; const TimeOut: Cardinal = high(cardinal));
var ready: Boolean;
begin
  ready := GetPacket(Data,Size);
  while (not ready) do
  begin
    //Wenn dies der Readthread vom HostSocket ist, dann muss ProcessPacket
    //hier extra aufgerufen werden! (Der Timeout klappt dann aber nicht!)
    if (GetCurrentThreadId = TSocketMultiplex(HostSocket).Handle) then
    begin
      TSocketMultiplex(HostSocket).ProcessPacket;
      ready := GetPacket(Data,Size);
    end
    else
      case BufferedPackets.WaitFor(TimeOut) of
      wrSignaled:
        begin
          ready := GetPacket(Data,Size);
          if not ready then                                         //wenn keine gelesen wurden, gibts auch keine mehr! -> warten
            BufferedPackets.ResetEvent;
        end;
      wrTimeout: raise ETimeOut.Create('TSplitSocket.RecvPacket: Timeout waiting for BufferedPackets');
      else raise Exception.Create('TSplitSocket.RecvPacket: ERROR while waiting for BufferedPackets');
      end;
  end;
end;

procedure TSplitSocket.SendPacket(var aData; aSize: Word);
begin
  with FHostSocket as TSocketMultiplex do
  begin
    SendBuf(aData,aSize,WorkProcessIndex);
  end;
end;

function TSocketMultiplex.OpenTunnel(PI: Integer; var Tunnel: TSplitSocket): Boolean;
var call: byte;
    p: pointer;
    s: word;
begin
  OutputDebugString(PChar('ThreadID: ' + IntToStr(GetCurrentThreadID) + ' OpenTunnel[' + IntToStr(PI) + ']'));

  tunnel := TSplitSocket.Create(Self, True);
  tunnel.WorkProcessIndex := PI;
  AddTunnel(tunnel);
  call := 2;
  tunnel.SendPacket(call,Sizeof(call));

  OutputDebugString(PChar('ThreadID: ' + IntToStr(GetCurrentThreadID) + ' OpenTunnel[' + IntToStr(PI) + ']Requesting...!'));

  tunnel.RecvPacket(p,s);
    
  call := Byte(p^);
  FreeMem(p);
  Result := (call = 3);
  if not Result then
  begin
    tunnel.Free;

    OutputDebugString(PChar('ThreadID: ' + IntToStr(GetCurrentThreadID) + ' OpenTunnel[' + IntToStr(PI) + ']Denied!'));

  end
  else
  begin
    OutputDebugString(PChar('ThreadID: ' + IntToStr(GetCurrentThreadID) + ' OpenTunnel[' + IntToStr(PI) + ']Accepted!'));
    if Assigned(OnOpenedTunnel) then
      OnOpenedTunnel(Self,Tunnel);
  end;

  OutputDebugString(PChar('ThreadID: ' + IntToStr(GetCurrentThreadID) + ' OpenTunnel[' + IntToStr(PI) + ']Finished'));
end;

procedure TSocketMultiplex.AddTunnel(tunnel: TSplitSocket);
begin
  tunnel.FHostSocket := self;  //wird eigentlich schon bei
    //TSplitSocket.Create gemacht!
  FTunnel.Add(tunnel);
end;

function TSocketMultiplex.AddTunnelEnd(PI: TProcessIndex): TSplitSocket;
var tunnel: TSplitSocket;
begin
  tunnel := TSplitSocket.Create(Self, False);
  tunnel.WorkProcessIndex := PI;
  AddTunnel(tunnel);
  Result := tunnel;
end;

procedure TSocketMultiplex.Close;
begin
  Socket.Close;
end;

function TSocketMultiplex.____SplitSocketList: TThreadList;
begin
  Result := FTunnel;
end;

procedure TSocketMultiplex.DoUnusedPacket(Header: TSocketMultiplexHeader;
  Data: pointer);
var NewTunnel: TSplitSocket;
    Accept: Boolean;
begin
  Status := 'Unused Packet[' + IntToStr(Header.ProcessIndex) +
            ']->Size:' + IntToStr(Header.Size);

  if (Header.Size = Sizeof(Byte))and(Byte(Data^) = 2) then
  begin
    Status := 'OpenTunnel Request[' + IntToStr(Header.ProcessIndex) + ']';
    NewTunnel := TSplitSocket.Create(Self, False);
    NewTunnel.WorkProcessIndex := Header.ProcessIndex;

    Accept := True;
    if Assigned(OnOpeningTunnel_ReadThread) then
      OnOpeningTunnel_ReadThread(Self,NewTunnel,Accept);

    if Accept then
    begin
      Byte(Data^) := 3;
      NewTunnel.SendPacket(Data^,Sizeof(Byte));
      AddTunnel(NewTunnel);
      Status := 'OpenTunnel Request[' + IntToStr(Header.ProcessIndex) +
                '] Accepted!';
      If Assigned(OnOpenedTunnel) then
        SynchronizeNotifyEvent(Sync_OnOpenedTunnel_Event,NewTunnel);
    end
    else
    begin
      Byte(Data^) := 1;
      NewTunnel.SendPacket(Data^,Sizeof(Byte));
      NewTunnel.Free;
      Status := 'OpenTunnel Request[' + IntToStr(Header.ProcessIndex) +
                '] Denied!';
    end;

    Status := 'OpenTunnel Request[' + IntToStr(Header.ProcessIndex) +
                '] Finish';
  end
  else raise Exception.Create('procedure TSocketMultiplex.DoUnusedPacket:' +
               'There is a BlindPacket[' + IntToStr(Header.ProcessIndex) +
               ']->Size:' + IntToStr(Header.Size));
                       
  FreeMem(Data);
end;

function TSocketMultiplex.RemoteHost: TTCPHost;
begin
  Result := Socket.RemoteHost;
end;

function TSocketMultiplex.____FindTunnel(PI: Word): TSplitSocket;
var i: Integer;
begin
  Result := nil;
  with FTunnel.LockList do
  try
    for i := 0 to Count-1 do
      if (TSplitSocket(Items[i]).WorkProcessIndex = PI) then
        Result := TSplitSocket(Items[i]);
  finally
    FTunnel.UnlockList;
  end;
end;

procedure TSocketMultiplex.StartWorking;
begin
  Resume;
end;

function TThreadDataSocket.LockData: pointer;
begin
  FDataCriticalSection.Enter;
  Result := FData;
end;

procedure TThreadDataSocket.UnlockData;
begin
  FDataCriticalSection.Leave;
end;

procedure TThreadDataSocket.SetData_locked_(const Data: pointer);
begin  //Es muss unbedingt vorher LockData und dannach UnlockData erfolgen!
  FData := Data;
end;

procedure TSocketMultiplex.ProcessPacket;
var NextHeader: TSocketMultiplexHeader;
    i, WorkAt: integer;
    data: pointer;
begin
  Status := 'Packet Read ...';
  Socket.ReceiveBuf(NextHeader,sizeof(NextHeader));   //Warten auf Packet
  GetMem(data,NextHeader.Size);
  Socket.ReceiveBuf(data^,NextHeader.Size);

  WorkAt := 0;
  with FTunnel.LockList do
  try
    for i := 0 to Count-1 do
    begin
      if (NextHeader.ProcessIndex = TSplitSocket(Items[i]).WorkProcessIndex) then
      begin
        Status := 'Forward Packet[' + IntToStr(NextHeader.ProcessIndex) +
                  ']->Size: ' + IntToStr(NextHeader.Size);
        SendPacketAtSplitSocket(NextHeader,data,WorkAt,TSplitSocket(Items[i]));
        inc(WorkAt);
      end;
    end;
  finally
    FTunnel.UnlockList;
  end;

  if (WorkAt = 0) then
    DoUnusedPacket(NextHeader,Data);
end;

constructor TThreadDataSocket.Create;
begin
  inherited;
  FData := nil;
  FDataCriticalSection := TCriticalSection.Create;
end;

destructor TThreadDataSocket.Destroy;
begin
  FDataCriticalSection.Free;
  inherited;
end;

function TSocketMultiplex.LockData: pointer;
begin
  Result := FData.LockData;
end;

procedure TSocketMultiplex.SetData_locked_(const Data: pointer);
begin
  FData.SetData_locked_(Data);
end;

procedure TSocketMultiplex.UnlockData;
begin
  FData.UnlockData;
end;

procedure TSocketMultiplex.Sync_OnOpenedTunnel_Event(Sender: TObject);
begin
  if Assigned(OnOpenedTunnel) then
    OnOpenedTunnel(Self,TSplitSocket(Sender));
end;

end.
