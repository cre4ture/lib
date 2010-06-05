unit SplitSocket;

//{$define nodebugstrings}

interface

uses
  {$ifdef Lazarus}TCPSocket_Lazarus{$else}TCPSocket{$endif}, StatusThread, SysUtils, Classes,
    Windows, Contnrs, SyncObjs, myevent;

type
  ETimeOut = class(Exception);
  EUnhandledException = class(Exception);
  ESplitSocketDestroyed = class(Exception);
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
    LastLockDataDesc: String;
    function LockData(Description: String): pointer;
    procedure UnlockData;
    procedure SetData_locked_(const Data: pointer);
    constructor Create;
    destructor Destroy; override;
  end;

  {#############################################################################
  TSplitSocket:
  Pufferobject von Packeten (ThreadSicher!)

  Prüfung: 2.9.2007 durch Ulrich Hornung -> Alles sicher!
    ACHTUNG: Die On.. Ereigniss-Felder sind nicht ThreadSicher!
      (Dies sollte aber kein Problem darstellen, solange sie nicht "im Betrieb"
       verändert werden) 
  #############################################################################}
  TSplitSocket = class(TThreadDataSocket)
  private
    PacketQueue: TQueue;
    LockPacketQueue: TMyEvent;
    BufferedPackets: TMyEvent;

    FHostSocket: TObject;
    FMaster: Boolean;
    FWorkProcessIndex: TProcessIndex;
  protected
    procedure NewPacket(Sender: TObject; Header: TSocketMultiplexHeader;
      Data: pointer);
  public
    OnNewPacket_ReadThread: TNotifyEvent;
    OnDestroy: TNotifyEvent;
    property WorkProcessIndex: TProcessIndex read FWorkProcessIndex;
    property Master: Boolean read FMaster;
    property HostSocket: TObject read FHostSocket;
    constructor Create(AHostSocket: TObject; AMaster: Boolean;
      ProcessIndex: TProcessIndex);
    destructor Destroy; override;
    procedure SendPacket(var aData; aSize: Word);
    procedure RecvPacket(var Data: pointer; var Size: Word;
      const TimeOut: Cardinal = high(cardinal));
    function GetPacket(var Data: pointer; var Size: Word): Boolean; overload;
    function GetPacket(var Data: pointer;
      var Size, ProcessIndex: Word): Boolean; overload;
    function GetBufferCount: Integer;
  end;
  TSocketMultiplex_ForEachTunnel = procedure(Sender: TObject;
      Socket: TSplitSocket; Data: pointer) of object;
  TSocketMultiplexOpeningTunnel = procedure(Sender: TObject; Tunnel: TSplitSocket; var Accept: Boolean) of Object;
  TSocketMultiplexOpenedTunnel = procedure(Sender: TObject; Tunnel: TSplitSocket) of object;
  TSocketMultiplex = class(TSThread)
  private
    ThreadStopp: TEvent;
    CanWrite: TEvent;
    FTunnel: TThreadList;
    Socket: TTCPSocket;
    FMaster: Boolean;
    FData: TThreadDataSocket;
    procedure SendPacketToSplitSocket(Header: TSocketMultiplexHeader;
      Data: pointer; WorkAt: Integer; ASocket: TSplitSocket);
    procedure AddTunnel(tunnel: TSplitSocket);
    procedure DoUnusedPacket(Header: TSocketMultiplexHeader; Data: pointer);
    procedure ProcessPacket;
    procedure Sync_OnOpenedTunnel_Event(Sender: TObject);
  public
    OnThreadExit: TNotifyEvent;
    OnDestroy: TNotifyEvent;
    OnOpeningTunnel_ReadThread: TSocketMultiplexOpeningTunnel;
    OnOpenedTunnel: TSocketMultiplexOpenedTunnel;
    constructor Create(ASocket: TTCPSocket; AMaster: Boolean);
    procedure Execute; override;
    destructor Destroy; override;
    procedure SendBuf(var Buf; Size, ProcessIndex: Word); virtual; //virtual nur für fullbuffered!
    function OpenTunnel(PI: Integer; var Tunnel: TSplitSocket): Boolean;
    function AddTunnelEnd(PI: TProcessIndex): TSplitSocket;
    procedure Close;
    function ____SplitSocketList: TThreadList;
    property Master: Boolean read FMaster;
    function RemoteHost: TTCPHost;
    procedure ForEachTunnel(Proc: TSocketMultiplex_ForEachTunnel;
      Data: Pointer);
    function TunnelExists(WPI: Word): Boolean;
    procedure StartWorking;
    function LockData(Description: String): pointer;
    procedure SetData_locked_(const Data: pointer);
    procedure UnlockData;
  end;

  TSocketMultiplexFullBuffered = class(TSocketMultiplex)
  private
    //Because of the thread-save PacketQueue of TSplitSocket, we use it also
    //for sending packets!
    FSendBuffer: TSplitSocket;
    FSendThread: TThread;
  protected
    procedure inherited_SendBuf(var Data; Size, ProcessIndex: Word);
  public
    constructor Create(ASocket: TTCPSocket; AMaster: Boolean);
    destructor Destroy; override;
    procedure SendBuf(var Buf; Size, ProzessIndex: Word); override;
    function GetSendBufferCount: Integer;
  end;

  TSMFB_ST_BufferProtPacket = record
    bppData: pointer;
    bppSize: Word;
    bppPI: Word;
  end;
  TSocketMultiplex_SendThread = class(TSThread)
  protected
    Buffer: TSplitSocket;
    Socket: TSocketMultiplexFullBuffered;
  public
    constructor Create(HostSocket: TSocketMultiplexFullBuffered;
      SendBuffer: TSplitSocket);
    procedure Execute; override;

  end;

const TimeOut = 3000;

procedure CheckMainThread(Description: String);

implementation

uses DateUtils, ComObj;

{$ifdef nodebugstrings}
procedure OutputDebugString(lpOutputString: PChar);
begin
  //Um Die Ausgabe von Debug-Strings zu verhindern, "verstecke" ich die function
  //einfach, indem ich eine lokale function mit dem gleichen namen mache!
  //windows.OutputDebugString( lpOutputString);
end;
{$endif}

procedure TSocketMultiplex.SendBuf(var Buf; Size, ProcessIndex: Word);
var Header: TSocketMultiplexHeader;
begin
  Header.Size := Size;
  Header.ProcessIndex := ProcessIndex;

  Status := 'TSocketMultiplex.SendBuf';
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
  Name := 'SMux(' + RemoteHost.IP + ':' + IntToStr(RemoteHost.Port) + ')';
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

  if Assigned(OnDestroy) then
    OnDestroy(Self);

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
    on E: EUnhandledException do
    begin
      //Nichts, einfach nur beenden!
    end;
    on E: Exception do
    begin
      Status := 'Exception in ' + Name + ':' + E.Message + ' (' + E.ClassName + ')';
      If Assigned(OnHandleException) then OnHandleException(Self,E,Status);
        //Hier solte normalerweise nie eine Exception abgefangen werden!
        //(siehe SendPacketToSplitSocket)
    end;
  end;
  
  if Assigned(OnThreadExit) then
    SynchronizeNotifyEvent(OnThreadExit,Self);

  ReturnValue := 0;
end;

constructor TSplitSocket.Create(AHostSocket: TObject; AMaster: Boolean;
  ProcessIndex: TProcessIndex);
begin
  inherited Create;
  FMaster := AMaster;
  FHostSocket := AHostSocket;
  FWorkProcessIndex := ProcessIndex;
  PacketQueue := TQueue.Create;
  LockPacketQueue := TMyEvent.Create(False, True);
  BufferedPackets := TMyEvent.Create(True, False);
end;

destructor TSplitSocket.Destroy;
var p: pointer;
    s: word;
begin
  if Assigned(OnDestroy) then
    OnDestroy(Self);

  while GetPacket(p,s) do //übrige Packete Freigeben!!
    FreeMem(p);

  BufferedPackets.Free;
  LockPacketQueue.Free;
  PacketQueue.Free;
  inherited;
end;

function TSplitSocket.GetBufferCount: Integer;
begin
  //OutputDebugString('TSplitSocket.GetBufferCount');
  if LockPacketQueue.WaitFor(TimeOut) = merSignaled then
  begin
    try
      Result := PacketQueue.Count;
    finally
      LockPacketQueue.SetEvent;
    end;
  end
  else raise Exception.Create('TSplitSocket.GetBufferedPacketCount: ' +
               'ERROR waiting for LockPacketQueue');
end;

{###############################################################################
 TSplitSocket.GetPacket

 Liest, falls vorhanden, das nächste Packet. Wenn kein Packet mehr im "Buffer"
 ist, gibt die Funktion False zurück!

 Diese Funktion Blockiert nicht!

 Verwende RecvPacket für blockierenden Aufruf!
###############################################################################}
function TSplitSocket.GetPacket(var Data: pointer; var Size: Word): Boolean;
var pi: Word;
begin
  Result := GetPacket(Data,Size,pi);
end;

function TSplitSocket.GetPacket(var Data: pointer;
  var Size, ProcessIndex: Word): Boolean;
var Packet: PSplitSocketPacket;
begin
  case LockPacketQueue.WaitFor(high(cardinal)) of
  merSignaled:        //warten bis queue freigegeben, sollte normal nicht lange dauern! (timeout zwecklos!) evtl. critical section!?
    begin
      try
        //checken ob Packet vohanden!
        Result := PacketQueue.Count > 0;
        if Result then
        begin
          Packet := PacketQueue.Pop;
          Size := Packet^.Header.Size;
          ProcessIndex := Packet^.Header.ProcessIndex;
          Data := Packet^.Data;
          FreeMem(Packet);
        end;

        //Wenn jetzt keins mehr vorhanden, dies melden:
        if (PacketQueue.Count = 0) then
          BufferedPackets.ResetEvent;  ///^=keine mehr da!

      finally
        LockPacketQueue.SetEvent;
      end;
    end;
  merTimeOut: raise ETimeOut.Create(
    'TSplitSocket.GetPacket: Timeout while waiting for LockPacketQueue');
  merDestroyed: raise ESplitSocketDestroyed.Create(
    'TSplitSocket.GetPacket: LockPacketQueue is Destroying');
  else raise Exception.Create(
    'TSplitSocket.GetPacket: ERROR while waiting for LockPacketQueue');
  end;
end;

procedure TSplitSocket.NewPacket(Sender: TObject;
  Header: TSocketMultiplexHeader; Data: pointer);
var Packet: PSplitSocketPacket;
begin
  GetMem(Packet,Sizeof(Packet));
  Packet^.Header := Header;
  Packet^.Data := Data;

  //OutputDebugString(PChar('tid' + inttostr(GetCurrentThreadID) + ': TSplitSocket.NewPacket'));
  if LockPacketQueue.WaitFor(TimeOut) = merSignaled then
  begin
    try
      PacketQueue.Push(Packet);
      //Melden das wieder Packete gibt:
      BufferedPackets.SetEvent;
    finally
      LockPacketQueue.SetEvent;
    end;

    if Assigned(OnNewPacket_ReadThread) then
      OnNewPacket_ReadThread(Self);

  end
  else raise Exception.Create('TSplitSocket.NewPacket: ERROR waiting for LockPacketQueue');
end;

procedure TSocketMultiplex.SendPacketToSplitSocket(Header: TSocketMultiplexHeader;
  Data: pointer; WorkAt: Integer; ASocket: TSplitSocket);
begin
  try
    ASocket.NewPacket(Self,Header,Data);
  except
    on E: Exception do
    begin
      Status := 'Exception in ' + Name + ':' + E.Message + ' (' + E.ClassName + ')';
      If not(Assigned(OnHandleException) and OnHandleException(Self,E,Status)) then
        raise EUnhandledException.Create('TSocketMultiplex.SendPacketToSplitSocket: Unhandled Exception');
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
procedure TSplitSocket.RecvPacket(var Data: pointer; var Size: Word;
  const TimeOut: Cardinal = high(cardinal));
var ready: Boolean;
begin
  ready := GetPacket(Data,Size);
  while (not ready) do
  begin
    //Wenn dies der Readthread vom HostSocket ist, dann muss ProcessPacket
    //hier extra aufgerufen werden! (Der Timeout klappt dann aber nicht!)
    if (HostSocket <> nil)and
       (GetCurrentThreadId = TSocketMultiplex(HostSocket).Handle) then
    begin
      //Nächstes Packet lesen lassen:
      TSocketMultiplex(HostSocket).ProcessPacket;
      //Schauen ob jetzt eins für uns da ist:
      ready := GetPacket(Data,Size);
    end
    else
      case BufferedPackets.WaitFor(TimeOut) of
        merSignaled: ready := GetPacket(Data,Size);
        merTimeOut: raise ETimeOut.Create('TSplitSocket.RecvPacket: Timeout waiting for BufferedPackets');
        merDestroyed: raise ESplitSocketDestroyed.Create('TSplitSocket.RecvPacket: Self is Destroying');
        else raise Exception.Create('TSplitSocket.RecvPacket: ERROR while waiting for BufferedPackets');
      end;
  end;
end;

procedure TSplitSocket.SendPacket(var aData; aSize: Word);
begin
  if (FHostSocket = nil) then
    raise Exception.Create('TSplitSocket.SendPacket: HostSocket = nil!');

  with FHostSocket as TSocketMultiplex do
  begin
    SendBuf(aData,aSize,WorkProcessIndex);
  end;
end;

function TSocketMultiplex.OpenTunnel(PI: Integer; var Tunnel: TSplitSocket): Boolean;
var call: array[0..1023] of byte;
    p: pointer;
    s: word;
begin
  Status := 'OpenTunnel[' + IntToStr(PI) + ']';

  tunnel := TSplitSocket.Create(Self, True, PI);
  AddTunnel(tunnel);
  call[0] := 2;
  //tunnel.SendPacket(call,Sizeof(call));
  Self.SendBuf(Call,Sizeof(Call),PI);

  Status := 'OpenTunnel[' + IntToStr(PI) + ']Requesting...!';

  tunnel.RecvPacket(p,s);

  if (s <> Sizeof(Call)) then
    raise Exception.Create('TSocketMultiplex.OpenTunnel: Error: Packet mit falscher Größe!');

  CopyMemory(@Call,p,s);
  FreeMem(p);
  Result := (call[0] = 3);
  if not Result then
  begin
    tunnel.Free;

    Status := ' OpenTunnel[' + IntToStr(PI) + ']Denied!';

  end
  else
  begin
    Status := ' OpenTunnel[' + IntToStr(PI) + ']Accepted!';
    if Assigned(OnOpenedTunnel) then
      OnOpenedTunnel(Self,Tunnel);
  end;

  Status := 'OpenTunnel[' + IntToStr(PI) + ']Finished';
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
  tunnel := TSplitSocket.Create(Self, False, PI);
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
    Call: array[0..1023] of byte;
begin
  Call[0] := 1; //prevent Compiler warning!
  
  Status := 'Unused Packet[' + IntToStr(Header.ProcessIndex) +
            ']->Size:' + IntToStr(Header.Size);

  if (Header.Size = Sizeof(Call))and(Byte(Data^) = 2) then
  begin
    Status := 'OpenTunnel Request[' + IntToStr(Header.ProcessIndex) + ']';
    NewTunnel := TSplitSocket.Create(Self, False, Header.ProcessIndex);

    Accept := True;
    if Assigned(OnOpeningTunnel_ReadThread) then
      OnOpeningTunnel_ReadThread(Self,NewTunnel,Accept);

    if Accept then
    begin
      Byte(Data^) := 3;
      NewTunnel.SendPacket(Data^,Header.Size);
      AddTunnel(NewTunnel);
      Status := 'OpenTunnel Request[' + IntToStr(Header.ProcessIndex) +
                '] Accepted!';
      If Assigned(OnOpenedTunnel) then
        SynchronizeNotifyEvent(Sync_OnOpenedTunnel_Event,NewTunnel);
    end
    else
    begin
      Byte(Data^) := 1;
      NewTunnel.SendPacket(Data^,Header.Size);
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

procedure TSocketMultiplex.ForEachTunnel(Proc: TSocketMultiplex_ForEachTunnel;
  Data: Pointer);
var i: Integer;
begin
  with FTunnel.LockList do
  try
    for i := 0 to Count-1 do
      Proc(Self, TSplitSocket(Items[i]), Data);
  finally
    FTunnel.UnlockList;
  end;
end;

function TSocketMultiplex.TunnelExists(WPI: Word): Boolean;
var i: Integer;
begin
  Result := False;
  with FTunnel.LockList do
  try
    for i := 0 to Count-1 do
      if TSplitSocket(Items[i]).FWorkProcessIndex = WPI then
      begin
        Result := True;
        Break;
      end;
  finally
    FTunnel.UnlockList;
  end;
end;

procedure TSocketMultiplex.StartWorking;
begin
  Resume;
end;

function TThreadDataSocket.LockData(Description: String): pointer;
begin
  FDataCriticalSection.Enter;
  LastLockDataDesc := Description;
  {OutputDebugString(PChar(Self.ClassName + '.LockData: ' +
                    IntToHex(Cardinal(self),6) + ' Locked! ' + Description)); }
  Result := FData;
end;

procedure TThreadDataSocket.UnlockData;
begin
  FDataCriticalSection.Leave;
  {OutputDebugString(PChar(Self.ClassName + '.UnlockData: ' +
                    IntToHex(Cardinal(self),6) + ' Unlocked!')); }
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
  Socket.ReceiveBuf(NextHeader, sizeof(NextHeader));   //Warten auf Packet
  GetMem(data,NextHeader.Size);
  Socket.ReceiveBuf(data^, NextHeader.Size);

  WorkAt := 0;
  with FTunnel.LockList do
  try
    for i := 0 to Count-1 do
    begin
      if (NextHeader.ProcessIndex = TSplitSocket(Items[i]).WorkProcessIndex) then
      begin
        Status := 'Forward Packet[' + IntToStr(NextHeader.ProcessIndex) +
                  ']->Size: ' + IntToStr(NextHeader.Size);
        SendPacketToSplitSocket(NextHeader,data,WorkAt,TSplitSocket(Items[i]));
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

function TSocketMultiplex.LockData(Description: String): pointer;
begin
  Result := FData.LockData(Description);
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

constructor TSocketMultiplexFullBuffered.Create(ASocket: TTCPSocket;
  AMaster: Boolean);
begin
  inherited;
  FSendBuffer := TSplitSocket.Create(nil,false, 0);
  FSendThread := TSocketMultiplex_SendThread.Create(Self,FSendBuffer);
end;

destructor TSocketMultiplexFullBuffered.Destroy;
begin
  inherited;

  //Thread anhalten:
  FSendThread.Terminate;
  OutputDebugString('TERMINATED');
  //SendBuffer freigeben: (Sendthread hängt womöglich gerade bei SendBuffer.GetPacket)
  FSendBuffer.Free;
  OutputDebugString('FREE');
  FSendBuffer := nil;
  //Angehaltenen Thread freigeben:
  FSendThread.Free;
  FSendThread := nil;
end;

constructor TSocketMultiplex_SendThread.Create(
  HostSocket: TSocketMultiplexFullBuffered; SendBuffer: TSplitSocket);
begin
  Buffer := SendBuffer;
  Socket := HostSocket;
  inherited Create(False);
end;

procedure TSocketMultiplex_SendThread.Execute;
var Data: pointer;
    Size: Word;
begin
  try
    while (not Terminated) do
    begin
      OutputDebugString('TSocketMultiplex_SendThread.Execute: Warten auf neues Packet zum verschicken');
      Buffer.RecvPacket(Data,Size);
      if (Size <> SizeOf(TSMFB_ST_BufferProtPacket))then raise Exception.Create(
        'TSocketMultiplex_SendThread.Execute: Error im Bufferprotokoll!!!');

      with TSMFB_ST_BufferProtPacket(Data^) do
      begin
        //SendBuf wurde in *FullBuffered zum zwecke des Zwischenpufferns
        //überschrieben:
        //Jetzt echt losschicken:
        OutputDebugString('TSocketMultiplex_SendThread.Execute: Packet verschicken');
        Socket.inherited_SendBuf(bppData^,bppSize,bppPI);

        //gesendete Daten freigeben:
        FreeMem(bppData);
      end;
      FreeMem(Data);
    end;
  finally
    OutputDebugString('TSocketMultiplex_SendThread.Execute: FINISHED!');
  end;
end;

function TSocketMultiplexFullBuffered.GetSendBufferCount: Integer;
begin
  Result := FSendBuffer.GetBufferCount;
end;

procedure TSocketMultiplexFullBuffered.inherited_SendBuf(var Data; Size,
  ProcessIndex: Word);
begin
  //SendBuf wurde in *FullBuffered zum zwecke des Zwischenpufferns
  //überschrieben, muss aber für den SendThread trotzdem verfügbar sein!
  inherited SendBuf(Data,Size,ProcessIndex);
end;

procedure TSocketMultiplexFullBuffered.SendBuf(var Buf; Size,
  ProzessIndex: Word);
var bpp: pointer;
    Header: TSocketMultiplexHeader;
begin
  Header.ProcessIndex := 0;
  Header.Size := SizeOf(TSMFB_ST_BufferProtPacket);
  GetMem(bpp,Header.Size);
  with TSMFB_ST_BufferProtPacket(bpp^) do
  begin
    GetMem(bppData,Size);
    CopyMemory(bppData,@buf,Size);
    bppSize := Size;
    bppPI := ProzessIndex;
  end;
  FSendBuffer.NewPacket(Self,Header,bpp);
end;

procedure CheckMainThread(Description: String);
begin
  if (GetCurrentThreadId <> MainThreadID) then
    raise Exception.Create('Call must be in Mainthread! (' + Description + ')');
end;

end.
