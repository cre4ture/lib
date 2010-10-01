unit TThreadSocketSplitter;

interface

uses
  syncobjs, Dialogs, Sysutils, Windows, classes, winsock, {$ifdef Lazarus}TCPSocket_Lazarus{$else}TCPSocket{$endif},
  ReadWriteEvent, StatusThread;

const
  TimeOut = 10000;
  Thread_Working = 300;
  Thread_Finished = 301;
  Thread_SuspendedBeforeExit = 309;
  Thread_ReadFromSocket = 310;

type
 {##############################################################################
  #                                                                            #
  #  TEventEx zählt die Anzahl Wartender Threads mit!                          #
  #                                                                            #
  ##############################################################################}
  TEventEx = class(TEvent)
  private
    FThreadsWaiting: Integer;
  public
    property ThreadsWaiting: Integer read FThreadsWaiting;
    function WaitFor(Timeout: DWORD): TWaitResult; // change by creature: "error caused by static function"  override;
    constructor Create(EventAttributes: PSecurityAttributes; AManualReset,
      InitialState: Boolean; const Name: string);
  end;
  
  ETerminated = class(Exception);
  EDeMultiplexer = class(Exception);

  TExceptionEvent = procedure(Sender: TThread; E: Exception; msg: String) of object;
  TSpltPacket1 = record
    Size: Word;
    ProzessIndex: Word;
  end;
  TDeMultiplexerOnNoneThreadData = procedure(Sender: TObject; var Data; Size, ProcIndex: Word) of object;
  TDeMultiplexer = class(TSThread)
  private
  protected
    Socket: TTCPSocket;           //DIE Verbindung zur Gegenstelle!
    NextLayer: TSpltPacket1;      //Verwendet Execute als ReadBuffer und zum Austausch der daten mit RecvBuf!
    SyncData: pointer;            //Wird von "Execute" zum Lesen von "Notes" und hauptsächlich zum DatenAustausch mit "SyncOnNoneThreadData" verwendet!
    CanWrite,                     //Wírd von "SendBuf" verwendet um gleichzeitigen Zugriff auf Socket.SendBuf zu verhindern!
    NewLayer,                     //Execute setzt dieses Event -> RecvBuf wartet darauf und gibt (setzt erneut) es bei Bedarf weiter!
    LayerProcessed: TEventEx;     //Execute wartet hier bis dieses Event von RecvBuf aufgerufen wird und damit die erfolgrieche Abarbeitung von NextLayer signalisiert.
    procedure SyncOnNoneThreadData;
    procedure SyncOnEndConnection;
    procedure CloseRecvBufLoops;
  public
    nTLimit: Word;
    ThreadAsync: Word;
    ThreadLimit: Word;
    NoteIndex: Word;
    OnNoneThreadData: TDeMultiplexerOnNoneThreadData;
    OnEndConnection: TNotifyEvent;
    {$ifndef Lazarus}OnException: TExceptionEvent;{$endif}
    property TCPSocket: TTCPSocket read Socket;
    constructor Create(ASocket: TTCPSocket);
    procedure SendBuf(var buf; Size, ProcIndex: Word; note: string);
    procedure Execute; override;
    destructor Destroy; override;
    //function RecvBufLength(ProcessID: Word): word; brauchts nicht!
    function DataAvailable(ProcessID: Word; var Size: Word): Boolean; overload;
    function DataAvailable(ProcessID: Word): Boolean; overload;
    function RecvBuf(var Buf; Size, ProcIndex: Word; SmallPacketAllowed: Boolean = False): Integer;
  end;
  TClientThreadEvent = function(Sender: TThread): Integer of object;
  TClientThreadNoThreadData = procedure(Sender: TThread; var Data; Size, ProcIndex: Word) of object;
  TClientThread = class(TSThread)
  protected
    FOnException: TExceptionEvent;
    procedure SocketOnNoThreadData(Sender: TObject; var Data; Size, ProcIndex: Word);
    procedure SocketOnEndConnection(Sender: TObject);
    procedure Sync_ThreadEnd;
    procedure SelfOnThreadStatusChanged(Sender: TObject);
    procedure SetOnException(p: TExceptionEvent);
    function GetOnException: TExceptionEvent;
  public
    Socket: TDeMultiplexer;
    Data: Pointer;
    Server: Boolean;
    OnNoThreadData: TClientThreadNoThreadData;
    OnDoLogin: TClientThreadEvent;
    OnDoSync: TClientThreadEvent;
    OnThreadEnd: TNotifyEvent;
    property OnException: TExceptionEvent read GetOnException write SetOnException;
    property Terminated;
    constructor Create(CreateSuspended: Boolean; ASocket: TTCPSocket);
    procedure Execute; override;
    destructor Destroy; override;
    procedure SendBuf(var buf; Size, ProcIndex: Word; note: string);   //SendBuf und RecvBuf sind einfach nur weiterleitungen zum DeMultiplexer!
    procedure RecvBuf(var Buf; Size, ProcIndex: Word);   //Es werden manchmal direkt die DeMultiplexerfunktionen gestarten, und manchmal diese hier!
  end;
  TOnClientThreadHostEvent = function(Sender: TObject; ClientThread: TClientThread; ServerSite: Boolean): integer of object;
  TClientThreadHost_noThDa = procedure(Sender: TThread; ClientThread: TClientThread; var Data; Size, ProcIndex: Word) of object;
  TnoThDaProc = record
    ProcIndex: word;
    Proc: TClientThreadHost_noThDa;
  end;
  TFilterFunction = function(AClientThread: TClientThread): Boolean;
  TClientThreadHost = class(TSThread)
  private
    procedure Server_ClientConnect(Sender: TObject; newSocket: TSocket);
  protected
    Sync_OnClientConnect_ClientThread: TClientThread;
    NTDProcs: array of TnoThDaProc;
    EV_Clients: TEvent;
    procedure Sync_OnClientConnect;
    procedure ClientThreadNoThreadData(Sender: TThread; var Data; Size, ProcIndex: Word);
    function ClientThreadLogin(Sender: TThread): Integer;
    function ClientThreadSync(Sender: TThread): Integer;
    procedure SetClientEvents(ClientThread: TClientThread);
    procedure ClientThreadEnd(Sender: TObject);
  public
    Clients: array of TClientThread;
    OnClientConnect: TOnClientThreadHostEvent;
    OnClientDisconnect: TOnClientThreadHostEvent;
    OnClientDoLogin: TOnClientThreadHostEvent;
    OnClientDoSync: TOnClientThreadHostEvent;
    ClientArrayReadWrite: TReadWriteEvent;
    {$ifndef Lazarus}OnException: TExceptionEvent;{$endif}
    Server: TSimpleServer;
    constructor Create;
    procedure Connect(IP: String; Port: Integer);
    procedure AddNoThreadDataProc(ProcIndex: Word; Proc: TClientThreadHost_noThDa);
    procedure SendBuf(var buf; Size, ProcID: Word; DontSendAt: TClientThread; Filter: TFilterFunction; note: string);
    destructor Destroy; override;
    procedure Disconnect(Index: Integer);
    procedure Execute; override;
    {procedure LockClientList;
    procedure ReleaseClientList;}
  end;

var
  SendNotes: Boolean;

implementation

uses Math;

procedure TClientThread.SocketOnNoThreadData(Sender: TObject; var Data; Size, ProcIndex: Word);
begin
  if Assigned(OnNoThreadData) then
    OnNoThreadData(Self,Data,Size,ProcIndex);
end;

procedure TClientThreadHost.AddNoThreadDataProc(ProcIndex: Word;
  Proc: TClientThreadHost_noThDa);
var i: integer;
begin
  i := length(NTDProcs);
  SetLength(NTDProcs,i+1);
  NTDProcs[i].ProcIndex := ProcIndex;
  NTDProcs[i].Proc := Proc;
end;

procedure TClientThreadHost.ClientThreadNoThreadData(Sender: TThread; var Data; Size, ProcIndex: Word);
var i: integer;
begin
  for i := 0 to length(NTDProcs)-1 do
  if NTDProcs[i].ProcIndex = ProcIndex then
  begin
    if Assigned(NTDProcs[i].Proc) then
      NTDProcs[i].Proc(Self,TClientThread(Sender),Data,Size,ProcIndex);
    break;
  end;
end;

constructor TClientThread.Create(CreateSuspended: Boolean; ASocket: TTCPSocket);
begin
  Socket := TDeMultiplexer.Create(ASocket);
  Socket.OnNoneThreadData := {$ifdef Lazarus}@{$endif}SocketOnNoThreadData;
  Socket.OnEndConnection := {$ifdef Lazarus}@{$endif}SocketOnEndConnection;
  
  OnOnThreadStatusChange := {$ifdef Lazarus}@{$endif}SelfOnThreadStatusChanged;

  Data := nil;
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  Name := Name + ' ' + ASocket.RemoteHost.IP + ':' + IntToStr(ASocket.RemoteHost.Port);
end;

procedure TClientThread.Execute;
var r: integer;
begin
  ReturnValue := Thread_Working;
  if Socket.Socket.Connected then
  begin
    Status := 'Run OnDoLogin; if Assigned';
    r := 0;
    try
      if Assigned(OnDoLogin) then r := OnDoLogin(Self);
      If r = 0 then
      begin
        Status := 'Run OnDoSynch; if Assigend';
        if Assigned(OnDoSync) then OnDoSync(Self);
      end;
    except
      on E: Exception do
      begin
        Status := 'Exception in ' + Name + ':' + E.Message + ' (' + E.ClassName + ')';
        if Assigned(FOnException) then FOnException(Self,E,Status);
        Terminate; //Wenn was Schief läuft, Trennen!
      end;
    end;

    if not Terminated then    //Pause nur dann, wenn alles Glattgelaufen ist
    begin
      Status := 'Pausiert, bis Verbindung beendet!';
      ReturnValue := Thread_SuspendedBeforeExit;
      Suspend;
      ReturnValue := Thread_Working;
    end;
    Terminate; 

    Status := 'leaving Execute';
  end;

  if Socket.ReturnValue <> Thread_Finished then
  begin
    Socket.Terminate;    //Beenden des DeMultiplexers
    Socket.Socket.Close; //Schließen dessen Sockets, damit er aus der Blockierenden Read-Function rauskommt
  end;
  
  if Assigned(OnThreadEnd) then
  begin
    Status := 'Run OnThreadEnd';
    Synchronize({$ifdef Lazarus}@{$endif}Sync_ThreadEnd);
  end;
  
  Status := 'Left Execute';
  ReturnValue := Thread_Finished;
end;

constructor TDeMultiplexer.Create(ASocket: TTCPSocket);
begin //braucht schon verbundenen Socket!
  Socket := ASocket;

  CanWrite := TEventEx.Create(nil,False,True,'');
  NewLayer := TEventEx.Create(nil,False,False,'');
  LayerProcessed := TEventEx.Create(nil,False,False,'');

  NoteIndex := 5;
  ThreadAsync := 512;
  nTLimit := 1024;
  ThreadLimit := High(ThreadLimit);
  FreeOnTerminate := False;
  inherited Create(false);
  Name := Name + ' ' + ASocket.RemoteHost.IP + ':' + IntToStr(ASocket.RemoteHost.Port);
end;

destructor TDeMultiplexer.Destroy;
begin
  //FreeOnTerminate := False; is in Create sowiso so festgelegt
  Terminate;
  Socket.Close;
  CanWrite.Free;
  while (LayerProcessed.ThreadsWaiting > 0) do LayerProcessed.SetEvent;
  LayerProcessed.Free;
  if ReturnValue <> Thread_Finished then WaitFor;

  Socket.Free;
  Inherited Destroy;
end;

procedure TDeMultiplexer.Execute;
begin
  Status := 'Enter Execute';
  ReturnValue := Thread_Working;

  try
    while (not Terminated)and(Socket.Connected) do
    begin
      Status := 'Read ...';
      Socket.ReceiveBuf(NextLayer,sizeof(NextLayer));

      if (NextLayer.ProzessIndex = NoteIndex) then
      begin  //Die Möglichkeit der Gegenstelle eine Statusnotiz zu geben! 
        GetMem(SyncData,NextLayer.Size+1);
        FillChar(SyncData^,NextLayer.Size+1,0);
        Socket.ReceiveBuf(SyncData^,NextLayer.Size);
        Status := 'remote-note: ' + PChar(SyncData);
        FreeMem(SyncData);
      end
      else
      if (NextLayer.ProzessIndex <= nTLimit) then                                   //NoneThread-Bereich!
      begin
        GetMem(SyncData,NextLayer.Size);
        Status := 'Read noThreadData ' + IntToStr(NextLayer.ProzessIndex) + '/' + IntToStr(NextLayer.Size);
        Socket.ReceiveBuf(SyncData^,NextLayer.Size);
        if Assigned(OnNoneThreadData) then
        begin
          if (NextLayer.ProzessIndex >= ThreadAsync) then SyncOnNoneThreadData
                                         else Synchronize({$ifdef Lazarus}@{$endif}SyncOnNoneThreadData);
        end;
        FreeMem(SyncData);
      end
      else if (NextLayer.ProzessIndex <= ThreadLimit) then                          //Thread-Bereich!
      begin
        NewLayer.SetEvent;                                                        //Neues Layer da!
        Status := 'Wait for LayerProcessed ' + IntToStr(NextLayer.ProzessIndex) + '/' + IntToStr(NextLayer.Size);
        if (LayerProcessed.WaitFor(TimeOut) <> wrSignaled) then                   //Warte bis Layer verarbeitet!
        begin
          raise EDeMultiplexer.Create('API: Execute, Timeout beim warten auf Datenverarbeitung (index/size:' +
                                       IntToStr(NextLayer.ProzessIndex) + '/' + IntToStr(NextLayer.Size));
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      Status := 'Exception in ' + Name + ':' + E.Message + ' (' + E.ClassName + ')';
      If Assigned(OnException) then OnException(Self,E,Status);
    end;
  end;
  Status := 'Leaving Execute';

  Socket.Close;
  CloseRecvBufLoops; //Beende eventuelle RecvBuf-Schleifen beendet werden!
  
  if Assigned(OnEndConnection) then
  begin
    Status := 'Run OnEndConnection';
    Synchronize({$ifdef Lazarus}@{$endif}SyncOnEndConnection);
  end;

  Status := 'Left Execute';
  ReturnValue := Thread_Finished;
end;

{function TDeMultiplexer.RecvBuf
-> Wartet auf das Event NewLayer, das angibt, dass der multiplexer das nächste Packet gelesen hat.
-> Falls der ProcIndex stimmt, gibt die Funktion das Packet an die aufrufende Funktion zurück und setzt das Event LayerProcessed um dem Multiplexer zu sagen, das das Packet verarbeitet wurde und ein neues gelesen werden kann.
-> Wenn ProcIndex nicht stimmt, wird das Event NewLayer nochmal aufgerufen, um das aktuelle Packet an den nächsten Thread weiterzuleiten.
-> Wenn der Multiplexer terminiert wurde, wird eine Exception aufgerufen!
-> Wenn ein Timeout beim warten auf NewLayer auftritt, wird nochmal drauf gewartet. Es gibt also kein wirkliches Timeout hier!
-> Die angegebene Packetgröße muss mit der empfangenen übereinstimmen, außer SmallPacketAllowed ist True. Aber selbst dann darf die empfangene Größe auf keinen Fall >größer< sein als von der aufrufenden Function angegeben!}

function TDeMultiplexer.RecvBuf(var Buf; Size, ProcIndex: Word;
  SmallPacketAllowed: Boolean = False): Integer;
var ready: Boolean;
begin
  Result := -1;
  if Terminated then raise ETerminated.Create('API: RecvBuf, ReadThread terminiert!');

  ready := False;
  while (not ready) do
  begin
    ready := (NewLayer.WaitFor(TimeOut) = wrSignaled);
    if Terminated then raise ETerminated.Create('API: RecvBuf, ReadThread terminiert!');

    if ready then
    begin
      if (NextLayer.ProzessIndex = ProcIndex) then
      begin
        if (NextLayer.Size = Size)or((NextLayer.Size <= Size)and(SmallPacketAllowed)) then
        begin
          if NextLayer.Size > 0 then     //wenn null, dann dann blockt socket.recv solange bis wieder was kommt, auch wenn er eigentlich nix lesen müsste!
            Socket.ReceiveBuf(buf,Size);
            
          Result := NextLayer.Size;
          LayerProcessed.SetEvent;                                              //Layer wurde Verarbeitet!
        end
        else raise EDeMultiplexer.Create('API: RecvBuf, BufSizeDiff!');
      end //(NextLayer.ProzessIndex = ProcIndex)
      else
      begin
        NewLayer.SetEvent;                                                      //Weil Falscher Layer -> Starte Neue Layer nochmal("unecht"), damit andere Threads drannkommen!
        ready := False;
      end;
    end;
  end;
end;

{ procedure TDeMultiplexer.SendBuf
-> Sendet Daten über die im Multiplexer vorhandene Verbindung (Socket)
-> Zum sicherstellen, das keine 2 sendvorgänge gleichzeitig passieren können, wird das TEventobject CanWrite verwendet
-> CanWrite muss immer wieder gesetzt werden, falls kein Timeout beim Warten auf dieses Event aufgetreten ist!
-> Ruft eine Exception auf, falls der Multiplexer terminiert wurde, oder ein Timeout beim Warten auf CanWrite auftritt! }
procedure TDeMultiplexer.SendBuf(var buf; Size, ProcIndex: Word; note: string);
var Layer: TSpltPacket1;
begin
  Layer.Size := Size;
  Layer.ProzessIndex := ProcIndex;

  Status := 'Sende: ' + IntToStr(ProcIndex) + '/' + IntToStr(Size) + ' - ' + Note;

  if (CanWrite.WaitFor(TimeOut) = wrSignaled) then //Sperren!
  try

    while not Socket.SelectWrite(1000) do
      sleep(100);

    if Terminated then raise ETerminated.Create('API: SendBuf, ReadThread wurde terminiert!');

    Socket.SendBuf(Layer,sizeof(Layer));
    if Size > 0 then Socket.SendBuf(buf,size);

    if SendNotes and (note <> '') then   //NoteIndex - Packete werden direkt im DeMultiplexer-Execute-Thread verarbeitet!
    begin
      Layer.Size := length(note);
      Layer.ProzessIndex := NoteIndex;

      Socket.SendBuf(Layer,sizeof(Layer));
      if Layer.Size > 0 then Socket.SendBuf(PChar(note)^,Layer.Size);
    end;

  finally
    CanWrite.SetEvent;                             //Freigeben!
  end else raise ETerminated.Create('API: SendBuf, Timeout beim Warten auf Schreiberlaubnis');
end;

procedure TDeMultiplexer.SyncOnNoneThreadData;
begin
  {######
   #  Wird von Execute Aufgerufen wenn NoneThreadData angekommen sind! Diese Daten befinden sich dann in SyncData! 
   ######}
  OnNoneThreadData(Self,SyncData^,NextLayer.Size,NextLayer.ProzessIndex);
end;

procedure TClientThreadHost.Connect(IP: String; Port: Integer);
var i: integer;
    sock: TTCPSocket;
begin
  sock := TTCPSocket.Create(-1);
  Status := 'Verbinde mit "' + IP + ':' + IntToStr(Port) + '" ...';
  if sock.Connect(IP,Port) then
  begin
    Status := 'Verbunden! Erstelle Clientthread ...';
    if ClientArrayReadWrite.LockAll(TimeOut) then
    try
      i := length(Clients);
      Setlength(Clients,i+1);
      Clients[i] := TClientThread.Create(True,sock);
      Clients[i].Server := False;
      SetClientEvents(Clients[i]);
      if Assigned(OnClientConnect) then
        OnClientConnect(Self,Clients[i],False);
      Status := 'Starte Clientthread!';
      Clients[i].Resume;
    finally
      ClientArrayReadWrite.Unlock;
    end
    else
    begin
      sock.Free;
      raise Exception.Create('TClientThreadHost.Connect: ClientArrayReadWrite.LockAll(TimeOut) ... Failed!');
    end;
  end
  else
  begin
    Status := 'Verbindung Fehlgeschlagen!';
    sock.free;
  end;
end;

constructor TClientThreadHost.Create;
begin
  inherited Create(False);

  ClientArrayReadWrite := TReadWriteEvent.Create();

  SetLength(Clients,0);
  EV_Clients := TEvent.Create(nil,false,True,'');

  Server := TSimpleServer.Create;
  Server.OnClientConnect := {$ifdef Lazarus}@{$endif}Server_ClientConnect;

  FreeOnTerminate := False;
end;

procedure TClientThreadHost.Sync_OnClientConnect;
begin
  OnClientConnect(Self,Sync_OnClientConnect_ClientThread,Sync_OnClientConnect_ClientThread.Server);
end;

function TClientThreadHost.ClientThreadLogin(Sender: TThread): Integer;
begin
  if Assigned(OnClientDoLogin) then
    Result := OnClientDoLogin(Self,TClientThread(Sender),TClientThread(Sender).Server)
  else Result := -1;
end;

function TClientThreadHost.ClientThreadSync(Sender: TThread): Integer;
begin
  if Assigned(OnClientDoSync) then
    Result := OnClientDoSync(Self,TClientThread(Sender),TClientThread(Sender).Server)
  else Result := -1;
end;

procedure TClientThreadHost.SetClientEvents(ClientThread: TClientThread);
begin
  ClientThread.OnThreadEnd := {$ifdef Lazarus}@{$endif}ClientThreadEnd;
  ClientThread.OnNoThreadData := {$ifdef Lazarus}@{$endif}ClientThreadNoThreadData;
  ClientThread.OnDoLogin := {$ifdef Lazarus}@{$endif}ClientThreadLogin;
  ClientThread.OnDoSync := {$ifdef Lazarus}@{$endif}ClientThreadSync;
  ClientThread.OnThreadStatus := OnThreadStatus;  //geerbt von TSThread
  ClientThread.OnException := OnException;
end;

procedure TClientThreadHost.SendBuf(var buf; Size, ProcID: Word; DontSendAt: TClientThread; Filter: TFilterFunction; note: string);
var i: integer;
begin
  ClientArrayReadWrite.LockWrite(TimeOut);
  for i := 0 to length(Clients)-1 do
  if (Clients[i] <> nil)and(Clients[i].Socket <> nil)and(Clients[i] <> DontSendAt)and((not Assigned(Filter))or(Filter(Clients[i]))) then
  begin
    try //in sendbuf können fehler auftreten! Die anderen  sockets sollen aber trotzdem auch die daten bekommen!
      Clients[i].Socket.SendBuf(buf,Size,ProcID,note);
    except
      //nix
    end;
  end;
  ClientArrayReadWrite.Unlock;
end;

destructor TClientThreadHost.Destroy;
var i: integer;
begin
  
  for i := 0 to length(Clients)-1 do
  begin
    Clients[i].OnThreadEnd := nil;
    Clients[i].Terminate;
    Clients[i].Socket.Socket.Close;
    Clients[i].OnThreadStatus := nil;  //Fehlermeldung, weil form mit routine nichtmehr vorhanden!
  end;
  inherited Destroy;
end;

destructor TClientThread.Destroy;
begin
  FreeOnTerminate := False;
  Terminate;
  if ReturnValue = Thread_SuspendedBeforeExit then Resume;
  if not (ReturnValue = Thread_Finished) then WaitFor;      //Wenn also destroy nicht aus dem selben Thread aufgerufen!

  Socket.Free;

  if ReturnValue <> Thread_Finished then
  begin
    if Suspended then Resume;
    WaitFor;
  end;
  inherited Destroy;
end;

procedure TClientThreadHost.Disconnect(Index: Integer);
begin
  if (Index < length(Clients)) then
  begin
    Clients[Index].Terminate;
    Clients[Index].Socket.Socket.Close;
  end;
end;

procedure TClientThread.SocketOnEndConnection(Sender: TObject);
begin
  Terminate;
  if ReturnValue = Thread_SuspendedBeforeExit then Resume;
end;

procedure TClientThreadHost.ClientThreadEnd(Sender: TObject);
var i,j: integer;
begin
  (* Hier wird der ClientThread nur aus der Liste gelöscht, Freigeben tut er sich am Ende selber.*)

  i := 0;
  if ClientArrayReadWrite.LockAll(TimeOut) then
  try
    while (i < length(Clients)) do
    begin
      if Clients[i] = TClientThread(Sender) then
      begin
        for j := i to length(Clients)-2 do
          Clients[j] := Clients[j+1];
        SetLength(Clients,length(Clients)-1);

        if Assigned(OnClientDisConnect) then
          OnClientDisConnect(Self,TClientThread(Sender),TClientThread(Sender).Server);

        Break; //gefunden -> Schleife frühzeitig verlassen!
      end
      else inc(i);
    end;
  finally
    ClientArrayReadWrite.Unlock;
  end
  else raise Exception.Create('TClientThreadHost.ClientThreadEnd: ClientArrayReadWrite.LockAll(TimeOut) ... Failed!');
end;

procedure TClientThread.Sync_ThreadEnd;
begin
  OnThreadEnd(self);
end;

procedure TDeMultiplexer.SyncOnEndConnection;
begin
  OnEndConnection(Self);
end;



procedure TClientThread.SelfOnThreadStatusChanged(Sender: TObject);
begin
  if Sender = Self then
  begin
    Socket.OnThreadStatus := OnThreadStatus;
  end else raise Exception.Create('Fataler Fehler: Sender <> Self');
end;

function TDeMultiplexer.DataAvailable(ProcessID: Word): Boolean;
var Size: Word;
begin
  Result := DataAvailable(ProcessID,Size);
end;

function TDeMultiplexer.DataAvailable(ProcessID: Word;
  var Size: Word): Boolean;
begin
  Result := NewLayer.WaitFor(0) = wrSignaled;                     //Wenn Layer da (in einer Millisekunde)
  if Result then
  begin
    Result := (NextLayer.ProzessIndex = ProcessID);               //und die nummer stimmt!
    If Result Then Size := NextLayer.Size;                        //größe mit zurückgeben!
    NewLayer.SetEvent;                                            //Rückgabe, aber nur wenn eins da ist!
  end;
end;

{procedure TClientThreadHost.LockClientList;
begin
  ClientArrayReadWrite.LockAll(TimeOut);
end;

procedure TClientThreadHost.ReleaseClientList;
begin
  ClientArrayReadWrite.Unlock;
end;}

procedure TClientThread.SendBuf(var buf; Size, ProcIndex: Word; note: string);
begin
  Socket.SendBuf(buf,Size,ProcIndex,note);
end;

procedure TClientThread.RecvBuf(var Buf; Size, ProcIndex: Word);
begin
  Socket.RecvBuf(Buf,Size,ProcIndex);
end;

constructor TEventEx.Create(EventAttributes: PSecurityAttributes; AManualReset,
      InitialState: Boolean; const Name: string);
begin
  inherited;
  FThreadsWaiting := 0;
end;

function TEventEx.WaitFor(Timeout: DWORD): TWaitResult;
begin
  inc(FThreadsWaiting);
  Result := inherited WaitFor(Timeout);
  dec(FThreadsWaiting);
end;

procedure TDeMultiplexer.CloseRecvBufLoops;
var loopcount: integer;
begin
  Terminate;
  loopcount := 0;
  while (NewLayer.ThreadsWaiting > 0)and(loopcount < 255) do
  begin
    NewLayer.SetEvent;
    Inc(loopcount);
  end;
  //Die Recv-Buf Schleifen müssen so gestaltet sein, dass sie, direkt nach dem erfolgreichen warten auf NewLayer, gleich terminate überprüfen und dann eine exception raushaun!
  //Sie müssen aber auch vor dem warten terminated überprüfen, damit sie nach diesem aufruf hier nicht wieder anfangen zu warten!
end;

procedure TClientThread.SetOnException(p: TExceptionEvent);
begin
  if Assigned(Socket) then Socket.OnException := p;
  FOnException := p;
end;

function TClientThread.GetOnException: TExceptionEvent;
begin
  Result := FOnException;
end;

procedure TClientThreadHost.Server_ClientConnect(Sender: TObject;
  newSocket: TSocket);
var i: integer;
    TCPSOCK: TTCPSocket;
begin
 {##############################################################################
   Verarbeitet neue Clientverbindungen vom Server(TSimpleServer).
   ->Erstellt eine TTCPSocket und fügt ihn ins Array ein!
  ##############################################################################}

  Status := 'ClientArrayReadWrite.LockAll ... Wait';
  if not ClientArrayReadWrite.LockAll(Timeout) then
    raise Exception.Create('ClientArrayReadWrite.LockAll ... Failed!');

  try
    Status := 'Create Client';
    i := length(Clients);
    Setlength(Clients,i+1);
    TCPSock := TTCPSocket.Create(newSocket);
    Clients[i] := TClientThread.Create(True,TCPSock);
    Clients[i].Server := True;
    SetClientEvents(Clients[i]);
    Sync_OnClientConnect_ClientThread := Clients[i];
  finally
    ClientArrayReadWrite.Unlock;
  end;

  if Assigned(OnClientConnect) then
    OnClientConnect(Self,Clients[i],True);

  Clients[i].Resume;
end;

procedure TClientThreadHost.Execute;
begin
  //nothing!
end;

end.
