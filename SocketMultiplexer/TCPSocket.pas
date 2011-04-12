unit TCPSocket;

{
################################################################################

Author: Ulrich Hornung
Date: März 2008

Funktion:

Simple Klassen für TCP-IP Verbindungen

################################################################################
}

interface

uses
  scktcomp, winsock, Sysutils, consts, RTLConsts, windows, StatusThread,
  Classes, SyncObjs;

type
  TSocket = winsock.TSocket;
  TTCPHost = record
    IP: String;
    Port: Word;
  end;
  TSimpleTCPSocket = class
  private
    function GetPeerAddr: SockAddr_In;
  protected
    FSocket: TSocket;
    FConnected: Boolean;
    procedure Error(Source: string);
  public
    function ReceiveBuf(var Buf; Size: Integer): Integer;
    function ReceiveLength: Integer;
    function SendBuf(var Buf; Size: Integer): Integer;
    constructor Create(Socket: TSocket = INVALID_SOCKET);
    procedure Close;
    destructor Destroy; override;

    function Accept: TSocket;
    procedure Bind(Port: Word);
    function Connect(IP: String; Port: Word): boolean;
    function RemoteHost: TTCPHost;
    procedure Listen;
    function Connected: Boolean;
    function SelectWrite(Timeout: LongInt): Boolean;
  end;
  /// Diesen verwenden, nicht den SimpleTCPSocket
  TTCPSocket = class(TSimpleTCPSocket)
  public
    procedure ReceiveBuf(var Buf; Size: Integer);
    procedure SendBuf(var Buf; Size: Integer);
  end;
  TSimpleServerClientConnect = procedure(Sender: TObject; newSocket: TSocket) of object;
  TSimpleServer = class(TSThread)
  private
    NewSocket: TSocket;
    FServerPort: Integer;
    FServerActive: Boolean;
    Started: TEvent;
    procedure DoOnClientConnect;
  public
    ListenSocket: TTCPSocket;  //ListenSocket, Aktiviert wenn Server aktiv (s. StartServer, StopServer)
    OnClientConnect: TSimpleServerClientConnect;
    property ServerPort: Integer read FServerPort;
    property ServerActive: Boolean read FServerActive;
    procedure Execute; override;
    function Start(Port: Integer): Boolean;
    procedure Stop;
    destructor Destroy; override;
    constructor Create;
  end;
  TSimpleServerComponent = class(TComponent)
  private
    FOnClientConnect: TSimpleServerClientConnect;
    procedure SetOnClientConnect(p: TSimpleServerClientConnect);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    SimpleServer: TSimpleServer;
    property OnClientConnect: TSimpleServerClientConnect read FOnClientConnect write SetOnClientConnect;
  end;
  TSimpleClientThread = class;
  TSimpleClientThreadOnSynchronize = procedure(Sender: TSimpleClientThread;
    Data: pointer) of object;
  TSimpleClientThreadOnThreadRun = procedure(Sender: TSimpleClientThread;
    socket: TTCPSocket) of object;
  TSimpleClientThread = class(TSThread)
  private
    SyncData: pointer;
    procedure doCallOnSyncEvent;
  protected
    ClientSocket: TTCPSocket;
  public
    OnThreadRun: TSimpleClientThreadOnThreadRun;
    OnSyncronise: TSimpleClientThreadOnSynchronize;
    property Terminated;
    procedure triggerSyncEvent(data: pointer);
    procedure Execute; override;
    procedure Close;
    destructor Destroy; override;
    constructor Create(socket: TSocket);
  end;

var
  WSAData: TWSAData;

function GetHostByName(Host: String): in_addr;
procedure Register;

implementation

procedure Startup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAStartup($0101, WSAData);
  if ErrorCode <> 0 then
    raise ESocketError.CreateResFmt(@sWindowsSocketError,
      [SysErrorMessage(ErrorCode), ErrorCode, 'WSAStartup']);
end;

function TSimpleTCPSocket.Accept: TSocket;
begin  //Nur für Listen Sockets, blockiert bis anfrage von clients, erstellt neuen Socket dessen Handle mit Result zurückgegeben wird! "Lauscht" aber dann weiter!
  Result := winsock.accept(FSocket,nil,nil);
  if Result = INVALID_SOCKET then Error('Accept');
end;

procedure TSimpleTCPSocket.Bind(Port: Word);
var sockaddr: sockaddr_in;
begin     //vor listen aufzurufen!
  sockaddr.sin_family := AF_INET;
  sockaddr.sin_port := htons(Port);
  sockaddr.sin_addr.S_addr := INADDR_ANY;
  if (winsock.bind(FSocket,sockaddr,sizeof(sockaddr)) = SOCKET_ERROR) then Error('Bind');
end;

procedure TSimpleTCPSocket.Close;
begin
  FConnected := False;
  closesocket(FSocket);
end;

function TSimpleTCPSocket.Connect(IP: String; Port: Word): boolean;
var
  sockaddr: sockaddr_in;
begin
  sockaddr.sin_family := AF_INET;
  sockaddr.sin_port := htons(Port);
  sockaddr.sin_addr.S_addr := inet_addr(PChar(IP));

  if sockaddr.sin_addr.S_addr = -1 then
    sockaddr.sin_addr := getHostByName(IP);

  if (winsock.connect(FSocket,sockaddr,sizeof(sockaddr)) = SOCKET_ERROR)
    then Error('Connect')
  else FConnected := True;
  
  Result := FConnected;
end;

function TSimpleTCPSocket.Connected: Boolean;
begin                    
  Result := FConnected;
  if Result then
    GetPeerAddr; //-> das müsste ja dann gehen!? (wenn nicht is er nichtmehr verbunden!)
end;

constructor TSimpleTCPSocket.Create(Socket: TSocket = INVALID_SOCKET);
begin
  inherited Create;
  Startup;                                                                        
  FSocket := Socket;
  FConnected := False;
  
  if FSocket = INVALID_SOCKET then
  begin
    FSocket := winsock.Socket(AF_INET,SOCK_STREAM,0);
    if FSocket = INVALID_SOCKET then Error('Create');
  end else FConnected := True;
end;

destructor TSimpleTCPSocket.Destroy;
begin
  Close;
  inherited;
end;

procedure TSimpleTCPSocket.Error(Source: string);
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAGetLastError;
  if ErrorCode <> 0 then
  begin
    Close;
    raise ESocketError.Create(SysErrorMessage(ErrorCode) + ' (' + inttostr(ErrorCode) + ') API: ' + Source);
  end;
end;

function TSimpleTCPSocket.GetPeerAddr: SockAddr_In;
var length: integer;
begin
  length := sizeof(Result);
  if getpeername(FSocket,Result,length) = SOCKET_ERROR then Error('GetPeerAddress');
end;

procedure TSimpleTCPSocket.Listen;
var r: Integer;
begin
  r := winsock.listen(FSocket,SOMAXCONN);
  if (r = SOCKET_ERROR) then Error('Listen');
end;

function TSimpleTCPSocket.ReceiveBuf(var Buf; Size: Integer): Integer;
begin
  if not FConnected then raise ESocketError.Create('API: ReceiveBuf, Socket nicht verbunden!');
  Result := recv(FSocket, Buf, Size, 0);
  if (Result = SOCKET_ERROR) then Error('ReceiveBuf');
  if Result = 0 then Close;
end;

function TSimpleTCPSocket.ReceiveLength: Integer;
begin
  Result := 0;
  if FConnected then
    if ioctlsocket(FSocket, FIONREAD, Longint(Result)) = SOCKET_ERROR then
      Error('ReceiveLength');
end;

function TSimpleTCPSocket.RemoteHost: TTCPHost;
var PeerAddr: SockAddr_In;
begin
  PeerAddr := getpeeraddr;
  Result.IP := inet_ntoa(peeraddr.sin_addr);
  Result.Port := ntohs(peeraddr.sin_port);
end;

function TSimpleTCPSocket.SelectWrite(Timeout: LongInt): Boolean;
var fds: TFDSET;
    time: TTimeval;
    count: integer;
begin
  FD_ZERO(fds);
  FD_SET(FSocket, fds);

  time.tv_sec := Timeout div 1000;
  time.tv_usec := Timeout - Time.tv_sec*1000;

  count := Select(0,nil,@fds,nil,@time);
  if count = SOCKET_ERROR then Error('SelectWrite');
  Result := count = 1;
end;

function TSimpleTCPSocket.SendBuf(var Buf; Size: Integer): Integer;
begin
  if not FConnected then raise ESocketError.Create('API: SendBuf, Socket nicht verbunden!');
  Result := send(FSocket, Buf, Size, 0);
  if Result = SOCKET_ERROR then Error('SendBuf');
end;

function GetHostByName(Host: String): in_addr;
var hostaddr: PHostEnt;
begin
  hostaddr := winsock.gethostbyname(Pchar(Host));
  if hostaddr <> nil then
    Result := PInAddr(hostaddr^.h_addr^)^
  else Result.S_addr := -1;
end;

procedure TTCPSocket.ReceiveBuf(var Buf; Size: Integer);
var ReadPos,ReadCount: Integer;
    ReadPointer: Pointer;
begin
  ReadPos := 0;
  repeat
    ReadPointer := Pointer(Integer(@Buf) + ReadPos);
    ReadCount := inherited ReceiveBuf(ReadPointer^,Size-ReadPos);
    ReadPos := ReadPos + ReadCount;
  until {(not FConnected)or}(ReadPos = Size);
end;

procedure TTCPSocket.SendBuf(var Buf; Size: Integer);
var SendPos,SendCount: Integer;
    SendPointer: Pointer;
begin
  SendPos := 0;
  repeat
    SendPointer := Pointer(Integer(@Buf) + SendPos);
    SendCount := inherited SendBuf(SendPointer^,Size-SendPos);
    SendPos := SendPos + SendCount;
  until {(not FConnected)or}(SendPos = Size);
end;

procedure TSimpleServer.Stop;
begin
  FServerActive := False;
  if ListenSocket <> nil then
    ListenSocket.Close;
end;

function TSimpleServer.Start(Port: Integer): Boolean;
begin
  if FServerActive then
    raise Exception.Create('TSimpleServer.Start: Server Already Started!');

  FServerPort := Port;
  FServerActive := True;
  Started.ResetEvent;
  Resume;
  Result := (Started.WaitFor(high(Cardinal)) = wrSignaled)and(FServerActive);
end;

procedure TSimpleServer.Execute;
begin
  inherited;

  ReturnValue := STILL_ACTIVE;
  Status := 'Started...';
  while not Terminated do
  begin
    if FServerActive then
    begin
      ListenSocket := TTCPSocket.Create;
      try
        ListenSocket.Bind(FServerPort);
        ListenSocket.Listen;
        Started.SetEvent;
        while (not Terminated)and(FServerActive) do
        begin
          Status := 'Listening...';
          NewSocket := ListenSocket.Accept;
          if NewSocket <> INVALID_SOCKET then
          begin
            Status := 'Notifying new client';
            if Assigned(OnClientConnect) then
            try
              Synchronize(DoOnClientConnect);
            except
              on E: Exception do
              begin
                Status := 'Exception while notifying new client in ' + Name + ':' + E.Message + ' (' + E.ClassName + ')';
              end;
            end;
          end;
          Sleep(100);
        end;
      except
        on E: Exception do
        begin
          Status := 'Exception while listening in ' + Name + ':' + E.Message + ' (' + E.ClassName + ') -> Server Paused';
          FServerActive := False;
          Started.SetEvent;
        end;
      end;
      ListenSocket.Free;
    end //If FServerActive then
    else
    begin
      Status := 'Paused';
      Suspend;
    end;
  end;
  Status := 'Stopped';
  ReturnValue := 0;
end;

procedure TSimpleServer.DoOnClientConnect;
begin
  OnClientConnect(Self,NewSocket);
end;

destructor TSimpleServer.Destroy;
begin
  Terminate;
  if FServerActive then
  begin
    FServerActive := False;
    ListenSocket.Close;
  end;
  Resume;
  if ReturnValue = STILL_ACTIVE then
    WaitFor;

  Started.Free;
  inherited;
end;

constructor TSimpleServer.Create;
begin
  inherited Create(False);
  FServerActive := False;
  FreeOnTerminate := False;
  Started := TEvent.Create(nil,True,False,'');
end;

procedure Register;
begin
  RegisterComponents('nice things', [TSimpleServerComponent]);
end;

constructor TSimpleServerComponent.Create(AOwner: TComponent);
begin
  inherited;
  SimpleServer := TSimpleServer.Create;
end;

destructor TSimpleServerComponent.Destroy;
begin
  SimpleServer.Free;
  inherited;
end;

procedure TSimpleServerComponent.SetOnClientConnect(
  p: TSimpleServerClientConnect);
begin
  SimpleServer.OnClientConnect := p;
  FOnClientConnect := p;
end;

{ TSimpleClientThread }

procedure TSimpleClientThread.Close;
begin
  Self.Terminate;
  if ClientSocket <> nil then
    ClientSocket.Close;
end;

constructor TSimpleClientThread.Create(socket: TSocket);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  ClientSocket := TTCPSocket.Create(socket);
end;

destructor TSimpleClientThread.Destroy;
begin
  Terminate;
  ClientSocket.Close;
  Resume;
  if ReturnValue = STILL_ACTIVE then
    WaitFor;
  inherited;
end;

procedure TSimpleClientThread.doCallOnSyncEvent;
begin
  OnSyncronise(Self, SyncData);
end;

procedure TSimpleClientThread.Execute;
begin
  inherited;

  ReturnValue := STILL_ACTIVE;
  Status := 'Started...';
  try
    if Assigned(OnThreadRun) then
    begin
      OnThreadRun(Self, ClientSocket);
    end;
  except
    on E: Exception do
    begin
      Status := 'Exception while running custom thread ' + Name + ':' + E.Message + ' (' + E.ClassName + ') -> terminate connection';
    end;
  end;
  Status := 'Closed';
  ReturnValue := 0;
end;

procedure TSimpleClientThread.triggerSyncEvent(data: pointer);
begin
  SyncData := data;
  Synchronize(doCallOnSyncEvent);
end;

end.
