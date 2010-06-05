unit TCPSocket_Lazarus;

interface

uses
  Sysutils, RTLConsts, StatusThread,
  Classes, SyncObjs, sockets;
  
  {$M+}

const
  INVALID_SOCKET = -1;
  SOCKET_ERROR = -1;

type
  TSocket = Longint;
  TTCPHost = record
    IP: String;
    Port: Word;
  end;
  TSimpleTCPSocket = class
  private
    function GetPeerName: AnsiString;
  protected
    FSocket: TSocket;
    FConnected: Boolean;
    procedure Error(Source: string);
  public
    function ReceiveBuf(var Buf; Size: Word): Integer;
    {function ReceiveLength: Integer;}
    function SendBuf(var Buf; Size: Word): Integer;
    constructor Create(Socket: TSocket = INVALID_SOCKET);
    procedure Close;
    destructor Destroy; override;

    function Accept: TSocket;
    procedure Bind(Port: Word);
    function Connect(IP: String; Port: Word): boolean;
    function RemoteHost: TTCPHost;
    procedure Listen;
    function Connected: Boolean;
    function SelectWrite(Timeout: Word): Boolean;
  end;
  TTCPSocket = class(TSimpleTCPSocket)
  public
    procedure ReceiveBuf(var Buf; Size: Word);
    procedure SendBuf(var Buf; Size: Word);
  end;
  TSimpleServerClientConnect = procedure(Sender: TObject; newSocket: TSocket) of object;
  TSimpleServer = class(TSThread)
  private
    NewSocket: TSocket;
    FServerPort: Integer;
    FServerActive: Boolean;
    Started: TEvent;
    procedure DoOnClientConnect; virtual;
  public
    ListenSocket: TTCPSocket;
    OnClientConnect: TSimpleServerClientConnect;
    property ServerPort: Integer read FServerPort;
    procedure Execute; override;
    function Start(Port: Integer): Boolean;
    procedure Stop;
    destructor Destroy; override;
    constructor Create;   //ListenSocket, Aktiviert wenn Server aktiv (s. StartServer, StopServer)
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

{function GetHostByName(Host: String): in_addr;}
procedure Register;

implementation

procedure Startup;
begin
  //Nothing!!!
end;

function TSimpleTCPSocket.GetPeerName: AnsiString;
var l: Longint;
begin
  Setlength(Result,255);
  l := 255;
  if Sockets.GetPeerName(FSocket,PChar(Result)^,l) <> 0 then Error('GetPeerName');
end;

function TSimpleTCPSocket.Accept: TSocket;
var SAddr: TInetSockAddr;
begin
  FillChar(SAddr,Sizeof(SAddr),0);
  Result := sockets.accept(FSocket,SAddr,Sizeof(SAddr));
  if Result = INVALID_SOCKET then Error('Accept');
end;

procedure TSimpleTCPSocket.Bind(Port: Word);
var SAddr: TInetSockAddr;
begin     //vor listen aufzurufen!
  SAddr.sin_family:=AF_INET;
  SAddr.sin_port:=htons(Port);
  SAddr.sin_addr.s_addr:=INADDR_ANY;
  if not sockets.Bind(FSocket,SAddr,sizeof(SAddr)) then
    Error('Bind');
end;

procedure TSimpleTCPSocket.Close;
begin
  FConnected := False;
  closesocket(FSocket);
end;

function TSimpleTCPSocket.Connect(IP: String; Port: Word): boolean;
var
  sockaddr: TInetSockAddr;
begin
  sockaddr.sin_family := AF_INET;
  sockaddr.sin_port := htons(Port);
  sockaddr.sin_addr := StrToHostAddr(IP);

  if sockaddr.sin_addr.S_addr = -1 then
    sockaddr.sin_addr := Sockets.StrToHostAddr(IP);

  if not(sockets.connect(FSocket,sockaddr,sizeof(sockaddr))) then Error('Connect')
  else FConnected := True;
  
  Result := FConnected;
end;

function TSimpleTCPSocket.Connected: Boolean;
begin                    
  Result := FConnected;
  if Result then
    GetPeerName; //-> das müsste ja dann gehen!? (wenn nicht is er nichtmehr verbunden!)
end;

constructor TSimpleTCPSocket.Create(Socket: TSocket = INVALID_SOCKET);
begin
  inherited Create;
  Startup;                                                                        
  FSocket := Socket;
  FConnected := False;
  
  if FSocket = INVALID_SOCKET then
  begin
    FSocket := Sockets.Socket(AF_INET,SOCK_STREAM,0);
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
  ErrorCode := Sockets.SocketError;
  if ErrorCode <> 0 then
  begin
    Close;
    raise Exception.Create((SysErrorMessage(ErrorCode) + ' (' + inttostr(ErrorCode) + ') API: ' + Source));
  end;
end;

procedure TSimpleTCPSocket.Listen;
var r: Integer;
begin
  if not sockets.listen(FSocket,1) then Error('Listen');
end;

function TSimpleTCPSocket.ReceiveBuf(var Buf; Size: Word): Integer;
begin
  if not FConnected then raise Exception.Create('API: ReceiveBuf, Socket nicht verbunden!');
  Result := sockets.recv(FSocket, Buf, Size, 0);
  if (Result = SOCKET_ERROR) then Error('ReceiveBuf');
  if Result = 0 then Close;
end;

{function TSimpleTCPSocket.ReceiveLength: Integer;
begin
  Result := 0;
  if FConnected then
    if ioctlsocket(FSocket, FIONREAD, Longint(Result)) = SOCKET_ERROR then Error('ReceiveLength');
end; }

function TSimpleTCPSocket.RemoteHost: TTCPHost;
begin
  Result.IP := GetPeerName;
  Result.Port := 0;
end;

function TSimpleTCPSocket.SelectWrite(Timeout: Word): Boolean;
begin
  Result := True; //Geht mit lazarus nich, also: einfach immer JA!
  //Weis eh nicht obs das ganze hier gebracht hat!!
end;

function TSimpleTCPSocket.SendBuf(var Buf; Size: Word): Integer;
begin
  if not FConnected then raise Exception.Create('API: SendBuf, Socket nicht verbunden!');
  Result := send(FSocket, Buf, Size, 0);
  if Result = SOCKET_ERROR then Error('SendBuf');
end;

{function GetHostByName(Host: String): in_addr;
var hostaddr: PHostEnt;
begin
  hostaddr := winsock.gethostbyname(Pchar(Host));
  if hostaddr <> nil then
    Result := PInAddr(hostaddr^.h_addr^)^
  else Result.S_addr := -1;
end;}

procedure TTCPSocket.ReceiveBuf(var Buf; Size: Word);
var ReadPos,ReadCount: Word;
    ReadPointer: Pointer;
begin
  ReadPos := 0;
  repeat
    ReadPointer := Pointer(Cardinal(@Buf) + ReadPos);
    ReadCount := inherited ReceiveBuf(ReadPointer^,Size-ReadPos);
    ReadPos := ReadPos + ReadCount;
  until (not FConnected)or(ReadPos = Size);
end;

procedure TTCPSocket.SendBuf(var Buf; Size: Word);
var SendPos,SendCount: Word;
    SendPointer: Pointer;
begin
  SendPos := 0;
  repeat
    SendPointer := Pointer(Cardinal(@Buf) + SendPos);
    SendCount := inherited SendBuf(SendPointer^,Size-SendPos);
    SendPos := SendPos + SendCount;
  until (not FConnected)or(SendPos = Size);
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
              Synchronize(@DoOnClientConnect);
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

end.
