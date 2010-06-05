unit MultiplexList;

interface

uses Classes, SplitSocket, SysUtils, {$ifdef Lazarus}TCPSocket_Lazarus{$else}TCPSocket{$endif}, StatusThread;

type
  TSocketMultiplexListOnOpeningTunnel = procedure(Sender: TObject;
    SMulti: TSocketMultiplex; Tunnel: TSplitSocket;
    var Accept: Boolean) of object;
  TSocketMultiplexListOnOpenedTunnel = procedure(Sender: TObject;
    SMulti: TSocketMultiplex; Tunnel: TSplitSocket) of object;
  TSocketMultiplexListOnRemoveSocket = procedure(Sender: TObject;
    SMulti: TSocketMultiplex) of object;
  TSocketMultiplexList = class
  private
    FList: TList;
    function GetSocket(i: Integer): TSocketMultiplex;
    procedure SocketMultiplexOnThreadExit(Sender: TObject);
    procedure SocketMultiplexOnOpeningTunnel_ReadThread(Sender: TObject;
      Tunnel: TSplitSocket; var Accept: Boolean);
    procedure SocketMultiplexOnOpenedTunnel(Sender: TObject;
      Tunnel: TSplitSocket);
    function SocketMultiplexOnHandleException(Sender: TThread; E: Exception;
      msg: String): Boolean;
    procedure RemoveSocketMultiplex(SMulti: TSocketMultiplex);
  public
    OnSocketOpeningTunnel_ReadThread: TSocketMultiplexListOnOpeningTunnel;
    OnSocketOpenedTunnel: TSocketMultiplexListOnOpenedTunnel;
    OnRemoveSocket: TSocketMultiplexListOnRemoveSocket;
    OnNewSocket: TSocketMultiplexListOnRemoveSocket;
    OnHandleException: THandleExceptionEvent;
    constructor Create;
    destructor Destroy; override;
    procedure AddSocketMultiplex(SMulti: TSocketMultiplex); overload;
    property Sockets[i: integer]: TSocketMultiplex read GetSocket; default;
    function Count: Integer;
    function AddSocketMultiplex(Socket: TSocket; Master: Boolean): TSocketMultiplex;
      overload;
  end;
  TSocketMultiplexListComponent = class(TComponent)
  private
    procedure SetOnSocketOpeningTunnel(
      P: TSocketMultiplexListOnOpeningTunnel);
    function GetOnSocketOpeningTunnel: TSocketMultiplexListOnOpeningTunnel;
    procedure SetOnSocketOpenedTunnel(
      P: TSocketMultiplexListOnOpenedTunnel);
    function GetOnSocketOpenedTunnel: TSocketMultiplexListOnOpenedTunnel;
    procedure SetOnRemoveSocket(P: TSocketMultiplexListOnRemoveSocket);
    function GetOnRemoveSocket: TSocketMultiplexListOnRemoveSocket;
    procedure SetOnNewSocket(P: TSocketMultiplexListOnRemoveSocket);
    function GetOnNewSocket: TSocketMultiplexListOnRemoveSocket;
    procedure SetOnException(P: THandleExceptionEvent);
    function GetOnException: THandleExceptionEvent;
  public
    SocketList: TSocketMultiplexList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnNewSocket: TSocketMultiplexListOnRemoveSocket
      read GetOnNewSocket write SetOnNewSocket;
    property OnSocketOpeningTunnel: TSocketMultiplexListOnOpeningTunnel
          read GetOnSocketOpeningTunnel write SetOnSocketOpeningTunnel;
    property OnSocketOpenedTunnel: TSocketMultiplexListOnOpenedTunnel
          read GetOnSocketOpenedTunnel write SetOnSocketOpenedTunnel;
    property OnException: THandleExceptionEvent
          read GetOnException write SetOnException;
    property OnRemoveSocket: TSocketMultiplexListOnRemoveSocket
          read GetOnRemoveSocket write SetOnRemoveSocket;
  end;

procedure Register;

implementation

uses Math;

procedure Register;
begin
  RegisterComponents('nice things', [TSocketMultiplexListComponent]);
end;

procedure TSocketMultiplexList.AddSocketMultiplex(
  SMulti: TSocketMultiplex);
begin
  if Assigned(OnNewSocket) then
    OnNewSocket(Self,SMulti);

  FList.Add(SMulti);
  {$ifdef Lazarus}
  SMulti.OnNoConnection := @SocketMultiplexOnNoConnection;
  SMulti.OnOpeningTunnel := @SocketMultiplexOnOpeningTunnel;
  SMulti.OnOpenedTunnel := @SocketMultiplexOnOpenedTunnel;
  SMulti.OnException := @SocketMultiplexOnException;
  {$else}
  SMulti.OnThreadExit := SocketMultiplexOnThreadExit;
  SMulti.OnOpeningTunnel_ReadThread := SocketMultiplexOnOpeningTunnel_ReadThread;
  SMulti.OnOpenedTunnel := SocketMultiplexOnOpenedTunnel;
  SMulti.OnHandleException := SocketMultiplexOnHandleException;
  {$endif}

  SMulti.StartWorking;  //Empfang starten, erst nachdem der socket in der liste!
end;

function TSocketMultiplexList.AddSocketMultiplex(
  Socket: TSocket; Master: Boolean): TSocketMultiplex;
begin
  Result := TSocketMultiplex.Create(TTCPSocket.Create(Socket), Master);
  AddSocketMultiplex(Result);
end;

function TSocketMultiplexList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TSocketMultiplexList.Create;
begin
  OnSocketOpeningTunnel_ReadThread := nil;
  OnSocketOpenedTunnel := nil;
  OnRemoveSocket := nil;
  OnNewSocket := nil;
  OnHandleException := nil;

  inherited;
  FList := TList.Create;
end;

destructor TSocketMultiplexList.Destroy;
begin
  while Count > 0 do
    Sockets[0].Free;

  FList.Free;
  inherited;
end;

function TSocketMultiplexList.GetSocket(i: Integer): TSocketMultiplex;
begin
  if (i >= 0)and(i < FList.Count) then
  begin
    Result := TSocketMultiplex(FList[i]);
  end else raise Exception.Create('TSocketMultiplexList.GetSocket: Fehler bei Bereichsüberprüfung!');
end;

procedure TSocketMultiplexList.RemoveSocketMultiplex(
  SMulti: TSocketMultiplex);
begin
  SMulti.OnThreadExit := nil;
  SMulti.OnOpeningTunnel_ReadThread := nil;
  SMulti.OnOpenedTunnel := nil;
  SMulti.OnHandleException := nil;
  FList.Remove(SMulti);

  if Assigned(OnRemoveSocket) then
    OnRemoveSocket(Self,SMulti);
end;

procedure TSocketMultiplexList.SocketMultiplexOnThreadExit(
  Sender: TObject);
begin
  RemoveSocketMultiplex(TSocketMultiplex(Sender));
end;

procedure TSocketMultiplexList.SocketMultiplexOnOpeningTunnel_ReadThread(
  Sender: TObject; Tunnel: TSplitSocket; var Accept: Boolean);
begin
  if Assigned(OnSocketOpeningTunnel_ReadThread) then
    OnSocketOpeningTunnel_ReadThread(Self,TSocketMultiplex(Sender),Tunnel,Accept);
end;

procedure TSocketMultiplexList.SocketMultiplexOnOpenedTunnel(
  Sender: TObject; Tunnel: TSplitSocket);
begin
  if Assigned(OnSocketOpenedTunnel) then
    OnSocketOpenedTunnel(Self,TSocketMultiplex(Sender),Tunnel);
end;

function TSocketMultiplexList.SocketMultiplexOnHandleException(Sender: TThread;
  E: Exception; msg: String): Boolean;
begin
  if Assigned(OnHandleException) then
    Result := OnHandleException(Sender,E,msg)
  else Result := False;
end;

constructor TSocketMultiplexListComponent.Create(AOwner: TComponent);
begin
  inherited;
  SocketList := TSocketMultiplexList.Create;
end;

destructor TSocketMultiplexListComponent.Destroy;
begin
  SocketList.Free;
  inherited;
end;

function TSocketMultiplexListComponent.GetOnSocketOpenedTunnel: TSocketMultiplexListOnOpenedTunnel;
begin
  Result := SocketList.OnSocketOpenedTunnel;
end;

function TSocketMultiplexListComponent.GetOnRemoveSocket: TSocketMultiplexListOnRemoveSocket;
begin
  Result := SocketList.OnRemoveSocket;
end;

function TSocketMultiplexListComponent.GetOnSocketOpeningTunnel: TSocketMultiplexListOnOpeningTunnel;
begin
  Result := SocketList.OnSocketOpeningTunnel_ReadThread;
end;

procedure TSocketMultiplexListComponent.SetOnRemoveSocket(
  P: TSocketMultiplexListOnRemoveSocket);
begin
  SocketList.OnRemoveSocket := P;
end;

procedure TSocketMultiplexListComponent.SetOnSocketOpenedTunnel(
  P: TSocketMultiplexListOnOpenedTunnel);
begin
  SocketList.OnSocketOpenedTunnel := P;
end;

procedure TSocketMultiplexListComponent.SetOnSocketOpeningTunnel(
  P: TSocketMultiplexListOnOpeningTunnel);
begin
  SocketList.OnSocketOpeningTunnel_ReadThread := P;
end;

procedure TSocketMultiplexListComponent.SetOnNewSocket(
  P: TSocketMultiplexListOnRemoveSocket);
begin
  SocketList.OnNewSocket := P;
end;

function TSocketMultiplexListComponent.GetOnNewSocket: TSocketMultiplexListOnRemoveSocket;
begin
  Result := SocketList.OnNewSocket;
end;

procedure TSocketMultiplexListComponent.SetOnException(
  P: THandleExceptionEvent);
begin
  SocketList.OnHandleException := P;
end;

function TSocketMultiplexListComponent.GetOnException: THandleExceptionEvent;
begin
  Result := SocketList.OnHandleException;
end;

end.
