unit MergeSocket;

interface

uses Classes, SyncObjs, SplitSocket, SysUtils, windows;

type
  TMergeSocketNewPacket = procedure(Sender: TObject; Socket: TSplitSocket) of object;
  TMergeSocket = class(TObject)
  private
    FSocketList: TList;
    ASocketPos: Integer;
    PacketsAvailable: TEvent;
    procedure SocketOnNewPacket_ReadThread(Sender: TObject);
    procedure SocketOnDestroy(Sender: TObject);
    function GetSocket(nr: integer): TSplitSocket;
  public
    OnNewPacket_ReadThread: TMergeSocketNewPacket;
    OnNewSocket: TMergeSocketNewPacket;
    OnRemoveSocket: TMergeSocketNewPacket;
    property Sockets[nr: Integer]: TSplitSocket read GetSocket;
    procedure AddSocket(Socket: TSplitSocket);
    procedure RemoveSocket(Socket: TSplitSocket);
    constructor Create;
    destructor Destroy; override;
    procedure RecvPacket(var Data: pointer; var Size: Word; var Socket: TSplitSocket;const TimeOut: Cardinal = High(Cardinal));
    function GetPacket(var Data: pointer; var Size: Word; var Socket: TSplitSocket): Boolean;
    procedure SendPacket(var Data; const Size: Word; SkipSocket: TSplitSocket = nil);
    function Count: Integer;
  end;
  TMergeSocketComponent = class(TComponent)
  private
    FOnNewPacket_ReadThread: TMergeSocketNewPacket;
    FOnNewSocket: TMergeSocketNewPacket;
    FOnRemoveSocket: TMergeSocketNewPacket;
    procedure SetFOnNewPacket_ReadThread(proc: TMergeSocketNewPacket);
    procedure SetFOnNewSocket(proc: TMergeSocketNewPacket);
    procedure SetFOnRemoveSocket(proc: TMergeSocketNewPacket);
  public
    MergeSocket: TMergeSocket;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnNewPacket_ReadThread: TMergeSocketNewPacket read FOnNewPacket_ReadThread
                                                           write SetFOnNewPacket_ReadThread;
    property OnNewSocket: TMergeSocketNewPacket read FOnNewSocket
                                                write SetFOnNewSocket;
    property OnRemoveSocket: TMergeSocketNewPacket read FOnRemoveSocket
                                                write SetFOnRemoveSocket;
  end;

procedure Register;

implementation

procedure TMergeSocket.AddSocket(Socket: TSplitSocket);
begin
  If Assigned(OnNewSocket) then
    OnNewSocket(Self,Socket);


    FSocketList.Add(Socket);
    Socket.OnDestroy := {$ifdef Lazarus}@{$endif}SocketOnDestroy;
    Socket.OnNewPacket_ReadThread := {$ifdef Lazarus}@{$endif}SocketOnNewPacket_ReadThread;

  //Da vor dem adden schon packete angekommen sein könnten:
  SocketOnNewPacket_ReadThread(Socket);
end;

function TMergeSocket.Count: Integer;
begin
  Result := FSocketList.Count;
end;

constructor TMergeSocket.Create;
begin
  inherited;
  FSocketList := TList.Create;
  ASocketPos := 0;
  PacketsAvailable := TEvent.Create(nil,True,False,'');

  OnNewSocket := nil;
  OnNewPacket_ReadThread := nil;
  OnNewPacket_ReadThread := nil;
end;

destructor TMergeSocket.Destroy;
begin
  while (Count > 0) do
  begin
    RemoveSocket(Sockets[0]);
  end;

  FSocketList.Free;
  inherited;
end;

{###############################################################################
 TMergeSocket.GetPacket

 Liest, falls vorhanden, das nächste Packet. Wenn kein Packet mehr im "Buffer"
 ist, gibt die Funktion False zurück!

 Diese Funktion Blockiert nicht!

 Hinweis zur Technik: Ich starte NICHT jedesmal wieder am Anfang der Liste,
 damit bei viel Datenverkehr auf der "Leitung0" auch die "Leitungen1,2,3..."
 Ausgelesen werden! Deswegen der Aufwand mit ASocketPos.
###############################################################################}
function TMergeSocket.GetPacket(var Data: pointer; var Size: Word; var Socket: TSplitSocket): Boolean;
var stop: Integer;
begin
  Result := False;
  stop := ASocketPos;

  if ASocketPos >= Count then
    ASocketPos := 0; //Könnte vorkommen, wenn Socket schließen,
                     // und aus der Liste gelöscht werden!

  if Count > 0 then  //Wenn es garkeine Sockets in der Liste gibt,
                              // würde es einen Crash geben!
  repeat
    Socket := Sockets[ASocketPos];
    Result := Socket.GetPacket(Data,Size);

    inc(ASocketPos);
    if ASocketPos >= Count then
      ASocketPos := 0;
  until (stop = ASocketPos) or Result;
end;

function TMergeSocket.GetSocket(nr: integer): TSplitSocket;
begin
  if (nr >= 0)and(nr < Count) then
    Result := TSplitSocket(FSocketList[nr])
  else raise Exception.Create('TMergeSocket.GetSocket: Fehler bei Bereichsüberprüfung');
end;


{###############################################################################
 TMergeSocket.RecvPacket

 Liest das nächste Packet. Wenn nötig wird solange blockiert bis ein Packet
 vorhanden ist!
 
 Ist quasi der Blockierende Socketaufruf.
###############################################################################}
procedure TMergeSocket.RecvPacket(var Data: pointer; var Size: Word;
  var Socket: TSplitSocket; const TimeOut: Cardinal = High(Cardinal));
var success: Boolean;
begin
  repeat
    case PacketsAvailable.WaitFor(TimeOut) of
    wrSignaled:
      begin
        success := GetPacket(Data, Size, Socket);
        if not success then
          PacketsAvailable.ResetEvent;
                    //alle sockets durch und kein Packet gelesen!!
                    // -> keine da, nochmal bis timeout warten!

      end;
    wrTimeout: raise ETimeOut.Create('TMergeSocket.RecvPacket: TimeOut while waiting for PacketsAvailable');
    else raise Exception.Create('TMergeSocket.RecvPacket: Error while waiting for PacketsAvailable');
    end;
  until success; //bis entweder ein timeout auftritt,
                 // oder erfolgreich gelesen wurde!
end;

procedure TMergeSocket.RemoveSocket(Socket: TSplitSocket);
begin
  Socket.OnDestroy := nil;
  Socket.OnNewPacket_ReadThread := nil;
  FSocketList.Remove(Socket);

  if Assigned(OnRemoveSocket) then
    OnRemoveSocket(Self,Socket);
end;

procedure TMergeSocket.SendPacket(var Data; const Size: Word; SkipSocket: TSplitSocket = nil);
var i: integer;
begin
  for i := 0 to Count-1 do
  begin
    if Sockets[i] <> SkipSocket then
    with Sockets[i] do
    begin
      try
        SendPacket(Data,Size);
      except

      end;
    end;
  end;
end;

procedure TMergeSocket.SocketOnDestroy(Sender: TObject);
begin
  RemoveSocket(TSplitSocket(Sender));
end;

procedure TMergeSocket.SocketOnNewPacket_ReadThread(Sender: TObject);
begin
  PacketsAvailable.SetEvent; 
  if Assigned(OnNewPacket_ReadThread) then
    OnNewPacket_ReadThread(Self,TSplitSocket(Sender));
end;

procedure Register;
begin
  RegisterComponents('nice things', [TMergeSocketComponent]);
end;

constructor TMergeSocketComponent.Create(AOwner: TComponent);
begin
  inherited;
  MergeSocket := TMergeSocket.Create;
end;

destructor TMergeSocketComponent.Destroy;
begin
  MergeSocket.Free;
  inherited;
end;

procedure TMergeSocketComponent.SetFOnNewPacket_ReadThread(
  proc: TMergeSocketNewPacket);
begin
  MergeSocket.OnNewPacket_ReadThread := proc;
  FOnNewPacket_ReadThread := proc;
end;

procedure TMergeSocketComponent.SetFOnNewSocket(
  proc: TMergeSocketNewPacket);
begin
  FOnNewSocket := proc;
  MergeSocket.OnNewSocket := proc;
end;

procedure TMergeSocketComponent.SetFOnRemoveSocket(
  proc: TMergeSocketNewPacket);
begin
  FOnRemoveSocket := proc;
  MergeSocket.OnRemoveSocket := proc;
end;

end.
