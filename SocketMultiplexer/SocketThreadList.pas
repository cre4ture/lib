unit SocketThreadList;

interface

{
################################################################################

Author: Ulrich Hornung
Date: März 2008

Funktion:

k.a. was ich mir da mal dabei gedacht hab ...

################################################################################
}

uses
  Classes, TCPSocket;

type
  TSocketThread = class(TThread)
  protected
    Socket: TTCPSocket;
  public
    constructor Create(CreateSuspended: Boolean; aSocket: TTCPSocket);
  end;
  TSocketThreadList = class
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(SThread: TSocketThread): integer;
  end;

implementation

constructor TSocketThread.Create(CreateSuspended: Boolean; aSocket: TTCPSocket);
begin
  Socket := aSocket;
  inherited Create(CreateSuspended);
end;

function TSocketThreadList.Add(SThread: TSocketThread): integer;
begin
  Result := FList.Add(SThread);
end;

constructor TSocketThreadList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TSocketThreadList.Destroy;
begin
  FList.Free;
  inherited;
end;

end.
