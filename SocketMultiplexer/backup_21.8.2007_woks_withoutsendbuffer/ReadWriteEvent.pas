unit ReadWriteEvent;

interface

uses
  SyncObjs;

type
  TLockType = (ltNone, ltAll, ltWrite);
  TReadWriteEvent = class(TObject)
  private
    WriteEvent: TEvent;
    FLockType: TLockType;
    FLockWriteCount: Integer;
  public
    property LockType: TLockType read FLockType;
    property LockWriteCount: integer read FLockWriteCount;
    function LockAll(Timeout: Cardinal): Boolean;
    constructor Create;
    procedure Unlock;
    destructor Destroy; override;
    function LockWrite(Timeout: Cardinal): Boolean;
  end;

implementation

uses SysUtils;

function TReadWriteEvent.LockAll(Timeout: Cardinal): Boolean;
begin
  Result := (WriteEvent.WaitFor(Timeout) = wrSignaled);
  if Result then
    FLockType := ltAll;
end;

constructor TReadWriteEvent.Create;
begin
  inherited;
  FLockType := ltNone;
  FLockWriteCount := 0;
  WriteEvent := TEvent.Create(nil,False,True,'');
end;

procedure TReadWriteEvent.Unlock;
begin
  case LockType of
  ltNone: ;
  ltAll:
    begin
      WriteEvent.SetEvent;
      FLockType := ltNone;
    end;
  ltWrite:
    begin
      dec(FLockWriteCount);
      if LockWriteCount = 0 then
      begin
        WriteEvent.SetEvent;
        FLockType := ltNone;
      end;
    end;
  end;
end;

destructor TReadWriteEvent.Destroy;
begin
  WriteEvent.Free;
  
  inherited;
end;

function TReadWriteEvent.LockWrite(Timeout: Cardinal): Boolean;
begin
  Result := (LockType = ltWrite) or (WriteEvent.WaitFor(Timeout) = wrSignaled);
  if Result then
  begin
    FLockType := ltWrite;
    inc(FLockWriteCount);
  end;
end;

end.
