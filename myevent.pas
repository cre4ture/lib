unit myevent;

interface

uses
  syncobjs;

type
  TMyEventResult = (merSignaled, merTimeOut, merDestroyed, merError); 
  TMyEvent = class
  private
    FEvent: TEvent;
    Destroying: TEvent;
  public
    constructor Create(ManualReset, InitialState: Boolean);
    destructor Destroy; override;
    function WaitFor(timeout: Cardinal): TMyEventResult;
    procedure SetEvent;
    procedure ResetEvent;
  end;

implementation

constructor TMyEvent.Create(ManualReset, InitialState: Boolean);
begin
  inherited Create;
  FEvent := TEvent.Create(nil, ManualReset, InitialState, '');
  Destroying := TEvent.Create(nil, True, False, '');
end;

destructor TMyEvent.Destroy;
var c: Integer;
begin
  Destroying.SetEvent;
  c := 0;
  repeat
    FEvent.SetEvent;
    //Melde solange, bis die Meldung von keinem anderen Thread mehr abgefangen
    //wird.
    if (FEvent.WaitFor(0) = wrSignaled) then
      inc(c);
  until (c > 10);

  FEvent.Free;
  Destroying.Free;
  inherited;
end;

procedure TMyEvent.ResetEvent;
begin
  FEvent.ResetEvent;
end;

procedure TMyEvent.SetEvent;
begin
  FEvent.SetEvent;
end;

function TMyEvent.WaitFor(timeout: Cardinal): TMyEventResult;
begin
  if (Destroying.WaitFor(0) = wrSignaled) then
  begin
    Result := merDestroyed;
  end
  else
  begin
    case FEvent.WaitFor(timeout) of
    wrSignaled:
      begin
        if (Destroying.WaitFor(0) = wrSignaled) then
          Result := merDestroyed
        else
          Result := merSignaled;
      end;
    wrTimeout: Result := merTimeOut;
    else
      Result := merError;
    end;
  end;
end;

end.
 