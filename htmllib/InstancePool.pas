unit InstancePool;

interface

uses classes, SysUtils;

type
  List = TList;
  TInstanceMemoryPool = class
  private
    FInstanceSize: Integer;
    FList: Pointer;
    FCapacity, FCount: integer;
    procedure SetCapacity(NewCapacity: Integer);
  public
    constructor Create(instanceSize: Integer);
    destructor Destroy; override;
  end;

implementation

{ TInstanceMemoryPool }

constructor TInstanceMemoryPool.Create(instanceSize: Integer);
begin
  inherited Create();
  FInstanceSize := instanceSize;
end;

destructor TInstanceMemoryPool.Destroy;
begin
  SetCapacity(0);
  inherited;
end;

procedure TInstanceMemoryPool.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    raise Exception.Create('TInstanceMemoryPool.SetCapacity():');
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;

end.
