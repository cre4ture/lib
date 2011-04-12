unit SDBFile;

{

Autor: Ulrich Hornung
Datum: 14.7.2007


1. Make a construtor Create, where you set the FHeaderSize and the FItemSize

}

interface

uses
  Classes, SysUtils;

type
  ESimpleDBStreamNROutOfRange = Exception;
  TSimpleDBStream = class
  private
    FStream: TStream;
  protected
    FHeaderSize: Integer;
    FItemSize: Integer;
    procedure GetItem(const nr: Integer; var buf);
    procedure SetItem(const nr: Integer; var buf);
    function GetHeader(var buf): Boolean;
    procedure SetHeader(var buf);
    function GetCount: Integer; virtual;
    procedure SetCount(newCount: Integer); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount write SetCount;
  end;

  TSimpleDBFile = class(TSimpleDBStream)
  private
    FFilename: string;
  public
    constructor Create(aFilename: string);
    destructor Destroy; override;
  end;

  TSimpleDBCachedFile = class(TSimpleDBFile)
  protected
    FPtrList: TList;
    //-------- Umwandlung von buf -> ptr START ---------------------------------
    function  NewItemPtr: pointer; virtual; abstract;
    procedure ItemToPtr(var Buf; const p: pointer); virtual; abstract;
    procedure PtrToItem(const p: pointer; var Buf); virtual; abstract;
    procedure DisposeItemPtr(const p: pointer); virtual; abstract;
    //-------- Umwandlung von buf -> ptr ENDE ----------------------------------
    procedure ClearList;
    procedure LoadList;
    function GetCachedItem(const nr: Integer): pointer;
    procedure SetItem(const nr: Integer; const ptr: pointer);
    procedure SetCount(newCount: Integer); override;
    function GetCount: Integer; override;
  public
    constructor Create(aFilename: string; doLoad: Boolean = True);
    destructor Destroy; override;
  end;

implementation

uses Math;

constructor TSimpleDBStream.Create;
begin
  inherited;
end;

destructor TSimpleDBStream.Destroy;
begin
  inherited;
end;

constructor TSimpleDBFile.Create(aFilename: string);
begin
  inherited Create();

  FFilename := aFilename;
  if FileExists(aFilename) then
    FStream := TFileStream.Create(FFilename,fmOpenReadWrite or fmShareDenyWrite)
  else
    FStream := TFileStream.Create(FFilename,fmCreate or fmShareDenyWrite);
end;

destructor TSimpleDBFile.Destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TSimpleDBStream.GetItem(const nr: Integer;
  var buf);
begin
  if (nr < Count)and(nr >= 0) then
  begin
    FStream.Position := FHeaderSize + (FItemSize * nr);
    FStream.ReadBuffer(buf,FItemSize);
  end else raise ESimpleDBStreamNROutOfRange.Create
    ('TSimpleDBStream.GetItem: nr(' + IntToStr(nr) + ') out of range!');
end;

procedure TSimpleDBStream.SetItem(const nr: Integer;
  var buf);
begin
  if (nr < Count+1)and(nr >= 0) then
  begin
    FStream.Position := FHeaderSize + (FItemSize * nr);
    FStream.WriteBuffer(buf,FItemSize);
  end else raise ESimpleDBStreamNROutOfRange.Create
    ('TSimpleDBStream.SetItem: nr(' + IntToStr(nr) + ') out of range!');
end;

function TSimpleDBStream.GetHeader(var buf): Boolean;
begin
  FStream.Position := 0;
  Result := (FStream.Read(buf,FHeaderSize) = FHeaderSize);
end;

procedure TSimpleDBStream.SetHeader(var buf);
begin
  FStream.Position := 0;
  FStream.WriteBuffer(buf,FHeaderSize);
end;

function TSimpleDBStream.GetCount: Integer;
begin
  Result := (FStream.Size - FHeaderSize) div FItemSize;
end;

procedure TSimpleDBStream.SetCount(newCount: Integer);
begin
  FStream.Size := FHeaderSize + (FItemSize * newCount);
end;

procedure TSimpleDBCachedFile.ClearList;
var i: Integer;
begin
  for i := 0 to FPtrList.Count-1 do
  begin
    DisposeItemPtr(FPtrList[i]);
  end;
  FPtrList.Clear;
end;

constructor TSimpleDBCachedFile.Create(aFilename: string;
  doLoad: Boolean = True);
begin
  inherited Create(aFilename);
  FPtrList := TList.Create;

  if doLoad then
    LoadList;
end;

destructor TSimpleDBCachedFile.Destroy;
begin
  if FPtrList <> nil then
    ClearList;
    
  FPtrList.Free;
  inherited;
end;

function TSimpleDBCachedFile.GetCachedItem(const nr: Integer): pointer;
begin
  result := FPtrList[nr];
end;

function TSimpleDBCachedFile.GetCount: Integer;
begin
  //Hier wird die geerbte Methode nicht aufgerufen,
  //da diese direkt auf die Datei zugreift.
  //Jedoch haben wir "Count" auch schon in der Liste gecached!

  Result := FPtrList.Count;
end;

procedure TSimpleDBCachedFile.LoadList;
var i: Integer;
    buf, p: pointer;
begin
  FPtrList.Count := inherited GetCount;
  GetMem(buf,FItemSize);
  for i := 0 to FPtrList.Count-1 do
  begin
    p := NewItemPtr;
    GetItem(i,buf^);
    ItemToPtr(buf^,p);
    FPtrList[i] := p;
  end;
  FreeMem(buf);
end;

procedure TSimpleDBCachedFile.SetCount(newCount: Integer);
var i, oldCount: Integer;
begin
  oldCount := FPtrList.Count;
  if (newCount > oldCount) then
  begin
    // organise capacity better than default behaviour:
    if FPtrList.Capacity < newCount then
      FPtrList.Capacity := newCount + 200;

    FPtrList.Count := newCount;
    for i := oldCount to newCount-1 do
      FPtrList[i] := NewItemPtr;
  end;

  inherited;
  
  if (newCount < oldCount) then
  begin
    for i := oldCount-1 downto newCount do
      DisposeItemPtr(FPtrList[i]);
    FPtrList.Count := newCount;
  end;
end;

procedure TSimpleDBCachedFile.SetItem(const nr: Integer;
  const ptr: pointer);
var buf: pointer;
begin
  GetMem(buf,FItemSize);
  PtrToItem(ptr,buf^);
  inherited SetItem(nr, buf^);
  ItemToPtr(buf^,FPtrList[nr]);
  FreeMem(buf);
end;

end.
