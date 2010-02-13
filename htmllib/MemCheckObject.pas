unit MemCheckObject;

interface

uses
  Classes, Dialogs;

type
  TMemCheckObj = class
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  MemCheckObjects: TList;

implementation

constructor TMemCheckObj.Create;
begin
  inherited;
  MemCheckObjects.Add(Self);
end;

destructor TMemCheckObj.Destroy;
begin
  MemCheckObjects.Remove(Self);
  inherited;
end;

function GetMemCheckObjects: String;
var i: integer;
begin
  Result := '';
  for i := 0 to MemCheckObjects.Count-1 do
    Result := Result + TMemCheckObj(MemCheckObjects[i]).ClassName + ', ';
end;
var s: string;
initialization
  MemCheckObjects := TList.Create;
finalization
  s := GetMemCheckObjects;

  if s <> '' then
    ShowMessage('Folgende Objecte wurden nicht freigegeben: ' + s);

  MemCheckObjects.Free;

end.
 