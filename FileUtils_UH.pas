unit FileUtils_UH;

interface

uses
  SysUtils, Classes;

procedure createDirectory_ALL(dir: string);
function GetDirFileList(dir: String; list: TStrings;
  mask: string = '*.*'): boolean;

implementation

procedure createDirectory_ALL(dir: string);
begin
  dir := ExtractFileDir(dir);
  if not DirectoryExists(dir) then
  begin
    createDirectory_ALL(dir); //recursion!!
    CreateDir(dir);
  end;
end;

function GetDirFileList(dir: String; list: TStrings;
  mask: string = '*.*'): boolean;
var f: TSearchRec;
    ende: Boolean;
begin
  list.Clear;
  Result := FindFirst(dir + mask, faAnyFile, f) = 0;
  ende := not Result;
  while not ende do
  begin
    list.Add(f.Name);
    ende := FindNext(f) <> 0;
  end;
  FindClose(f);
end;

end.
