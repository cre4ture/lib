unit creaturesStrUtils;

interface

function ReplaceStr(const str: String; const find, replace: string): string;

implementation

uses StrUtils, Classes, SysUtils;

function ReplaceStr(const str: String; const find, replace: string): string;
var pstart, pfind: integer;
begin
  Result := '';
  pstart := 1;
  pfind := PosEx(find, str, pstart);
  while pfind > 0 do
  begin
    Result := Result + copy(str,pstart,pfind-pstart);
    pstart := pfind + length(find);
    pfind := PosEx(find, str, pstart);
  end;
end;

end.
