unit fast_strings;

// Hier musst du "delphi" als conditional define angeben!
{$ifndef delphi}
{$mode objfpc}{$H+}
{$define lazarus}
{$endif}

interface

uses
  Classes, SysUtils, strutils;

function find_first_char(s: string; search_chars: string; pos: Integer): Integer;
function char_pos(s: string; c: char; pos: integer): Integer;
function pos_no_case(s, search: string; source_length, search_length, offset: integer): integer;
function fast_pos(s, search: string; source_length, search_length, offset: integer): integer;

implementation

function fast_pos(s, search: string; source_length, search_length, offset: integer): integer;
begin
  Result := PosEx(search, s, offset);
end;

function pos_no_case(s, search: string; source_length, search_length, offset: integer): integer;
begin
  s := LowerCase(s);
  search := LowerCase(search);

  Result := PosEx(search, s, offset);
end;

function char_pos(s: string; c: char; pos: integer): Integer;
begin
  {Result := -1;
  for x := pos to length(s) do
  begin
    if s[x] = c then
    begin
      Result := x;
      break;
    end;
  end;}
  Result := PosEx(c, s, pos);
end;

function find_first_char(s: string; search_chars: string; pos: Integer): Integer;
{ Gibt die position des zuerst gefundenen Zeichens zurück, falls keines der
  beiden Zeichen gefunden wurde, wird length(s)+1 zurückgegeben! }
var p, ic: integer;
begin
  Result := length(s)+1;

  for ic := 1 to length(search_chars) do
  begin
    p := char_pos(s, search_chars[ic], pos);
    if (p > 0)and(p < Result) then
    begin
      Result := p;
    end;
  end;
end;


end.

