unit fast_strings;

// Hier musst du "delphi" als conditional define angeben!
{$ifndef delphi}
{$mode objfpc}{$H+}
{$define lazarus}
{$endif}

interface

uses
  Classes, SysUtils, strutils;

function find_first_char(
  s: string;
  search_chars: string;
  pos: Integer;
  var where: Integer): char;
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

function find_first_char(
  s: string;
  search_chars: string;
  pos: Integer;
  var where: Integer): char;
{ Gibt das zuerst gefundene Zeichen zurück, falls keines gefunden wurde: #0
  Die Position wird in "where" geschrieben, falls keines der
  beiden Zeichen gefunden wurde, wird where := length(s)+1 ! }
var p, ic: integer;
begin
  where := length(s)+1;
  Result := #0;

  for ic := 1 to length(search_chars) do
  begin
    p := char_pos(s, search_chars[ic], pos);
    if (p > 0)and(p < where) then
    begin
      where := p;
      Result := search_chars[ic];
    end;
  end;
end;


end.

