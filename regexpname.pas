unit regexpname;
(*******************************************************************************
 * Autor: Ulrich Hornung
 * Date: 05.05.2008
 *
 * This Unit extend the TRegExpr-Class with named sub-expressions
 *
 ******************************************************************************)

interface

uses
  regexpr, Classes;

type
  Tregexpn = class
  private
    namelist: array of string;
    procedure addsubexprname(nr: integer; name: string);
    procedure namedsubexpr(ex: string);

  public
    //Use this to access subexpressions by index!
    regexpr: TRegExpr;

    //Use this to start a new matching:
    function match(subject: string; Offset: Integer = 1): boolean;
    //Use this to continue matching:
    function matchnext: boolean;

    constructor Create;
    destructor Destroy; override;

    //Use this to set the (named)regular exprassion
    procedure setexpression(expr: string; namedsubex: boolean = true);

    //Use this to get the content of a named subexpression,
    //after successfull matching
    function getsubexpr(name: string): string;

    //keine Ahnung, nicht implementiert!
    function EscapeString(s: string): String;

    //Use this to get the name of the subexpression at index "nr"
    function MatchName(nr: integer): string;
    //Use this to get a list of all named subexpressions and its values
    //  Format:  <name> -> "<value>" <name2> -> "<value2>" ...
    function SubExListToStr: string;
  end;

implementation

uses SysUtils, StrUtils;

procedure Tregexpn.addsubexprname(nr: integer; name: string);
begin
  if nr >= 0 then
  begin
    if nr >= length(namelist) then
      setlength(namelist,nr+1);
    namelist[nr] := name;
  end else raise Exception.Create('regexpn.addsubexprname: negative nameindex!');
end;

constructor Tregexpn.Create;
begin
  inherited;
  regexpr := TRegExpr.Create;
end;

destructor Tregexpn.Destroy;
begin
  regexpr.Free;
  inherited;
end;

function Tregexpn.EscapeString(s: string): String;
begin
  Result := s;
end;

function Tregexpn.getsubexpr(name: string): string;
var i: integer;
begin
  Result := '';
  i := 0;
  while i < length(namelist) do
  begin
    if name = namelist[i] then
    begin
      Result := regexpr.Match[i];
      break;
    end;
    inc(i);
  end;
end;

function Tregexpn.matchnext: boolean;
begin
  result := regexpr.ExecNext;
end;

function Tregexpn.match(subject: string; Offset: Integer = 1): boolean;
begin
  regexpr.InputString := subject;
  result := regexpr.ExecPos(Offset);
end;

procedure Tregexpn.namedsubexpr(ex: string);
var p, pe, i: integer;
    exn, exname: string;
begin
  exn := '';
  i := 0;
  p := Pos('(',ex);
  while (p > 0) do
  begin
    if ((p = 1)or(ex[p-1] <> '\')) then
    begin
      pe := PosEx('>',ex,p);
      inc(i);
      if ((copy(ex,p,3) = '(?<')and(pe > 0)) then
      begin
        exname := copy(ex,p+3,pe-(p+3));
        Delete(ex,p+1,pe-p);
        addsubexprname(i,exname);
      end;
      //else addsubexprname(i,'');
    end;
    exn := exn + copy(ex,1,p);
    Delete(ex,1,p);
    p := Pos('(',ex);
  end;
  exn := exn + ex;
  regexpr.Expression := exn;
end;

procedure Tregexpn.setexpression(expr: string; namedsubex: boolean = true);
begin
  SetLength(namelist,0);
  if namedsubex then namedsubexpr(expr)
  else regexpr.Expression := expr;
end;

function Tregexpn.MatchName(nr: integer): string;
begin
  if nr < length(namelist) then
    Result := namelist[nr]
  else Result := '';
end;

function Tregexpn.SubExListToStr: string;
var i: integer;
begin
  Result := '';
  for i := 0 to length(namelist)-1 do
    if (namelist[i] <> '') then
    begin
      Result := Result + namelist[i] + ' -> "' + getsubexpr(namelist[i]) + '" ';
    end;
end;

end.
