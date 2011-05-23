unit parser;

{
################################################################################

  Author: Ulrich Hornung
  Date: 16.6.2008
  Modified: 13.02.2010

  This Unit is only for parsing HTML-Code in single tags. Thus html is a kind of
  XML it can also be used for parsing XML-Code. WARNING: Some spezial HTML-Tags
  are treated in an unnormal way! i.e. <script></script>

################################################################################
}

interface

uses
  Classes, SysUtils, fast_strings;

type
  PHTMLParser_Attribute = ^THTMLParser_Attribute;
  THTMLParser_Attribute = record
    Name: String;
    Value: String;
  end;
  THTMLParserTagType = (
    pttNone,
    pttStartTag,
    pttEndTag,
    pttEmptyTag,
    pttContent,
    pttComment,
    pttCDATA
  );
  THTMLParser = class
  private
    fCurPos: integer;
    AttrList: TList;
    procedure ClearAttrList;
    function GetCurrAttr(Index: Integer): THTMLParser_Attribute;
    function AddAttribute(Name, Value: String): Integer;
    function getCP: Integer;
    procedure setCP(const Value: Integer);
  public
    CurName: string;
    CurTagType: THTMLParserTagType;
    HTMLCode: string;
    CurContent: String;
    Ready: Boolean;
    property CurPosition: Integer read getCP write setCP;
    property CurrAttr[Index: Integer]: THTMLParser_Attribute read GetCurrAttr;
    procedure StartParser;
    function Parse: Boolean;
    constructor Create;
    destructor Destroy; override;
    function AttrCount: Integer;
  end;

function ReadTagValue(html: string; pos: Integer; var TagValue: string): Integer;
function ReadAttribute(html: string; var Position: integer;
  var TagName, TagValue: string): boolean;

implementation

function THTMLParser.AddAttribute(Name, Value: String): Integer;
var attr: PHTMLParser_Attribute;
begin
  New(attr);
  attr^.Name := Name;
  attr^.Value := Value;
  Result := AttrList.Add(attr);
end;

function THTMLParser.AttrCount: Integer;
begin
  Result := AttrList.Count;
end;

procedure THTMLParser.ClearAttrList;
begin
  while AttrList.Count > 0 do
  begin
    {
      Hier war das Speicherleck:
      Dispose(AttrList[0]);
      Wenn Dispose nicht weis welchen Typ es freizugeben hat,
      Kann es keine Dynamischen String/Arrays mitfreigeben!
      Siehe Online-Hilfe "Finalize"

      Deswegen:
    }
    Dispose(PHTMLParser_Attribute(AttrList[0]));
    AttrList.Delete(0);
  end;
end;

constructor THTMLParser.Create;
begin
  inherited;
  AttrList := TList.Create;
end;

destructor THTMLParser.Destroy;
begin
  AttrList.Free;
  inherited;
end;

function THTMLParser.getCP: Integer;
begin
  result := fCurPos;
end;

function THTMLParser.GetCurrAttr(Index: Integer): THTMLParser_Attribute;
begin
  Result := THTMLParser_Attribute(AttrList[Index]^);
end;

function THTMLParser.Parse: Boolean;
var ps, pe, tmp: Integer;
    aname, aval: string;
    treffer: char;
begin
  ClearAttrList();

  //Wenn am ende des strings angekommen -> fertig!
  If CurPosition >= length(HTMLCode) then
  begin
    Result := False;
    Ready := True;
    Exit;
  end;

  ps := CurPosition;
  //Wenn Letzter Tag ein Starttag war und "script" hieﬂ, muss der n‰chste tag </script> sein!
  if (CurTagType = pttStartTag)and(LowerCase(CurName) = 'script') then
    CurPosition := pos_no_case(HTMLCode,'</script',length(HTMLCode),8,CurPosition)
  else
    CurPosition := char_pos(HTMLCode,'<',CurPosition);
  CurTagType := pttNone;
  //Wenn Ende erreicht, rest noch als content zur¸ckgeben!
  if CurPosition = 0 then
    CurPosition := length(HTMLCode)+1;

  //teste auf content
  if (ps < CurPosition) then
  begin
    CurTagType := pttContent;
    CurContent := Copy(HTMLCode,ps,CurPosition-ps);
    Result := true;
    Exit;
  end;

  if (CurPosition > 0)and(HTMLCode[CurPosition+1] = '/') then
  begin
    CurTagType := pttEndTag;
    CurPosition := CurPosition+1;
  end
  else CurTagType := pttStartTag;

  //Tagname lesen
  treffer := find_first_char(HTMLCode, '! >'#9#13, CurPosition, pe);
  if HTMLCode[pe-1] = '/' then
    pe := pe-1;
  CurName := Copy(HTMLCode,CurPosition+1,pe-CurPosition-1);

  // Kommentar? oder CDATA?
  if (treffer = '!') then
  begin
    CurName := Copy(HTMLCode, CurPosition+1, 3);
    if CurName = '!--' then
    begin
      CurTagType := pttComment;
      pe := fast_pos(HTMLCode,'-->',length(HTMLCode),3,CurPosition);
      CurContent := Copy(HTMLCode,CurPosition,pe-CurPosition+3);
      CurPosition := pe+3;
      Result := true;
      Exit;
    end
    else
    begin
      CurName := Copy(HTMLCode, CurPosition+1, 8);
      if CurName = '![CDATA[' then
      begin
        CurPosition := CurPosition + 9;
        CurTagType := pttContent;
        pe := fast_pos(HTMLCode,']]>',length(HTMLCode),3,CurPosition);
        CurContent := Copy(HTMLCode,CurPosition,pe-CurPosition);
        CurPosition := pe+3;
        Result := true;
        Exit;
      end
      else
      begin
        CurName := '!???';
      end;
    end;
  end;

  CurPosition := pe;

  tmp := CurPosition;
  while ReadAttribute(HTMLCode, tmp, aname, aval) do
  begin
    //Wenn Attribute vorhanden, dann lese die erst ein. Ansonsten breche ab!
    AddAttribute(aname,aval);
  end;
  CurPosition := tmp;

  if (HTMLCode[CurPosition] = '/') then
  begin
    CurPosition := CurPosition+1;
    if(CurTagType = pttStartTag) then
      CurTagType := pttEmptyTag;
  end;

  CurPosition := CurPosition+1;
  Result := True;
end;

procedure THTMLParser.setCP(const Value: Integer);
begin
  if Value >= fCurPos then
    fCurPos := Value
  else
    raise Exception.Create('THTMLParser.setCP: Step Backward detected!');
end;

procedure THTMLParser.StartParser;
begin
  CurPosition := 1;
  CurName := '';
  CurTagType := pttNone;
  Ready := False;
end;

function ReadTagValue(html: string; pos: Integer; var TagValue: string): Integer;
var Quote: Char;
    pe, ps: Integer;
begin
  Result := pos;
  Quote := UTF8Encode(html[Result])[1];
  if not((Quote = '"')or(Quote = '''')) then
  begin
    Quote := #0;

    ps := char_pos(html,' ',Result);
    pe := char_pos(html,'>',Result);  //   class=inactive>
    if (pe > 0)and((ps = 0)or(pe < ps)) then
    begin
      Result := pe;
      if html[Result-1] = '/' then
        Result := Result-1;
    end else Result := ps;
    if Result = 0 then
      Result := length(html)+1;
  end
  else
  begin
    pos := pos+1;
    while true do
    begin
      Result := char_pos(html,Quote,Result+1);
      if (Result = 0) then
      begin
        Result := char_pos(html,'>',Result+1);
        if (Result = 0) then
          Result := length(html)+1
        else
          if html[Result-1] = '/' then
            Result := Result-1;
        break;
      end
      else
      if (not(html[Result-1] = '\')) then
        break;
    end;
  end;

  TagValue := Copy(html,pos,Result-pos);

  if Quote <> #0 then inc(Result);
end;

function ReadAttribute(html: string; var Position: integer;
  var TagName, TagValue: string): boolean;
var pe, pl, pg: integer;
begin
  // trim all whitespaces at actual position
  while (position <= length(html))and
        (html[Position] <= ' ' {in  [' ', #10, #13, #9]}) do Position := Position + 1;

  pe := char_pos(html, '>', Position);
  if (pe = 0) then pe := length(html);

  pg := char_pos(html, '=', Position);
  if (pg = 0) then pg := high(integer);

  pl := char_pos(html, ' ', Position);
  if (pl = 0) then pl := high(integer);

  //Wenn '>' das erste gefundene Zeichen ist
  if (pe <= pg)and(pe <= pl) then
  begin
    if html[pe-1] = '/' then dec(pe);

    TagName := trim(copy(html, Position, pe-position));
    TagValue := TagName;
    Position := pe;
  end else
  //Wenn '=' das erste gefundene Zeichen ist
  if (pg <= pl) then
  begin
    TagName := trim(copy(html, Position, pg-position));
    Position := ReadTagValue(html, pg+1, TagValue);
  end else
  //Wenn ' ' das erste gefundene Zeichen ist
  begin
    TagName := trim(copy(html, Position, pl-position));
    TagValue := TagName;
    Position := pl;
  end;

  Result := (TagName <> '');
end;

end.
