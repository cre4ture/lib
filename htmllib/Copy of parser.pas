unit parser;

interface

uses
  Classes, SysUtils, FastStrings;

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
    pttComment
  );
  THTMLParser = class
  private
    AttrList: TList;
    procedure ClearAttrList;
    function GetCurrAttr(Index: Integer): THTMLParser_Attribute;
    function AddAttribute(Name, Value: String): Integer;
  public
    CurName: string;
    CurTagType: THTMLParserTagType;
    HTMLCode: string;
    CurPosition: Integer;
    CurContent: String;
    Ready: Boolean;
    property CurrAttr[Index: Integer]: THTMLParser_Attribute read GetCurrAttr;
    procedure StartParser;
    function Parse: Boolean;
    constructor Create;
    destructor Destroy; override;
    function AttrCount: Integer;
  end;

function ReadTagValue(html: string; pos: Integer; var TagValue: string): Integer;

implementation

uses StrUtils;

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

function THTMLParser.GetCurrAttr(Index: Integer): THTMLParser_Attribute;
begin
  Result := THTMLParser_Attribute(AttrList[Index]^);
end;

function findfirst(s: string; c1, c2: char; pos: Integer): Integer;
{ Gibt die position des zuerst gefundenen Zeichens zur¸ck, falls keines der
  beiden Zeichen gefunden wurde, wird length(s)+1 zur¸ckgegeben! }
var p: integer;
begin
  Result := FastCharPos(s,c1,pos);
  p := FastCharPos(s,c2,pos);
  if (p > 0)and((Result = 0)or(p < Result)) then
    Result := p
  else
    if Result = 0 then
      Result := length(s)+1;
end;

function THTMLParser.Parse: Boolean;
var ps, pe: Integer;
    s, aname, aval: string;
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
    CurPosition := FastPosNoCase(HTMLCode,'</script',length(HTMLCode),8,CurPosition)
  else
    CurPosition := FastCharPos(HTMLCode,'<',CurPosition);
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
  pe := findfirst(HTMLCode,' ','>',CurPosition);
  if HTMLCode[pe-1] = '/' then
    pe := pe-1;
  CurName := Copy(HTMLCode,CurPosition+1,pe-CurPosition-1);

  //teste auf Comment
  if Copy(CurName,1,3) = '!--' then
  begin
    CurName := Copy(CurName,1,3);
    CurTagType := pttComment;
    pe := FastPos(HTMLCode,'-->',length(HTMLCode),3,CurPosition);
    CurContent := Copy(HTMLCode,CurPosition,pe-CurPosition+3);
    CurPosition := pe+3;
    Result := true;
    Exit;
  end;

  CurPosition := pe;

  while true do
  begin
    ps := FastCharPos(HTMLCode,'=',CurPosition);
    pe := FastCharPos(HTMLCode,'>',CurPosition);
    //Falls '>' zum abschluss des tags fehlt:
    if pe = 0 then pe := length(HTMLCode)+1;

    //Wenn Attribute vorhanden, dann lese die erst ein. Ansonsten breche ab!
    if (ps > 0)and(ps < pe) then
    begin
      aname := Trim(Copy(HTMLCode,CurPosition+1,ps-(CurPosition+1)));
      CurPosition := ReadTagValue(HTMLCode,ps+1,s);
      aval := s;
      AddAttribute(aname,aval);
    end else break;
  end;

  if (HTMLCode[pe-1] = '/')and(CurTagType = pttStartTag) then
    CurTagType := pttEmptyTag;

  CurPosition := pe+1;
  Result := True;
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
    ps := FastCharPos(html,' ',Result);
    pe := FastCharPos(html,'>',Result);  //   class=inactive>
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
      Result := FastCharPos(html,Quote,Result+1);
      if (Result = 0) then
      begin
        Result := FastCharPos(html,'>',Result+1);
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
end;

end.
