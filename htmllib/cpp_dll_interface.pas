unit cpp_dll_interface;

  {
  UNICODE Handling: NONE! All Strings are AnsiStrings!
  You have to manually convert WideStrings into UTF8 if you want to use this
  Parser!

  16.10.2011 Ulrich Hornung
  }

interface

uses parser_types, Classes;

const MaxListSize = MaxInt div 16;

type
  ppAnsiChar = ^PAnsiChar;
  pParserHandle = ^Integer;
  pTagType = ^THTMLParserTagType;
  PAnsiCharList = packed array[0..MaxListSize-1] of PAnsiChar;
  pPAnsiCharList = ^PAnsiCharList;
  ppPAnsiCharList = ^pPAnsiCharList;
  TCPPHTMLParser = class
  private
    myParser: pParserHandle;
    fname: PAnsiChar;
    ftype: THTMLParserTagType;
    fcontent: PAnsiChar;
    fattrcount: cardinal;
    fcode: AnsiSTring;
    fready: boolean;
    fAttrNames: pPAnsiCharList;
    fAttrValues: pPAnsiCharList;
  protected
    function getCurrAttr(Index: Cardinal): THTMLParser_Attribute;
  public
    property CurrAttr[Index: Cardinal]: THTMLParser_Attribute read getCurrAttr;
    function CurContent: AnsiString;
    function CurName: AnsiString;
    function CurTagType: THTMLParserTagType;
    function Parse: Boolean;
    function Ready: boolean;
    function AttrCount: Integer;
    constructor Create(htmlcode: AnsiString);
    destructor Destroy; override;
  end;

implementation

uses SysUtils;

const cpp_parser_dll = 'cpphtmlparser.dll';

function creax_createParser(htmlcode: PAnsiChar): pParserHandle; cdecl; external cpp_parser_dll;
function creax_freeParser(parser: pParserHandle): pParserHandle; cdecl; external cpp_parser_dll;
function creax_parse(parser: pParserHandle; tag_type: pTagType;
  tagName: ppAnsiChar; tagContent: ppAnsiChar;
  attributeCount: PCardinal; names: ppPAnsiCharList;
  values: ppPAnsiCharList): Boolean; cdecl; external cpp_parser_dll;

function creax_getAttribute(parser: pParserHandle; index: cardinal;
  name: ppAnsiChar; value: ppAnsiChar): boolean; cdecl; external cpp_parser_dll;

{ TCPPHTMLParser }

function TCPPHTMLParser.AttrCount: Integer;
begin
  Result := fattrcount;
end;

constructor TCPPHTMLParser.Create(htmlcode: AnsiString);
begin
//   inherited Create(htmlcode); don't call an abstract constructor!
  fready := false;
  fcode := htmlcode;
  myParser := creax_createParser(PAnsiChar(fcode));
end;

destructor TCPPHTMLParser.Destroy;
begin
  creax_freeParser(myParser);
  inherited;
end;

function TCPPHTMLParser.CurContent: AnsiString;
begin
  Result := fcontent;
end;

function TCPPHTMLParser.getCurrAttr(Index: Cardinal): THTMLParser_Attribute;
//var pname, pvalue: PAnsiChar;
begin
  {if (not creax_getAttribute(myParser, index, @pname, @pvalue)) then
    raise Exception.Create('TCPPHTMLParser.GetCurrAttr(): Failed fetching attribute!');
  Result.Name := pname;
  Result.Value := pvalue;}
  if (Index >= fattrcount) then
    raise Exception.Create('TCPPHTMLParser.getCurrAttr(): index out of bounds!');
  Result.Name := (fAttrNames^)[Index];
  Result.Value := (fAttrValues^)[Index];
end;

function TCPPHTMLParser.CurName: AnsiString;
begin
  Result := fname;
end;

function TCPPHTMLParser.CurTagType: THTMLParserTagType;
begin
  Result := ftype;
end;

function TCPPHTMLParser.Parse: Boolean;
begin
  Result := creax_parse(myParser, @ftype, @fname, @fcontent, @fattrcount,
    @fAttrNames, @fAttrValues);
  if not Result then
    fready := true;
end;

function TCPPHTMLParser.Ready: boolean;
begin
  Result := fready;
end;

end.
