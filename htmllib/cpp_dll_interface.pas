unit cpp_dll_interface;

interface

uses html;

type
  ppChar = ^PChar;
  pParserHandle = ^Integer;
  pTagType = ^THTMLParserTagType;
  TCPPHTMLParser = class(TParserInterface)
  private
    myParser: pParserHandle;
    fname: PChar;
    ftype: THTMLParserTagType;
    fcontent: PChar;
    fattrcount: cardinal;
    fcode: string;
    fready: boolean;
  protected
    function getCurrAttr(Index: Integer): THTMLParser_Attribute; override;
  public
    function CurContent: String; override;
    function CurName: string; override;
    function CurTagType: THTMLParserTagType; override;
    function Parse: Boolean; override;
    function Ready: boolean; override;
    function AttrCount: Integer; override;
    constructor Create(htmlcode: string);
    destructor Destroy; override;
  end;

implementation

uses SysUtils;

const cpp_parser_dll = 'data/cpphtmlparser.dll';

function creax_createParser(htmlcode: PChar): pParserHandle; cdecl; external cpp_parser_dll;
function creax_freeParser(parser: pParserHandle): pParserHandle; cdecl; external cpp_parser_dll;
function creax_parse(parser: pParserHandle; tag_type: pTagType;
  tagName: ppChar; tagContent: ppChar;
  attributeCount: PCardinal): boolean; cdecl; external cpp_parser_dll;
function creax_getAttribute(parser: pParserHandle; index: cardinal;
  name: ppChar; value: ppChar): boolean; cdecl; external cpp_parser_dll;

{ TCPPHTMLParser }

function TCPPHTMLParser.AttrCount: Integer;
begin
  Result := fattrcount;
end;

constructor TCPPHTMLParser.Create(htmlcode: string);
begin
//   inherited Create(htmlcode); don't call an abstract constructor!
  fready := false;
  fcode := htmlcode;
  myParser := creax_createParser(PChar(fcode));
end;

destructor TCPPHTMLParser.Destroy;
begin
  creax_freeParser(myParser);
  inherited;
end;

function TCPPHTMLParser.CurContent: String;
begin
  Result := fcontent;
end;

function TCPPHTMLParser.getCurrAttr(Index: Integer): THTMLParser_Attribute;
var pname, pvalue: PChar;
begin
  if (not creax_getAttribute(myParser, index, @pname, @pvalue)) then
    raise Exception.Create('TCPPHTMLParser.GetCurrAttr(): Failed fetching attribute!');
  Result.Name := pname;
  Result.Value := pvalue;
end;

function TCPPHTMLParser.CurName: string;
begin
  Result := fname;
end;

function TCPPHTMLParser.CurTagType: THTMLParserTagType;
begin
  Result := ftype;
end;

function TCPPHTMLParser.Parse: Boolean;
begin
  Result := creax_parse(myParser, @ftype, @fname, @fcontent, @fattrcount);
  if not Result then
    fready := true;
end;

function TCPPHTMLParser.Ready: boolean;
begin
  Result := fready;
end;

end.
