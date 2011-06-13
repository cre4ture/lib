unit cpp_dll_interface;

interface

uses parser_types, Classes;

type
  ppChar = ^PChar;
  pParserHandle = ^Integer;
  pTagType = ^THTMLParserTagType;
  PCharList = packed array[0..MaxListSize] of PChar;
  pPCharList = ^PCharList;
  ppPCharList = ^pPCharList;
  TCPPHTMLParser = class
  private
    myParser: pParserHandle;
    fname: PChar;
    ftype: THTMLParserTagType;
    fcontent: PChar;
    fattrcount: cardinal;
    fcode: string;
    fready: boolean;
    fAttrNames: pPCharList;
    fAttrValues: pPCharList;
  protected
    function getCurrAttr(Index: Cardinal): THTMLParser_Attribute;
  public
    property CurrAttr[Index: Cardinal]: THTMLParser_Attribute read getCurrAttr;
    function CurContent: String;
    function CurName: string;
    function CurTagType: THTMLParserTagType;
    function Parse: Boolean;
    function Ready: boolean;
    function AttrCount: Integer;
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
  attributeCount: PCardinal; names: ppPCharList;
  values: ppPCharList): Boolean; cdecl; external cpp_parser_dll;

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

function TCPPHTMLParser.getCurrAttr(Index: Cardinal): THTMLParser_Attribute;
//var pname, pvalue: PChar;
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
