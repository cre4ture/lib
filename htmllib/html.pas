unit html;

{
################################################################################

  Author: Ulrich Hornung
  Date: 16.6.2008
  Modified: 19.09.08

  This Unit is for parsing HTML-Code in a tree structure. Thus html is a kind of
  XML it can also be used for parsing XML-Code. WARNING: Tags like <link>/<meta>
  /<img>/<br>/<input> are always treated like empty tags. You can change this by
  editing the "html_emptytags" constant.

################################################################################
}

interface

{//test bestanden! $define memcheck}

{$ifndef delphi}
{$define lazarus}
{$endif}

{$define use_cpp_parser}

uses
  Classes, SysUtils, parser_types,
  StrUtils {$ifdef memcheck}, MemCheckObject{$endif}
  {$ifdef use_cpp_parser}, cpp_dll_interface{$endif}
  {$ifndef use_cpp_parser}, parser{$endif}
  ;
  

const
  html_emptytags: array[0..6] of string = (
    'link', 'meta', 'img', 'br', 'input', 'param', 'tr"' (*OGame QuickFix for IE*)
  );

type
  THTMLPathNotFoundException = class(Exception);
{$ifndef use_cpp_parser}
  TParserInterface = THTMLParser;
{$else}
  TParserInterface = TCPPHTMLParser;
{$endif}
  {TParserInterface = class
  protected
    function getCurrAttr(Index: Integer): THTMLParser_Attribute; virtual; abstract;
  public
    function CurName: string; virtual; abstract;
    function CurTagType: THTMLParserTagType; virtual; abstract;
    function CurContent: String; virtual; abstract;
    property CurrAttr[Index: Integer]: THTMLParser_Attribute read getCurrAttr;
    function Ready: boolean; virtual; abstract;
    function Parse: Boolean; virtual; abstract;
    function AttrCount: Integer; virtual; abstract;
    constructor Create(htmlcode: string); virtual; abstract;
  end;}
  TXDocType = (xdt_html, xdt_xml);
  THTMLAttribute = class{$ifdef memcheck}(TMemCheckObj){$endif}
  public
    Name: string;
    Value: string;
    constructor Create(const aName: string = ''; const aValue: string = '');
    function AsInteger: Integer; overload;
    function AsInteger(default: integer): Integer; overload;
    function ToString: string;
  end;
  THTMLElement = class; //Vorabdeklaration
  TFindTagCheckFunction = function
    (CurElement: THTMLElement; Data: pointer): Boolean of object;
  THTMLElement = class{$ifdef memcheck}(TMemCheckObj){$endif}
  private
    FTagName: String;
    attrs: TList;
    childs: TList;
    dummyAttr: THTMLAttribute;
    docType: TXDocType;
    function html_isemptytag(tagname: string): Boolean;
    function GetAttributeValueByName(Name: string): string;
    procedure SetAttributeValueByName(Name: string; value: string);
    function GetAttributeByIndex(Index: Integer): THTMLAttribute;
    function GetChildTag(Index: Integer): THTMLElement;
    function ParseChilds(p: TParserInterface): Integer;
    function GetAttrByName(const name: string): THTMLAttribute;
    function GetAttrByNameDef(const name: string): THTMLAttribute;
  public
    Content: String;
    TagNameNr: Integer;
    TagType: THTMLParserTagType;
    ParentElement: THTMLElement;
    property TagName: String read FTagName write FTagName;
    property AttributeValue[Name: string]: string
       read GetAttributeValueByName
       write SetAttributeValueByName; default;
    property AttributesByName[const Name: string]: THTMLAttribute read GetAttrByNameDef;
    property Attributes[Index: Integer]: THTMLAttribute read GetAttributeByIndex;
    property ChildElements[Index: Integer]: THTMLElement read GetChildTag;
    /// Create
    /// Constructor of THTMLELement
    /// Attension: If you give here the aParent parameter, you must not
    /// call AddChildElement in addition.
    constructor Create(aParent: THTMLElement;
      const aTagName: String; aDocType: TXDocType = xdt_html);
    function FindChildTag(aName: WideString; Index: Integer = 0): THTMLElement;
    ///  FindChildTagPath
    ///  'html/body/div:10/a:2' <- find such paths
    ///  if no ':[0..9]' is given ':0' is assumed
    ///  if ':x' is given, every element with the name is searched,
    ///  the first matching element is returned.
    function FindChildTagPath(aPath: String): THTMLElement;
    function FindChildTagPath_e(aPath: String): THTMLElement;
    function FindChildTagEx(aName1, aName2: WideString; Index: Integer = 0): THTMLElement;
    function AttributeCount: Integer;
    function AttributesToString: String;
    destructor Destroy; override;
    function AttributeAdd(const aName, aValue: String): Integer;
    function TagPath: String;
    function FullTagContent: String;
    procedure ClearAttributes;
    /// ClearChilds:
    ///  Destroys all Childobjects
    procedure ClearChilds;
    function ChildCount: Integer;
    function AddChildElement(child: THTMLElement): Integer;
    function FullHTML: string;
    procedure ParseHTMLCode(const s: string);
    procedure ParseHTMLCodeWide(const s: WideString);
    procedure ParseHTMLCodeUTF8(const s: AnsiString);
    function FindTagRoutine(
      checkfunction: TFindTagCheckFunction; Data: pointer): THTMLElement;
    function ChildNameCount(const aTagName: String): Integer;
    procedure ChildsMove(NewParent: THTMLElement);
    ///  DeleteTagRoutine:
    ///  Traversiert rekursiv alle Child-Tags und ruft für jedes Element die "checkfunction" auf.
    ///  wenn "checkfunction" = true wird das aktuelle Element gelöscht, es wird beim darauffolgenden Element weitergemacht.
    ///  \return Anzahl der gelöschten Elemente
    ///  \param checkfunction Funtionszeiger auf checkfunktion
    ///  \Data Pointer wird der checkfuntion  als Parameter übergeben
    function DeleteTagRoutine(checkfunction: TFindTagCheckFunction;
      Data: pointer): integer;
    procedure DeleteChildElement(index: integer);
  end;
  EHTMLTableError = class(Exception);
  THTMLTableRow = class(THTMLElement)
  private
    function GetRowItem(Index: Integer): THTMLElement;
  public
    property RowItems[Index: Integer]: THTMLElement read GetRowItem;
    function RowItemCount: Integer;
  end;
  THTMLTable = class(THTMLElement)
  private
    function GetRow(Index: Integer): THTMLTableRow;
    function GetCell(row, col: Integer): THTMLElement;
  public
    property Rows[Index: Integer]: THTMLTableRow read GetRow;
    property Cells[row, col: Integer]: THTMLElement read GetCell;
    function RowCount: Integer;
  end;

  //For HTMLFindRoutine_NameAttribute:
  THTMLFindRoutine_dataobj = class
  public
    tag_name: string;
    attr_name: string;
    attr_value: string;
    function routine(CurElement: THTMLElement; Data: pointer): Boolean;
  end;
  //For HTMLFindRoutine_NameAttributeWithin:
  THTMLFindRoutine_dataobj_within = class
  public
    tag_name: string;
    attr_name: string;
    attr_value: string;
    function routine(CurElement: THTMLElement; Data: pointer): Boolean;
  end;
  // for HTMLFindRoutineEx
  THTMLFindRoutingEx_FindCondition_NameType = (
    frefcnt_content,
    frefcnt_tagname,
    frefcnt_attribute
  );
  THTMLFindRoutingEx_FindCondition = class
  public
    nametype: THTMLFindRoutingEx_FindCondition_NameType;
    attrname: string;
    value: string;
    within: boolean;
    function apply(node: THTMLElement): boolean;
  end;
  THTMLFindRoutineEx = class
  private
    fList: TList;
  public
    function addCondition(cond: THTMLFindRoutingEx_FindCondition): integer;
    function routine(CurElement: THTMLElement; Data: pointer): Boolean;
    constructor Create;
    destructor Destroy; override;
    function searchFor(root_tag: THTMLElement): THTMLElement;
  end;
  //For HTMLGenerateHumanReadableText:
  THTMLGenerateHumanReadableText_dataobj = class
  private
    rootnode: THTMLElement;
    list: TStringList;
    line: string;
    function routine(CurElement: THTMLElement; Data: pointer): Boolean;
  public
    function generate: string;
    constructor Create(node: THTMLElement);
    destructor Destroy; override;
  end;
  

function UnEscapeStr(s: string; esc: char = '\'): string;
function EscapeStr(s: string; esc: char = '\'; toesc: char = '"'): string;
function HTMLFindRoutine_NameAttribute(root_tag: THTMLElement; ftag_name,
  ftag_attribute, ftag_attribute_value: string): THTMLElement;
function HTMLFindRoutine_NameAttribute_Within(root_tag: THTMLElement; ftag_name,
  ftag_attribute, ftag_attribute_value: string): THTMLElement;

function HTMLGenerateHumanReadableText(node: THTMLElement): string;

implementation

function conv_UTF8(const s: AnsiString): string;
begin
{$ifdef UNICODE}
  Result := UTF8ToWideString(s);
{$else}
  Result := s;
{$endif}
end;

function HTMLGenerateHumanReadableText(node: THTMLElement): string;
var iterator: THTMLGenerateHumanReadableText_dataobj;
begin
  iterator := THTMLGenerateHumanReadableText_dataobj.Create(node);
  try
    Result := iterator.generate;
  finally
    iterator.Free;
  end;
end;

function THTMLElement.html_isemptytag(tagname: string): Boolean;
var i: integer;
begin
  Result := False;

  if (docType = xdt_html) then
  begin
    for i := 0 to length(html_emptytags)-1 do
    begin
      if (tagname = html_emptytags[i]) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function UnEscapeStr(s: string; esc: char = '\'): string;
var p: Integer;
begin
  p := PosEx(esc, s);
  while (p > 0) do
  begin
    s := copy(s,1,p-1) + copy(s,p+1,high(integer));
    p := PosEx(esc,s,p+1);
  end;
  Result := s;
end;

function EscapeStr(s: string; esc: char = '\'; toesc: char = '"'): string;
var p: Integer;
begin
  p := PosEx(toesc, s);
  while (p > 0) do
  begin
    s := copy(s,1,p-1) + esc + copy(s,p,high(integer));
    p := PosEx(toesc,s,p+2);
  end;
  Result := s;
end;

procedure THTMLElement.ParseHTMLCodeWide(const s: WideString);
var p: TParserInterface;
begin
  //RootElement ist das Element selbst!
  p := TParserInterface.Create(UTF8Encode(s));
  try
    repeat
      ParseChilds(p);
    until p.Ready;
  finally
    p.Free;
  end;
end;

procedure THTMLElement.ParseHTMLCodeUTF8(const s: AnsiString);
var p: TParserInterface;
begin
  //RootElement ist das Element selbst!
  p := TParserInterface.Create(s);
  try
    repeat
      ParseChilds(p);
    until p.Ready;
  finally
    p.Free;
  end;
end;

procedure THTMLElement.ParseHTMLCode(const s: string);
begin
{$ifdef UNICODE}
  ParseHTMLCodeWide(s);
{$else}
  ParseHTMLCodeUTF8(s);
{$endif}
end;

function THTMLAttribute.AsInteger: Integer;
begin
  Result := StrToInt(Value);
end;

function THTMLAttribute.AsInteger(default: integer): Integer;
begin
  Result := StrToIntDef(Value, default);
end;

constructor THTMLAttribute.Create(const aName: string = ''; const aValue: string = '');
begin
  inherited Create;
  
  Name := aName;
  Value := aValue;
end;

constructor THTMLElement.Create(aParent: THTMLElement;
  const aTagName: String; aDocType: TXDocType = xdt_html);
begin
  inherited Create;
  docType := aDocType;
  FTagName := aTagName;
  dummyAttr := THTMLAttribute.Create();
  attrs := TList.Create;
  childs := TList.Create;
  ParentElement := nil;  // wird bei AddChildElement gesetzt
  if aParent <> nil then
    aParent.AddChildElement(Self);
end;

function THTMLElement.GetAttributeByIndex(Index: Integer): THTMLAttribute;
begin
  Result := THTMLAttribute(attrs[Index]);
end;

function THTMLElement.GetAttributeValueByName(Name: string): string;
var attr: THTMLAttribute;
begin
  Result := '';
  attr := GetAttrByName(Name);
  if attr <> nil then
  begin
    Result := attr.Value;
  end;
end;

function THTMLTable.GetRow(Index: Integer): THTMLTableRow;
begin
  Result := THTMLTableRow(FindChildTag('tr',Index));
  if Result = nil then
    raise EHTMLTableError.Create('The requested row doesn''t exist!');
end;

function THTMLTable.GetCell(row, col: Integer): THTMLElement;
begin
  result := GetRow(row).RowItems[col];
end;

function THTMLElement.FindChildTag(aName: WideString; Index: Integer = 0): THTMLElement;
var i, c: Integer;
begin
  c := -1;
  Result := nil;
  for i := 0 to ChildCount-1 do
  begin
    if (ChildElements[i].TagName = aName) then
      inc(c);

    if (c = Index) then
    begin
      Result := ChildElements[i];
      break;
    end;
  end;
end;

function THTMLElement.FindChildTagEx(aName1, aName2: WideString; Index: Integer = 0): THTMLElement;
var i, c: Integer;
begin
  c := -1;
  Result := nil;
  for i := 0 to ChildCount-1 do
  begin
    if (ChildElements[i].TagName = aName1)or
       (ChildElements[i].TagName = aName2) then
      inc(c);

    if (c = Index) then
    begin
      Result := ChildElements[i];
      break;
    end;
  end;
end;

function THTMLTableRow.GetRowItem(Index: Integer): THTMLElement;
begin
  Result := FindChildTagEx('th','td',Index);
  if Result = nil then
    raise EHTMLTableError.Create('The requested rowitem doesn''t exist!');
end;


function THTMLElement.AttributeCount: Integer;
begin
  Result := attrs.Count;
end;

function THTMLElement.AttributesToString: String;
var i: integer;
begin
  Result := '';
  for i := 0 to AttributeCount-1 do
  begin
    Result := Result + ' ' + Attributes[i].ToString;
  end;
end;

function THTMLElement.GetChildTag(Index: Integer): THTMLElement;
begin
  Result := THTMLElement(childs[Index]);
end;

destructor THTMLElement.Destroy;
begin
  ClearChilds;
  ClearAttributes;
  attrs.Free;
  childs.Free;
  dummyAttr.Free;
  inherited;
end;

function THTMLElement.AttributeAdd(const aName, aValue: String): Integer;
begin
  Result := attrs.Add(THTMLAttribute.Create(aName, aValue));
end;

function THTMLElement.TagPath: String;
begin
  if ParentElement <> nil then
    Result := ParentElement.TagPath + TagName + ':' + IntToStr(TagNameNr) + '/'
  else
    Result := '';
end;

function THTMLElement.FullTagContent: String;
var i: integer;
begin
  if TagType = pttComment then
    Result := ''
  else
    Result := Content;
    
  for i := 0 to ChildCount-1 do
  begin
    Result := Result + ChildElements[i].FullTagContent;
  end;
end;

procedure THTMLElement.ClearAttributes;
var i: integer;
begin
  for i := 0 to attrs.Count-1 do
  begin
    THTMLAttribute(attrs[i]).Free;
  end;
  attrs.Clear;
end;

procedure THTMLElement.ClearChilds;
var i: Integer;
begin
  for i := 0 to childs.Count-1 do
  begin
    THTMLElement(childs[i]).Free;
  end;
  childs.Clear;
end;

function THTMLElement.ChildCount: Integer;
begin
  result := childs.Count;
end;

function THTMLElement.AddChildElement(child: THTMLElement): Integer;
var i, c: integer;
begin
  c := 0;
  for i := 0 to ChildCount-1 do
  begin
    if ChildElements[i].TagName = child.TagName then
      inc(c);
  end;
  child.TagNameNr := c;
  child.ParentElement := Self;
  Result := childs.Add(child);
end;

function THTMLElement.FullHTML: string;
var i: integer;
begin
  if (TagType = pttContent)or
     (TagType = pttComment) then
    Result := Content
  else
  begin
    Result := '<'+TagName;
    if AttributeCount > 0 then
      Result := Result + AttributesToString;

    if TagType <> pttEmptyTag  then
    begin
      Result := Result + '>';
      for i := 0 to ChildCount-1 do
      begin
        Result := Result + ChildElements[i].FullHTML;
      end;
      Result := Result + '</'+TagName+'>';
    end
    else Result := Result + '/>';
  end;
end;

function THTMLAttribute.ToString: string;
begin
  if pos('"',Value) = 0 then
    Result := Name + '="' + Value + '"'
  else
    Result := Name + '=''' + Value + '''';
end;

function SameTagNames(name1, name2: string): Boolean;
begin
  //Ich musste diese function mit einprogramieren weil der ogame-progarmiere
  //so ein mords schlamper ist und teilweise eine tabellenzelle mit <td>
  //anfängt und mit </th> beendet (oder andersrum).
  // Komischerweise interpretiert das der browser sogar richig!

  Result := (name1 = name2);
  if (not Result)and((name1 = 'td')or(name1 = 'th')) then
  begin
    Result := (name2 = 'th')or(name2 = 'td');
  end;
end;

function THTMLElement.ParseChilds(p: TParserInterface): Integer;
var child: THTMLElement;
    ende: Boolean;

  procedure DoStartTag;
  var i: Integer;
      tname: string;
      tmp_attr: THTMLParser_Attribute;
  begin
    tname := conv_UTF8(p.CurName);
    if tname = 'table' then
      child := THTMLTable.Create(Self, tname)
    else
    if tname = 'tr' then
    begin
      child := THTMLTableRow.Create(Self, tname)
    end
    else
      child := THTMLElement.Create(Self, tname, docType);

    child.TagType := p.CurTagType;

    for i := 0 to p.AttrCount-1 do
    begin
      tmp_attr := p.CurrAttr[i];
      child.AttributeAdd(conv_UTF8(tmp_attr.Name),conv_UTF8(tmp_attr.Value));
    end;

    if (p.CurTagType <> pttEmptyTag)and(not html_isemptytag(tname)) then
      if child.ParseChilds(p) < 0 then // end tag error workaround
        ende := true;
  end;

  procedure DoEndTag;
  begin
    ende := SameTagNames(TagName, conv_UTF8(p.CurName));
    if not ende then  // workaround for common syntax errors in htmlcode
    begin
      if (ParentElement <> nil)and
         (SameTagNames(ParentElement.TagName, conv_UTF8(p.CurName))) then
      begin
        child := THTMLElement.Create(Self,'<-/'+conv_UTF8(p.CurName), docType);
        child.TagType := pttNone;
        ende := true;
        // activate workaround:
        Result := (Result+1) *-1;
      end else
      begin
        child := THTMLElement.Create(Self,'x/'+conv_UTF8(p.CurName), docType);
        child.TagType := pttNone;
      end;
    end;
  end;

  procedure DoContent;
  begin
    child := THTMLElement.Create(Self, '><',docType);
    child.TagType := pttContent;
    child.Content := conv_UTF8(p.CurContent);
  end;

  procedure DoComment;
  begin
    child := THTMLElement.Create(Self,'<!--',docType);
    child.Content := conv_UTF8(p.CurContent);
    child.TagType := p.CurTagType;
  end;

begin
  ende := false;
  Result := 0;
  while (not ende)and(p.Parse) do
  begin
    inc(Result);
    case p.CurTagType of
    pttStartTag, pttEmptyTag: DoStartTag;
    pttEndTag: DoEndTag;
    pttContent: DoContent;
    pttComment: DoComment;
    end;
  end;
end;

function THTMLElement.FindTagRoutine(
  checkfunction: TFindTagCheckFunction; Data: pointer): THTMLElement;
var i: Integer;
begin
  Result := nil;
  i := 0;
  while (Result = nil)and(i < ChildCount) do
  begin
    if checkfunction(ChildElements[i],Data) then
      Result := ChildElements[i]
    else
      Result := ChildElements[i].FindTagRoutine(checkfunction, Data);

    inc(i);
  end;
end;

function THTMLTable.RowCount: Integer;
begin
  Result := ChildNameCount('tr');
end;

function THTMLElement.ChildNameCount(const aTagName: String): Integer;
var i: Integer;
begin
  Result := 0;
  for i := 0 to ChildCount-1 do
    if ChildElements[i].TagName = aTagName then
      inc(Result);
end;

function THTMLTableRow.RowItemCount: Integer;
begin
  Result := ChildNameCount('td') + ChildNameCount('th');
end;

procedure THTMLElement.ChildsMove(NewParent: THTMLElement);
var i: integer;
begin
  for i := 0 to ChildCount-1 do
  begin
    NewParent.AddChildElement(ChildElements[i]);
  end;
  childs.Clear;
end;

function THTMLElement.FindChildTagPath_e(aPath: String): THTMLElement;
begin
  Result := FindChildTagPath(aPath);
  if Result = nil then
    raise THTMLPathNotFoundException.Create(
                           'THTMLElement.FindChildTagPath_e(' + aPath + '): '+
                           ' could not find tag!');
end;

function THTMLElement.FindChildTagPath(aPath: String): THTMLElement;
var ps, pp: integer;
    s: String;
    i: Integer;
begin
  ps := pos('/',aPath);
  if ps = 0 then ps := length(aPath)+1;
  pp := pos(':',aPath);
  if (pp = 0)or(pp > ps) then
  begin
    pp := ps;
    i := 0;
  end
  else
  begin
    s := copy(aPath,pp+1,ps-(pp+1));
    if s = 'x' then
      i := -1
    else
      i := StrToInt(s);
  end;

  //tagname
  s := copy(aPath,1,pp-1);
  //new path
  aPath := copy(aPath,ps+1,high(Integer));
  if s = '..' then // Eine Ebene höher 
  begin
    Result := Self.ParentElement;

    if Result <> nil then
    begin
      if length(aPath) > 0 then
        Result := Result.FindChildTagPath(aPath);
    end;
  end
  else
    if i = -1 then  //search all with this name
    begin
      Result := nil;

      for i := 0 to ChildCount-1 do
        if ChildElements[i].TagName = s then
        begin
          Result := ChildElements[i];
          if length(aPath) > 0 then
            Result := Result.FindChildTagPath(aPath);

          if Result <> nil then
            break;
        end;
    end
    else
    begin
      Result := FindChildTag(s,i);

      if Result <> nil then
      begin
        if length(aPath) > 0 then
          Result := Result.FindChildTagPath(aPath);
      end;
    end;
end;

function THTMLElement.DeleteTagRoutine(
  checkfunction: TFindTagCheckFunction; Data: pointer): integer;
var i: Integer;
begin
  Result := 0;
  i := 0;
  while (i < ChildCount) do
  begin
    if checkfunction(ChildElements[i],Data) then
    begin
      Result := Result + 1;
      DeleteChildElement(i);
    end
    else
    begin
      Result := Result + ChildElements[i].DeleteTagRoutine(checkfunction, Data);
      inc(i);
    end;
  end;
end;

procedure THTMLElement.SetAttributeValueByName(Name: string;
  value: string);
var attr: THTMLAttribute;
begin
  attr := GetAttrByName(Name);
  //Wenn nicht vorhanden, dann erstellen:
  if attr = nil then
  begin
    AttributeAdd(Name, value);
  end
  else
  begin
    attr.Value := value;
  end;
end;

function THTMLElement.GetAttrByName(const name: string): THTMLAttribute;
var i: Integer;
begin
  Result := nil;
  for i := 0 to attrs.Count-1 do
  begin
    if (name = THTMLAttribute(attrs[i]).Name) then
    begin
      Result := THTMLAttribute(attrs[i]);
      break;
    end;
  end;
end;

function THTMLElement.GetAttrByNameDef(const name: string): THTMLAttribute;
begin
  Result := GetAttrByName(name);
  if Result = nil then
  begin
    dummyAttr.Name := name;
    dummyAttr.Value := '';
    Result := dummyAttr;
  end;
end;

procedure THTMLElement.DeleteChildElement(index: integer);
var i: integer;
    s: string;
begin
  // Actualize the TagNameNr
  s := ChildElements[index].TagName;
  for i := index+1 to ChildCount-1 do
  begin
    if ChildElements[i].TagName = s then
      dec(ChildElements[i].TagNameNr);
  end;
  
  ChildElements[index].Free;
  childs.Delete(index);
end;

function HTMLFindRoutine_NameAttribute(root_tag: THTMLElement; ftag_name,
  ftag_attribute, ftag_attribute_value: string): THTMLElement;
var fdata: THTMLFindRoutine_dataobj;
begin
  fdata := THTMLFindRoutine_dataobj.Create;
  try
    fdata.tag_name := ftag_name;
    fdata.attr_name := ftag_attribute;
    fdata.attr_value := ftag_attribute_value;
    Result := root_tag.FindTagRoutine({$ifdef lazarus}{@}{$endif}fdata.routine,nil);
  finally
    fdata.Free;
  end;
end;

function HTMLFindRoutine_NameAttribute_Within(root_tag: THTMLElement; ftag_name,
  ftag_attribute, ftag_attribute_value: string): THTMLElement;
var fdata: THTMLFindRoutine_dataobj_within;
begin
  fdata := THTMLFindRoutine_dataobj_within.Create;
  try
    fdata.tag_name := ftag_name;
    fdata.attr_name := ftag_attribute;
    fdata.attr_value := ftag_attribute_value;
    Result := root_tag.FindTagRoutine({$ifdef lazarus}{@}{$endif}fdata.routine,nil);
  finally
    fdata.Free;
  end;
end;

function THTMLFindRoutine_dataobj.routine(CurElement: THTMLElement;
  Data: pointer): Boolean;
begin
  Result := (CurElement.TagName = Self.tag_name)and
            (CurElement.AttributeValue[Self.attr_name] = Self.attr_value);
end;

{ THTMLGenerateHumanReadableText_dataobj }

constructor THTMLGenerateHumanReadableText_dataobj.Create(
  node: THTMLElement);
begin
  inherited Create;
  rootnode := node;
  list := TStringList.Create;
end;

destructor THTMLGenerateHumanReadableText_dataobj.Destroy;
begin
  list.Free;
  inherited;
end;

function THTMLGenerateHumanReadableText_dataobj.generate: string;
begin
  rootnode.FindTagRoutine(routine, nil);
  Result := list.Text;
end;

function THTMLGenerateHumanReadableText_dataobj.routine(
  CurElement: THTMLElement; Data: pointer): Boolean;

  procedure append(text: string);
  begin
    line := line + text;
  end;

  procedure newLine;
  begin
    list.Add(line);
    line := '';
  end;

begin
  Result := false; // we just want to iterate over all elements!
  
  case CurElement.TagType of
    pttStartTag, pttEmptyTag:
      begin
        if (CurElement.TagName = 'tr')or
           (CurElement.TagName = 'br') then
          newLine
        else
        if (CurElement.TagName = 'td')or
           (CurElement.TagName = 'th')  then
          append(#9)   // tab
        else
        if (CurElement.TagName = 'script') then
          ;  // noting
      end;
    pttEndTag, pttComment:
      ; // nothing
  else
    if not (
       (CurElement.ParentElement <> nil) and
       (CurElement.ParentElement.TagName = 'script')
       ) then
      append(CurElement.FullTagContent);
  end;
end;

{ THTMLFindRoutine_dataobj_within }

function THTMLFindRoutine_dataobj_within.routine(CurElement: THTMLElement;
  Data: pointer): Boolean;
begin
  Result := (CurElement.TagName = Self.tag_name)and
            (pos(Self.attr_value, CurElement.AttributeValue[Self.attr_name]) > 0);
end;

{ THTMLFindRoutineEx }

function THTMLFindRoutineEx.addCondition(
  cond: THTMLFindRoutingEx_FindCondition): integer;
begin
  Result := fList.Add(cond);
end;

constructor THTMLFindRoutineEx.Create;
begin
  inherited Create;
  fList := TList.Create;
end;

destructor THTMLFindRoutineEx.Destroy;
begin
  while (fList.Count > 0) do
  begin
    THTMLFindRoutingEx_FindCondition(fList[0]).Free;
    fList.Delete(0);
  end;
  fList.Free;
  inherited Destroy;
end;

function THTMLFindRoutineEx.routine(CurElement: THTMLElement;
  Data: pointer): Boolean;
var i: integer;
    cond: THTMLFindRoutingEx_FindCondition;
begin
  Result := true;
  for i := 0 to fList.Count-1 do
  begin
    cond := THTMLFindRoutingEx_FindCondition(fList[i]);
    Result := cond.apply(CurElement);
    if (not Result) then
      break;
  end;
end;

function THTMLFindRoutineEx.searchFor(root_tag: THTMLElement): THTMLElement;
begin
  Result := root_tag.FindTagRoutine({$ifdef lazarus}{@}{$endif}self.routine,nil);
end;

{ THTMLFindRoutingEx_FindCondition }

function THTMLFindRoutingEx_FindCondition.apply(
  node: THTMLElement): boolean;
var cond_content: string;
begin
  case nametype of
  frefcnt_content:
    begin
      cond_content := node.Content;
    end;
  frefcnt_tagname:
    begin
      cond_content := node.TagName;
    end;
  frefcnt_attribute:
    begin
      cond_content := node.AttributeValue[attrname];
    end;
  end;

  if within then
  begin
    Result := pos(value, cond_content) > 0;
  end
  else
  begin
    Result := (value = cond_content);
  end;
end;

end.
