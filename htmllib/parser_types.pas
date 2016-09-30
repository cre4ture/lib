unit parser_types;

interface

type
  THTMLParserTagType = (
    pttNone,
    pttStartTag,
    pttEndTag,
    pttEmptyTag,
    pttContent,
    pttComment,
    pttCDATA
  );
  PHTMLParser_Attribute = ^THTMLParser_Attribute;
  THTMLParser_Attribute = record
    Name: AnsiString;
    Value: AnsiString;
  end;


implementation

end.

