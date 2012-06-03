%option reentrant
%option prefix="LanAB_"
%option bison-bridge
%option bison-locations
%option noyywrap
%option yylineno

%{
	#include "LanAB_Context.h"
    #include "LanAB.tab.hpp"

	#define YY_EXTRA_TYPE LanAB_Context*
	#define YY_USER_ACTION yylloc->first_line = yylineno;

    #define YY_INPUT(buf,result,max_size) yyextra->yy_input(buf, result, max_size, YY_NULL)

    //#define CREAD(TEXT) std::cout << TEXT << std::endl
    #define CREAD(TEXT)

%}

/* maybe faster if we group some chars rather than put each single character back as TEXT: */
stuff           ([ =!:;&%,.\-+*/\\()\[\]{}\"'])+
identifier      [a-zA-Z_][a-zA-Z0-9_]*
macro_begin     [a-zA-Z_][a-zA-Z0-9_]*\(
some_number     [+-]?[0-9]+[a-zA-Z]*

const_num       [+-]?[0-9]+
const_num_l     [+-]?[0-9]+[L]

%x CMD DEFINE_NAME MACRO_PARAMS DEFINE_VALUE COND MESSAGE_START MESSAGE

%%

<INITIAL>"#"[ ]*             CREAD("START CMD"); yymore(); BEGIN(CMD);
<INITIAL>{identifier}        CREAD("IDENT"); yylval->text = new std::string(yytext); return K_IDENT;
<INITIAL>[\n]+               CREAD("NEWLINE"); yylval->text = new std::string(yytext); return TEXT;
<INITIAL>{stuff}             CREAD("<stuff>"); yylval->text = new std::string(yytext); return TEXT;
<INITIAL>{some_number}       CREAD("<some_number>"); yylval->text = new std::string(yytext); return TEXT;
<INITIAL>.                   CREAD("<unknown_text>"); CREAD(yytext); yylval->text = new std::string(yytext); return TEXT;

<CMD>"error"        CREAD("ERROR_CMD"); BEGIN(MESSAGE_START); return CMD_ERROR;
<CMD>"if"           CREAD("IF"); BEGIN(COND); return CMD_IF;
<CMD>"elif"         CREAD("ELIF"); BEGIN(COND); return CMD_ELIF;
<CMD>"ifdef"        CREAD("IFDEF");  return IFDEF;
<CMD>"undef"        CREAD("UNDEF");  return K_UNDEF;
<CMD>"ifndef"       CREAD("IFNDEF"); return IFNDEF;
<CMD>"else"         CREAD("ELSE");   return ELSE;
<CMD>"endif"        CREAD("ENDIF");  return ENDIF;
<CMD>"include"[ ]*  CREAD("INCLUDE; CMD END"); yymore(); BEGIN(INITIAL);
<CMD>"define"[ ]*   CREAD("START DEFINE"); BEGIN(DEFINE_NAME);
 <DEFINE_NAME>{macro_begin}     CREAD("macro_begin"); yylval->text = new std::string(yytext); BEGIN(MACRO_PARAMS); return MACRO_BEGIN;
 <DEFINE_NAME>{identifier}      CREAD("simple define"); yylval->text = new std::string(yytext); BEGIN(DEFINE_VALUE); return K_DEFINE;
     <MACRO_PARAMS>","              CREAD("NEXT_PARAM"); return ',';
     <MACRO_PARAMS>{identifier}     CREAD("MACRO_PARAM"); yylval->text = new std::string(yytext); return K_PARAM;
     <MACRO_PARAMS>")"              CREAD("MACRO_END"); BEGIN(DEFINE_VALUE); return MACRO_END;
 <DEFINE_VALUE>.                CREAD("yymore()"); yymore();
 <DEFINE_VALUE>"\\"\n           CREAD("K_DEFINE_VALUE_ENDLINE"); yylval->text = new std::string(yytext); return K_DEFINE_VALUE_ESC_ENDLINE;
 <DEFINE_VALUE>\n               CREAD("K_DEFINE_VALUE"); yylval->text = new std::string(yytext); BEGIN(INITIAL); return K_DEFINE_VALUE_ENDLINE;
<CMD>" "            /* ignore whitespace */
<CMD>"\t"           /* ignore whitespace */
<CMD>"\n"           CREAD("CMD END"); BEGIN(INITIAL); return ENDLINE;
<CMD>{identifier}   CREAD("NAME"); yylval->text = new std::string(yytext); return NAME;

<COND>"defined"     CREAD("defined"); return K_DEFINED;
<COND>"&&"          CREAD("&&"); return OP_AND;
<COND>"||"          CREAD("&&"); return OP_OR;
<COND>">="          CREAD("&&"); return OP_GE;
<COND>"<="          CREAD("&&"); return OP_SE;
<COND>"=="          CREAD("&&"); return OP_EQ;
<COND>[ ]+          { /* ignore whitespace */ }
<COND>{identifier}  CREAD("NAME"); yylval->text = new std::string(yytext); return NAME;
<COND>{const_num}   CREAD("NAME"); yylval->text = new std::string(yytext); return K_NUMBER;
<COND>{const_num_l} CREAD("NAME"); yylval->text = new std::string(yytext); return K_NUMBER_L;
<COND>.             return yytext[0];
<COND>"\\"\n        { /* ignore escaped newline */ }
<COND>\n            CREAD("IF_END"); BEGIN(INITIAL); return K_IF_END;

<MESSAGE_START>"\"" BEGIN(MESSAGE); return K_MESSAGE_START;
<MESSAGE_START>" "  /* ignore whitespace */
<MESSAGE_START>.    return yytext[0];
<MESSAGE>"\""       BEGIN(INITIAL); yylval->text = new std::string(yytext); return K_MESSAGE_END;
<MESSAGE>"\n"       throw std::runtime_error("preprocessor error message must end with \" in same line!");
<MESSAGE>.          yymore();
%%

std::string LanAB_Context::getYYtext()
{
    return yyget_text(scanner);
}

int LanAB_Context::getLineNo()
{
    return yyget_lineno(scanner);
}

void LanAB_Context::init_scanner()
{
	yylex_init(&scanner);
	yyset_extra(this, scanner);
}

void LanAB_Context::destroy_scanner()
{
	yylex_destroy(scanner);
}
