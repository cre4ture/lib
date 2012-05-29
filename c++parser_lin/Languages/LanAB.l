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

%x CMD DEFINE_NAME MACRO_PARAMS DEFINE_VALUE

%%

"#"                 CREAD("START CMD"); yymore(); BEGIN(CMD);
{identifier}        CREAD("IDENT"); yylval->text = new std::string(yytext); return K_IDENT;
[\n]+               CREAD("NEWLINE"); yylval->text = new std::string(yytext); return TEXT;
{stuff}             CREAD("<stuff>"); yylval->text = new std::string(yytext); return TEXT;
{some_number}       CREAD("<some_number>"); yylval->text = new std::string(yytext); return TEXT;
.                   CREAD("<unknown_text>"); CREAD(yytext); yylval->text = new std::string(yytext); return TEXT;

<CMD>"ifdef"        CREAD("IFDEF");  return IFDEF;
<CMD>"ifndef"       CREAD("IFNDEF"); return IFNDEF;
<CMD>"else"         CREAD("ELSE");   return ELSE;
<CMD>"endif"        CREAD("ENDIF");  return ENDIF;
<CMD>"include"[ ]*  CREAD("INCLUDE; CMD END"); yymore(); BEGIN(INITIAL);
 <CMD>"define"[ ]*  CREAD("START DEFINE"); BEGIN(DEFINE_NAME);
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
