%option reentrant
%option prefix="LanAB_"
%option bison-bridge
%option bison-locations
%option noyywrap
%option yylineno

%{
	#include "LanAB_Context.h"
	#include "LanAB.tab.h"

	#define YY_EXTRA_TYPE LanAB_Context*
	#define YY_USER_ACTION yylloc->first_line = yylineno;

    void ab_input(char* const  buf, int& result, const int max_size, LanAB_Context* const context)
    {
        if(context->is->eof())
            result = YY_NULL;
        else {
            result = context->is->readsome(buf, max_size);
        }
    }

    #define YY_INPUT(buf,result,max_size) ab_input(buf, result, max_size, yyextra)

    //#define CREAD(TEXT) std::cout << TEXT << std::endl
    #define CREAD(TEXT)

%}

identifier      [a-zA-Z_][a-zA-Z0-9_]+

%x CMD

%%

"#"                 CREAD("START CMD"); yymore(); BEGIN(CMD);
\n                  CREAD("NEWLINE"); yylval->text = new std::string(yytext); return TEXT;
.                   CREAD("yymore()"); yymore();

<CMD>"ifdef"        CREAD("IFDEF");  return IFDEF;
<CMD>"ifndef"       CREAD("IFNDEF"); return IFNDEF;
<CMD>"else"         CREAD("ELSE");   return ELSE;
<CMD>"endif"        CREAD("ENDIF");  return ENDIF;
<CMD>"include"      CREAD("INCLUDE; CMD END"); yymore(); BEGIN(INITIAL);
<CMD>"define"       CREAD("DEFINE"); return DEFINE;
<CMD>" "            /* ignore whitespace */
<CMD>"\t"           /* ignore whitespace */
<CMD>"\n"           CREAD("CMD END"); BEGIN(INITIAL); return ENDLINE;
<CMD>{identifier}   CREAD("NAME"); yylval->text = new std::string(yytext); return NAME;
%%

void LanAB_Context::init_scanner()
{
	yylex_init(&scanner);
	yyset_extra(this, scanner);
}

void LanAB_Context::destroy_scanner()
{
	yylex_destroy(scanner);
}
