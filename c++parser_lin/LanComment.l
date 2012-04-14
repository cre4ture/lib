%option reentrant
%option prefix="LanComment_"
%option bison-bridge
%option bison-locations
%option noyywrap
%option yylineno

%{
    #include "LanComment_Context.h"
    #include "LanComment.tab.hpp"

    #define YY_EXTRA_TYPE LanComment_Context*
	#define YY_USER_ACTION yylloc->first_line = yylineno;

    void ab_input(char* const  buf, int& result, const int max_size, LanComment_Context* const context)
    {
        if(context->is.eof())
            result = YY_NULL;
        else {
            result = context->is.readsome(buf, max_size);
        }
    }

    #define YY_INPUT(buf,result,max_size) ab_input(buf, result, max_size, yyextra)

    //#define CREAD(TEXT) std::cout << TEXT << std::endl
    #define CREAD(TEXT)

%}

identifier      [a-zA-Z_][a-zA-Z0-9_]+

%x LINE BLOCK

%%

<INITIAL>"//"       CREAD("START CommentLine"); yylval->text = new std::string(yytext, strlen(yytext)-2); BEGIN(LINE); return TEXT;
<INITIAL>"/*"       CREAD("START CommentBlock"); yylval->text = new std::string(yytext, strlen(yytext)-2); BEGIN(BLOCK); return TEXT;
<INITIAL>[^/]+      CREAD("TEXT"); yylval->text = new std::string(yytext); return TEXT;
<INITIAL>.          CREAD("TEXT"); yylval->text = new std::string(yytext); return TEXT;

<LINE>"\\\n"        CREAD("ESC ENDLINE"); yymore();
<LINE>"\n"          CREAD("ENDLINE"); yylval->text = new std::string(yytext, strlen(yytext)-1); BEGIN(INITIAL); return LINECOMMENT;
<LINE>[^\\\n]+      CREAD("lcom yymore()"); yymore();
<LINE>.             CREAD("lcom yymore()"); yymore();
<BLOCK>"*/"         CREAD("ENDBLOCK"); yylval->text = new std::string(yytext); BEGIN(INITIAL); return BLOCKCOMMENT;
<BLOCK>[^*]+        CREAD("bcom yymore()"); yymore();
<BLOCK>.            CREAD("bcom yymore()"); yymore();
<BLOCK><<EOF>>      CREAD("B: ENDE"); throw std::runtime_error("end of file reached before end of block comment!");

%%

void LanComment_Context::init_scanner()
{
	yylex_init(&scanner);
	yyset_extra(this, scanner);
}

void LanComment_Context::destroy_scanner()
{
	yylex_destroy(scanner);
}
