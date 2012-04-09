%pure-parser
%name-prefix="LanAB_"
%locations
%defines
%error-verbose
%parse-param { LanAB_Context* context }
%lex-param { void* scanner  }

%union
{
	int integer;
	char* cptr;
}

%token <cptr> TEXT NAME
%token ERR
%token IFDEF IFNDEF INCLUDE ENDLINE ENDIF ELSE DEFINE

%{
    #include <iostream>
    #include <sstream>
    #include "LanAB_Context.h"
    #include "LanCD_Context.h"

    using namespace std;

    int LanAB_lex(YYSTYPE* lvalp, YYLTYPE* llocp, void* scanner);

    void LanAB_error(YYLTYPE* locp, LanAB_Context* context, const char* err)
    {
	    cout << locp->first_line << ":" << err << endl;
    }

    #define scanner context->scanner
%}

%%

start:	code
	;

code:	code line
	|
	;

line:
	  TEXT
		{
		    if (context->level_off == 0)
			context->codefifo.push_data(std::string($1));
		}
	| IFDEF NAME ENDLINE
		{
		    context->if_def($2);
		}
	| IFNDEF NAME ENDLINE
		{
		    context->if_n_def($2);
		}
	| ELSE ENDLINE
		{
		    context->else_if();
		}
	| ENDIF ENDLINE
		{
		    context->end_if();
		}
	| DEFINE NAME ENDLINE
	    {
		context->defines.setDefine($2, "");
	    }
	| DEFINE NAME NAME ENDLINE
	    {
		context->defines.setDefine($2, $3);
	    }
	;
