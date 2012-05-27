%option reentrant
%option prefix="LanFS_"
%option bison-bridge
%option bison-locations
%option noyywrap
%option yylineno

%{
    #include <stdio.h>
    #include "Global.h"
    #include "Symbols.h"

    #include "ast_node_global_def_include.h"
    #include "ast_node_types.hpp"
    #include "ast_nodes_impl.hpp"
    #include "ast_nodes_flow.hpp"
    #include "ast_nodes_func.hpp"
    #include "ast_nodes_ops.hpp"
    #include "factory_ast_node_global_def.h"


    #include "LanFS_Context.h"
    #include "LanFS.tab.hpp"

    #define YY_EXTRA_TYPE LanFS_Context*
    #define YY_USER_ACTION yylloc->first_line = yylineno;

    #define YY_INPUT(buf,result,max_size) yyextra->yy_input(buf, result, max_size, YY_NULL)

    extern int zeile;
%}

delim       [ \t]
digit       [0-9]
zahl        [-]?{digit}+
identifier  [a-zA-Z_][a-zA-Z_0-9]{0,98}
pp_include  #include
pp_filename ["<][^">]*[">]$

pp_define           #define
pp_define_name      [a-zA-Z_][a-zA-Z_0-9]{0,98}
pp_define_comp      {pp_define_name}[ ][^\n]*$

%x comm inline pp_incl pp_def
%%

{delim}      	{}
"\n"         	{ }
{zahl}       	{ yylval->i = atoi(yytext); return (ZAHL);}
"<"          	{ return(CMPL); }
">"          	{ return(CMPG); }
"<="         	{ return(CMPLE); }
">="         	{ return(CMPGE); }
"=="         	{ return(CMPEQ); }
"!="         	{ return(CMPNE); }
"while"      	{ return(WHILE); }
"for"           { return(FOR); }
"if"         	{ return(IF); }
"else"       	{ return(ELSE); }
"do"        	{ return(DO); }
"return"     	{ return(KW_RETURN); }
{identifier} 	{
                    yylval->txt = new std::string(yytext);
                    return NAME;
                }
.            	{ return yytext[0]; }
%%

std::string LanFS_Context::getYYtext()
{
    return yyget_text(scanner);
}

int LanFS_Context::getLineNo()
{
    return yyget_lineno(scanner);
}

void LanFS_Context::init_scanner()
{
    yylex_init(&scanner);
	yyset_extra(this, scanner);
}

void LanFS_Context::destroy_scanner()
{
	yylex_destroy(scanner);
}
