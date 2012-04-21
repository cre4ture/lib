%option reentrant
%option prefix="LanCF_"
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


    #include "LanCF_Context.h"
    #include "LanCF.tab.hpp"

    #define YY_EXTRA_TYPE LanCF_Context*
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

%x comm inline pp_incl pp_def BLOCK
%%

{pp_include}    { BEGIN pp_incl; }
<pp_incl>{pp_filename}      {
                                yylval->txt = new std::string(yytext);
                                BEGIN INITIAL;
                                return PP_INCLUDE;
                            }

{pp_define}     { BEGIN pp_def; }
<pp_def>"\n"                { BEGIN INITIAL; }
{delim}      	{}
"\n"         	{}
<INITIAL>"namespace"            { return NAMESPACE; }
<INITIAL>"class"            { return CLASS; }
<INITIAL>"struct"            { return STRUCT; }
<INITIAL>"private"            { return PRIVATE; }
<INITIAL>"public"            { return PUBLIC; }
<INITIAL>"protected"            { return PROTECTED; }
<INITIAL>"{"    { BEGIN(BLOCK); yyextra->level = 1; return BLOCK_START; }
<BLOCK>[^{}]+    { yymore(); }
<BLOCK>"{"       { yyextra->level++; yymore(); }
<BLOCK>"}"                       {
                                    yyextra->level--;
                                    if (yyextra->level == 0)
                                    {
                                        BEGIN(INITIAL);
                                        yylval->txt = new std::string(yytext, strlen(yytext)-1);
                                        return BLOCK_END;
                                    }
                                    else
                                    {
                                        yymore();
                                    }
                                }
{zahl}       	{ yylval->i = atoi(yytext); return ZAHL;}
{identifier} 	{
                    yylval->txt = new std::string(yytext);
                    return NAME;
                }
"/*"         	{ BEGIN comm; }
<comm>"*/"   	{ BEGIN INITIAL; }
<comm>.|\n   	{ /* skip */ }
.            	{ return yytext[0]; }
%%

std::string LanCF_Context::getYYtext()
{
    return yyget_text(scanner);
}

int LanCF_Context::getLineNo()
{
    return yyget_lineno(scanner);
}

void LanCF_Context::init_scanner()
{
    yylex_init(&scanner);
	yyset_extra(this, scanner);
}

void LanCF_Context::destroy_scanner()
{
	yylex_destroy(scanner);
}