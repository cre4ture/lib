%{
/***************************************************
  Datei   minic.l
  LEX-Eingabe fuer vereinfachtes C
 ***************************************************/ 
#include "ast_node_types.hpp"
#include "ast_nodes_flow.hpp"
#include "ast_nodes_impl.hpp"
#include "ast_nodes_ops.hpp"
#include "ast_nodes_func.hpp"
#include "Symbols.h"
#include "factory_ast_node_global_def.h"
#include "ast_node_global_def_include.h"
#include "minic.tab.hpp"

extern int zeile;
%}

delim       [ \t]
digit       [0-9]
zahl        [-]?{digit}+
identifier  [a-zA-Z_][a-zA-Z_0-9]{0,98}
pp_include  #include
pp_filename ["<][^">]*[">]$


%x comm inline pp_incl
%%

{pp_include}    { BEGIN pp_incl; }
<pp_incl>{pp_filename}      {
                            yylval.txt = new std::string(yytext);
                            BEGIN INITIAL;
                            return PP_INCLUDE;
                            }
{delim}      	{}
"\n"         	{ zeile++;}
{zahl}       	{ yylval.i = atoi(yytext); return (ZAHL);}
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
					Symbol* symb = symbContext->find(yytext);
					TypeOfSymbol t;
					if (symb != NULL)
                        t = symb->getSymbolType();
					else
						t = st_none;
					switch (t) {
					case st_none:
						yylval.txt = new std::string(yytext);
						return UNKNOWN_NAME;
					case st_type:
						yylval.typeSym = (SymbolType*)symb;
						return TYPESYMBOL;
					case st_var:
						yylval.varSym = (SymbolVar*)symb;
						return VARSYMBOL;
					case st_func:
						yylval.funcSym = (SymbolFunc*)symb;
						return FUNCSYMBOL;
               		}
             	}
"/*"         	{ BEGIN comm; }
<comm>"*/"   	{ BEGIN INITIAL; }
<comm>.|\n   	{ /* skip */ }
.            	{ return yytext[0]; }
%%

int yywrap() {
	return 1;
};
