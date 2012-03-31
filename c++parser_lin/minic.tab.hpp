
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     ZAHL = 258,
     UNKNOWN_NAME = 259,
     PP_INCLUDE = 260,
     TYPESYMBOL = 261,
     VARSYMBOL = 262,
     FUNCSYMBOL = 263,
     ADD = 264,
     SUB = 265,
     MUL = 266,
     DIV = 267,
     END = 268,
     CMPL = 269,
     CMPG = 270,
     CMPLE = 271,
     CMPGE = 272,
     CMPEQ = 273,
     CMPNE = 274,
     ASSIGNOP = 275,
     ASSIGNOPG = 276,
     ASSIGNOPP = 277,
     ASSIGNOPPG = 278,
     WHILE = 279,
     FOR = 280,
     IF = 281,
     ELSE = 282,
     DO = 283,
     VOIDDEF = 284,
     INTDEF = 285,
     INTPDEF = 286,
     INTVECDEF = 287,
     INTVECPDEF = 288,
     INTVECARRDEF = 289,
     KW_RETURN = 290,
     KW_UNSIGNED = 291,
     KW_LONG = 292,
     KW_INT = 293,
     KW_CHAR = 294,
     FUNCTIONDEF = 295,
     FUNCTIONCALL = 296,
     PARLIST = 297,
     PAR = 298,
     PRGM = 299,
     STMTLIST = 300,
     STMT = 301,
     BLOCK = 302,
     TERM = 303,
     FAKT = 304,
     KLAMMER = 305,
     DEFLIST = 306,
     EXPR = 307,
     EXPRLIST = 308,
     ASM = 309,
     PUSH = 310,
     POP = 311,
     IMUL = 312,
     L_THEN = 313
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 37 "minic.ypp"

    int i;
    std::string *txt;
    ast_node_global_def* gDef;
    ast_node_global_defList *gDefList;
    ast_node_statement *stmt;
    ast_node_statement_value_expr* stmtExpr;
    ast_node_statementlist *stmtList;
    ast_node_value_expr *valExpr;
    ast_node_lvalue_expr *lvalExpr;
    ast_node_exprlist *exprL;
    ast_node_parlist *parL;
	ast_node_declaration_var *varDecl;
    ast_node_constIntList *constL;
	SymbolType* typeSym;
	SymbolVar* varSym;
	SymbolFunc* funcSym;



/* Line 1676 of yacc.c  */
#line 131 "minic.tab.hpp"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;


