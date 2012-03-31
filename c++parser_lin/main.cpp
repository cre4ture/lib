
#include <iostream>

#include <stdio.h>
#include "Global.h"
#include "Symbols.h"
#include "xmlwriter.h"

#include "ast_node_types.hpp"
#include "ast_nodes_impl.hpp"
#include "ast_nodes_flow.hpp"
#include "ast_nodes_func.hpp"
#include "ast_nodes_ops.hpp"
#include "factory_ast_node_global_def.h"
#include "ast_node_global_def_include.h"


ast_node_wurzel *wurzel;

FILE *ausgabe;
extern FILE *yyin;
FILE* compile_output;

int main(int argc, char *argv [])
{
    int result = 0;

    if (argc != 2) {
        fprintf(stderr, "usage: %s <inputfile> \n", argv[0]);
        return 1;
    }

    compile_output = stdout; // use stdout as destination file, change this later to write to file!

    symbContext->addSymbol(new SymbolType("*")); // pointer
    SymbolType* sym_int = new SymbolType("int");
    symbContext->addSymbol(sym_int);
    SymbolFunc* buildInOp2 = new SymbolFunc("##buildInOp2", sym_int);
    symbContext->addSymbol(buildInOp2);
    symbContext->addSymbol(new SymbolType("void"));
    symbContext->addSymbol(new SymbolType("int_vec"));

    symbContext->registerNewOperator2Function("+", sym_int, sym_int, buildInOp2);
    symbContext->registerNewOperator2Function("-", sym_int, sym_int, buildInOp2);
    symbContext->registerNewOperator2Function("*", sym_int, sym_int, buildInOp2);
    symbContext->registerNewOperator2Function("/", sym_int, sym_int, buildInOp2);

    yyin = fopen(argv[1], "r");
    ausgabe = fopen("zcode", "w");
    if (yyin == NULL || ausgabe == NULL) {
        fprintf(stderr, "Could not open file\n");
        exit(1);
    }

    try {
        startParser();
    } catch (std::runtime_error err) {
        yyerror(err.what());
        result = -1;
    }

    if (result == 0)
    {
        try {
            xmlwriter writer(std::cout);
            writer.beginTag("root");
            if (wurzel != NULL) wurzel->writeToXML(writer);
            writer.endTag("root");
        } catch (std::runtime_error err) {
            fprintf(stderr,"Compilerfehler: %s\n", err.what());
            result = -1;
        }
    }

    fclose(yyin);
    fclose(ausgabe);

    return result;
}


