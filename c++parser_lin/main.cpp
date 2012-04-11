
#include <iostream>

#include <stdio.h>
#include <fstream>
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

#include <iostream>
#include "LanAB_Context.h"
#include "LanCD_Context.h"

#include "creax_thread.h"

int* preprocessorThreadRoutine(LanAB_Context* param)
{
    try {
        LanAB_parse(param);
        param->preprocessor_result = 0;
    } catch (std::runtime_error& err) {
        fprintf(stderr,"preprocessor error: %s\n", err.what());
        param->preprocessor_result = -1;
    }
    param->codefifo.close_fifo();
    return NULL;
}

int* parserThreadRoutine(LanCD_Context* param)
{
    try {
        LanCD_parse(param);
        param->parser_result = 0;
    } catch (std::runtime_error& err) {
        fprintf(stderr,"parser error: %s\n", err.what());
        param->parser_result = -1;
    }
    return NULL;
}

int main(int argc, char *argv [])
{
    std::ostream* new_output;
    std::istream* new_input;
    int result = 0;

    for (int i = 0; i < argc; i++)
    {
        std::cout << "[" << i << "]: " << argv[i] << std::endl;
    }

    if ((argc <= 1)||(argc > 3)) {
        fprintf(stderr, "usage: %s <inputfile> [<outputfile>]\n", argv[0]);
        return 1;
    }

    if (argc == 3)
    {
        new_output = new std::ofstream(argv[2]);
    }
    else
    {
        new_output = &std::cout; // use stdout as destination file, change this later to write to file!
    }

    new_input = new std::ifstream(argv[1]);

    LanAB_Context context(new_input);
    LanCD_Context& lanCDcont = context.cdcontext;

    SymbolType* sym_int = new SymbolType("int");
    lanCDcont.symbContext->addSymbol(sym_int);
    SymbolFunc* buildInOp2 = new SymbolFunc("##buildInOp2", sym_int);
    lanCDcont.symbContext->addSymbol(buildInOp2);
    lanCDcont.symbContext->addSymbol(new SymbolType("void"));
    lanCDcont.symbContext->addSymbol(new SymbolType("char"));

    lanCDcont.symbContext->registerNewOperator2Function("+", sym_int, sym_int, buildInOp2);
    lanCDcont.symbContext->registerNewOperator2Function("-", sym_int, sym_int, buildInOp2);
    lanCDcont.symbContext->registerNewOperator2Function("*", sym_int, sym_int, buildInOp2);
    lanCDcont.symbContext->registerNewOperator2Function("/", sym_int, sym_int, buildInOp2);

    lanCDcont.dependencies = new ast_node_define_depencies(NULL);
    context.defines.saveDependencies(lanCDcont.dependencies);

//#define USE_THREADS

#ifdef USE_THREADS
    creax::thread<LanAB_Context, int> preprocessorThread(&preprocessorThreadRoutine, &context);
    creax::thread<LanCD_Context, int> parserThread(&parserThreadRoutine, &lanCDcont);

    preprocessorThread.join();
    parserThread.join();
#else
    preprocessorThreadRoutine(&context);
    parserThreadRoutine(&lanCDcont);
#endif

    /*std::string text;
    while (context.codefifo.pop_data(text))
    {
        std::cout << text;
    }*/

    try {
        xmlwriter writer(*new_output);
        writer.beginTag("root");
        if (lanCDcont.wurzel != NULL) lanCDcont.wurzel->writeToXML(writer);
        writer.endTag("root");
    } catch (std::runtime_error err) {
        fprintf(stderr,"Compilerfehler: %s\n", err.what());
        result = -1;
    }

    delete new_input;

    return result;
}


