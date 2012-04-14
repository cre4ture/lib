
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
#include "Languages/LanAB_Context.h"
#include "Languages/LanCF_Context.h"

#include "creax_thread.h"
#include "ParamParser.h"

void parseDefines(std::map<std::string, std::string>& defines, mefu::ParamParser& parser)
{
    std::string value = parser.getStringVal("defines","");
    std::vector<std::string> definelist;
    mefu::readStringListFromString(value,definelist,';');

    for (size_t i = 0; i < definelist.size(); i++)
    {
        std::vector<std::string> name_value;
        mefu::readStringListFromString(definelist[i],name_value,'=');
        switch (name_value.size())
        {
        case 1:
            defines[name_value[0]] = "";
            break;
        case 2:
            defines[name_value[0]] = name_value[1];
            break;
        default:
            throw std::runtime_error("invalid definelist parameter!");
        }
    }
}

int* commentFilterThreadRoutine(LanComment_Context* param)
{
    try {
        LanComment_parse(param);
        param->result = 0;
    } catch (std::runtime_error& err) {
        fprintf(stderr,"comment filter error: %s\n", err.what());
        param->result = -1;
    }
    param->fifo.close_fifo();
    return NULL;
}

int* preprocessorThreadRoutine(LanAB_Context* param)
{
    try {
        LanAB_parse(param);
        param->preprocessor_result = 0;
    } catch (std::runtime_error& err) {
        fprintf(stderr,"preprocessor error: %s\n", err.what());
        param->preprocessor_result = -1;
    }
    param->output_fifo.close_fifo();
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

int* blockParserThreadRoutine(LanCF_Context* param)
{
    try {
        LanCF_parse(param);
        param->parser_result = 0;
    } catch (std::runtime_error& err) {
        fprintf(stderr,"block parser error: %s\n", err.what());
        param->parser_result = -1;
    }
    return NULL;
}

int main(int argc, char *argv [])
{
    mefu::ParamParser parser(argc, argv);
    std::ostream* new_output;
    std::ifstream* new_input;
    int result = 0;

    std::string in_file  = parser.getStringVal("in","");
    std::string out_file = parser.getStringVal("out","");

    if (in_file == "") {
        fprintf(stderr, "usage: %s in=\"<inputfile>\" [out=\"<outputfile>\"] [defines=\"<define>[=<value>][;<define2>...]\"]\n", argv[0]);
        return 1;
    }

    if (out_file != "")
        new_output = new std::ofstream(out_file.c_str());
    else
        new_output = &std::cout;

    std::map<std::string,std::string> defines;
    parseDefines(defines, parser);

    new_input = new std::ifstream(in_file.c_str());

    if (!new_input->is_open())
    {
        std::cerr << "could not open file: " << in_file << std::endl;
        return -1;
    }

    creax::threadfifo<text_type> stage1_2;
    creax::threadfifo<code_piece> stage2_3;

    // stage 1
    LanComment_Context lanComment_context(*new_input, stage1_2);
    // stage 2
    LanAB_Context lanAB_context(stage1_2, stage2_3);
    // stage 3
    LanCF_Context lanCFcont(stage2_3);

    //lanAB_context.setCDContext(&lanCDcont);
    lanAB_context.defines.loadDefines(defines);

    lanCFcont.dependencies = new ast_node_define_depencies(NULL);
    lanAB_context.defines.saveDependencies(lanCFcont.dependencies);

//#define USE_THREADS

#ifdef USE_THREADS
    creax::thread<LanComment_Context, int> commentFilterThread(&preprocessorThreadRoutine, &lanComment_context);
    creax::thread<LanAB_Context, int> preprocessorThread(&preprocessorThreadRoutine, &lanAB_context);
    creax::thread<LanCD_Context, int> parserThread(&parserThreadRoutine, &lanCDcont);

    commentFilterThread.join();
    preprocessorThread.join();
    parserThread.join();
#else
    commentFilterThreadRoutine(&lanComment_context);
    preprocessorThreadRoutine(&lanAB_context);
    //parserThreadRoutine(&lanCDcont);
    blockParserThreadRoutine(&lanCFcont);
#endif

    /*std::string text;
    while (context.codefifo.pop_data(text))
    {
        std::cout << text;
    }*/

    try {
        xmlwriter writer(*new_output);
        writer.beginTag("root");
        //if (lanCDcont.wurzel != NULL) lanCDcont.wurzel->writeToXML(writer);
        writer.endTag("root");
    } catch (std::runtime_error err) {
        fprintf(stderr,"Compilerfehler: %s\n", err.what());
        result = -1;
    }

    delete new_input;

    return result;
}


