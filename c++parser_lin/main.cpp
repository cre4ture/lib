
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
#include "creax_filenamepath.h"
#include "ParamParser.h"

#include "cpp_parser.h"

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

int main(int argc, char *argv [])
{
    mefu::ParamParser parser(argc, argv);
    std::ostream* new_output;
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

    try {


        cpp_parser cpp(extractFilepath(in_file));
        cpp.setDefines(defines);
        cpp.parse_file(in_file);



        xmlwriter writer(*new_output);
        writer.beginTag("root");
        //if (lanCDcont.wurzel != NULL) lanCDcont.wurzel->writeToXML(writer);
        writer.endTag("root");
    } catch (std::runtime_error err) {
        fprintf(stderr,"Compilerfehler: %s\n", err.what());
        result = -1;
    }

    return result;
}


