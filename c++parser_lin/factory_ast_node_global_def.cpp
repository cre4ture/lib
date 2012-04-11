#include "factory_ast_node_global_def.h"
#include "ast_node_global_def_include.h"
#include "factory_ast_node.h"
#include "ast_nodes_func.hpp"

typedef ast_node_global_def* (*creatorFunc)(creax::htmlparser& xml);
static std::map<std::string, creatorFunc> lookupTable;


ast_node_global_def* factory_ast_node_global_def::createFromXML(creax::htmlparser& xml)
{
    if (lookupTable.size() == 0)
    {

    }

    std::string type = xml.getAttribute("type");
    creatorFunc func = lookupTable[type];
    ast_node_global_def* result;

    if (func != NULL)
    {
        result = (*func)(xml);
    }
    else
    {
        throw std::runtime_error("factory_ast_node_global_def: no entry for type: " + type);
    }
    return result;
}
