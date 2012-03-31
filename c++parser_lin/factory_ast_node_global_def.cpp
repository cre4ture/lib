#include "factory_ast_node_global_def.h"
#include "ast_node_global_def_include.h"
#include "factory_ast_node.h"
#include "ast_nodes_func.hpp"

typedef ast_node_global_def* (*creatorFunc)(creax::htmlparser& xml);
static std::map<std::string, creatorFunc> lookupTable;

static ast_node_global_def* creatorFuncGD_include(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_EmptyTag)
        throw std::runtime_error("creatorFuncGD_include: empty tag expected");
    ast_node_global_def_include* inc =
            new ast_node_global_def_include(xml.getAttribute("filename"),
                                            xml.getAttribute("lib") != "0", NULL);
    return inc;
}

static ast_node_global_def* creatorFuncDG_var(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_StartTag)
        throw std::runtime_error("creatorFuncDG_var: start tag expected!");

    xml.parseToNextTag();
    ast_node_declaration_var* var = dynamic_cast<ast_node_declaration_var*>(factory_ast_node::createFromXML(xml));
    xml.parseToNextTag();
    if (xml.curTagType != creax::tt_EndTag)
        throw std::runtime_error("creatorFuncDG_var: expected end tag!");

    return new ast_node_global_def_var_def(var, NULL);
}

static ast_node_global_def* creatorFuncDG_function_def(creax::htmlparser& xml)
{
    std::string name = xml.getAttribute("name");
    std::string tpname = xml.getAttribute("resulttype");
    SymbolType* tp = dynamic_cast<SymbolType*>(symbContext->find(tpname));
    if (tp == NULL)
        throw std::runtime_error("creatorFuncDG_function_def: type not found: " + tpname);

    symbContext->addSymbol(new SymbolFunc(name, tp));
    beginNewSymbContext();

    xml.parseToNextTag();
    ast_node_parlist* parlist = dynamic_cast<ast_node_parlist*>(factory_ast_node::createFromXML(xml));
    xml.parseToNextTag();
    ast_node_statementlist* stmtlist = dynamic_cast<ast_node_statementlist*>(factory_ast_node::createFromXML(xml));

    xml.parseToNextTag();
    if (xml.curTagType != creax::tt_EndTag)
        throw std::runtime_error("creatorFuncDG_function_def: expected end tag!");

    endSymbContext();

    return new ast_node_function_def(name, tp, parlist, stmtlist, NULL);
}

ast_node_global_def* factory_ast_node_global_def::createFromXML(creax::htmlparser& xml)
{
    if (lookupTable.size() == 0)
    {
        lookupTable["globaldef_include"] = &creatorFuncGD_include;
        lookupTable["globaldef_var"] = &creatorFuncDG_var;
        lookupTable["function_def"] = &creatorFuncDG_function_def;
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
