#ifndef AST_NODE_GLOBAL_DEF_INCLUDE_H
#define AST_NODE_GLOBAL_DEF_INCLUDE_H

#include "ast_nodes_impl.hpp"
#include "factory_ast_node_global_def.h"

class ast_node_wurzel: public ast_node
{
private:
     ast_node_global_defList* defList;
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "wurzel");
    }
public:
    void compile();
    ast_node_wurzel(ast_node_global_defList* a_defList, ast_node* parent)
        : ast_node(parent, annt_vardecl), defList(a_defList)
    {
        addChild(a_defList);
    }
};

class ast_node_global_def_include: public ast_node_global_def
{
private:
    std::string filename;
    bool lib;

protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "globaldef_include");
        writer.addAttribute("filename", filename);
        writer.addAttribute("lib", lib);
    }
public:
    virtual void compile() {
        // TODO
    }

    ast_node_global_def_include(const std::string a_filename, const bool a_lib, ast_node* parent);
};

#endif // AST_NODE_GLOBAL_DEF_INCLUDE_H
