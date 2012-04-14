#ifndef AST_NODE_GLOBAL_DEF_INCLUDE_H
#define AST_NODE_GLOBAL_DEF_INCLUDE_H

#include "ast_nodes_impl.hpp"
#include "factory_ast_node_global_def.h"

#include <set>
#include <map>

class ast_node_define_depencie: public ast_node
{
private:
    std::string name;
    std::string value;
    bool isset;

protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "dependencie");
        writer.addAttribute("name", name);
        writer.addAttribute("isset", isset);
        writer.addAttribute("value", value);
    }

public:
    ast_node_define_depencie(std::string a_name, std::string a_value, bool a_isSet)
        : ast_node(NULL, annt_buildintypedecl),
          name(a_name),
          value(a_value),
          isset(a_isSet)
    {}
};

class ast_node_define_depencies: public ast_node
{
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "dependencies");
    }
public:
    ast_node_define_depencies(ast_node* parent)
        : ast_node(parent, annt_buildintypedecl)
    {}

    size_t addChild(ast_node_define_depencie* depends)
    {
        return ast_node::addChild(depends);
    }
};

class ast_node_wurzel: public ast_node
{
private:
    ast_node_global_defList* defList;
    ast_node_define_depencies* define_dependencies;

protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "root");
    }

public:
    void compile();
    ast_node_wurzel(ast_node_define_depencies* dependencies, ast_node_global_defList* a_defList, ast_node* parent)
        : ast_node(parent, annt_vardecl), defList(a_defList), define_dependencies(dependencies)
    {
        addChild(dependencies);
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

ast_node_wurzel* parseCXML(const std::string& filename);
ast_node_wurzel* parseCXML(int fd, size_t size);

#endif // AST_NODE_GLOBAL_DEF_INCLUDE_H
