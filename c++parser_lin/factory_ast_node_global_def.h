#ifndef FACTORY_AST_NODE_GLOBAL_DEF_H
#define FACTORY_AST_NODE_GLOBAL_DEF_H

#include "ast_node.h"
#include "ast_node_types.hpp"
#include "xmlparser/html_parser.h"

class factory_ast_node_global_def
{
private:
    factory_ast_node_global_def() {}
public:
    static ast_node_global_def* createFromXML(creax::htmlparser& xml);
};

class ast_node_global_defList: public ast_node
{
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "global_deflist");
    }

public:
    void compile_member();
    ast_node_global_defList(ast_node* parent)
        : ast_node(parent, annt_buildintypedecl)
    {}

    void parseXML(creax::htmlparser& xml)
    {
        while (xml.parse())
        {
            switch (xml.curTagType)
            {
            case creax::tt_StartTag:
            case creax::tt_EmptyTag:
                addChild(factory_ast_node_global_def::createFromXML(xml));
                break;
            case creax::tt_EndTag:
                return;
            case creax::tt_Content:
                break; // ignore (whitespsdace) content
            default:
                throw std::runtime_error("wrong xml2!");
            }
        }
    }

    size_t addChild(ast_node_global_def* gdef)
    {
        return ast_node::addChild(gdef);
    }

    int count_members()
    {
        return child_nodes.size();
    }
};


#endif // FACTORY_AST_NODE_GLOBAL_DEF_H
