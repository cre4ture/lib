#ifndef AST_NODE_H
#define AST_NODE_H

#include "auto_ptr_vector.h"
#include "xmlwriter.h"

enum ast_node_node_type
{
    annt_buildintypedecl,   // no childs

    annt_funcproto, // 0: paramlist
    annt_funcimpl,  // 0: paramlist, 1: statmentlist

    annt_vardecl,   // [0: initial assignment value]

    annt_operator2, // 0: operator_l 1: operator_r
    annt_operator1, // 0: operator

    annt_expression,
    annt_statement
};

class ast_node
{
private:
    ast_node* parent_node;
    ast_node_node_type type;

protected:

    mefu::auto_ptr_vector<ast_node> child_nodes;

    virtual void writeAttributes(xmlwriter& writer) = 0;

    virtual void writeChildTags(xmlwriter& writer)
    {
        for (size_t i = 0; i < child_nodes.size(); i++)
        {
            writer.beginTag("child");
            ast_node* child = child_nodes[i];
            if (child != NULL)
                child->writeToXML(writer);
            writer.endTag("child");
        }
    }

    void setParent(ast_node* parent)
    {
        if (parent_node != NULL)
            throw std::runtime_error("ast_node::setParent(): This node already has a parent!");

        parent_node = parent;
    }

public:
    ast_node(ast_node* const a_parent_node, ast_node_node_type a_type) // NULL for root node;
        : parent_node(a_parent_node), type(a_type)
    {
        if (parent_node != NULL)
            parent_node->addChild(this);
    }

    size_t addChild(ast_node* const a_child_node)  // child node has to be created with parent=NULL
    {
        size_t index = child_nodes.size();
        if (a_child_node == NULL)
            throw std::runtime_error("can not add NULL child!");
        a_child_node->setParent(this);
        child_nodes.push_back(a_child_node);
        return index;
    }

    ast_node* operator[](size_t index)
    {
        return child_nodes[index];
    }

    ast_node_node_type getNodeType()
    {
        return type;
    }

    virtual void writeToXML(xmlwriter& writer)
    {
        writeAttributes(writer);
        writeChildTags(writer);
    }
};

#endif // AST_NODE_H
