#ifndef FACTORY_AST_NODE_H
#define FACTORY_AST_NODE_H

#include "ast_node.h"
#include "ast_node_types.hpp"
#include "xmlparser/html_parser.h"

class factory_ast_node
{
private:
    factory_ast_node() {}
public:
    static ast_node* createFromXML(creax::htmlparser& xml);
};

#endif // FACTORY_AST_NODE_H
