#ifndef AST_NODES_OPS_HPP
#define AST_NODES_OPS_HPP

#include <string>
#include <stdexcept>
#include "ast_node_types.hpp"
#include "ast_nodes_impl.hpp"

class ast_node_op: public ast_node_value_expr
{
protected:
    std::string name;
    ast_node_value_expr* const op1;
    ast_node_value_expr* const op2;
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "op2");
        writer.addAttribute("name", name);
    }
public:
    virtual void compile_value();
    ast_node_op(std::string a_name, ast_node_value_expr* a_op1, ast_node_value_expr* a_op2, ast_node* parent)
        : ast_node_value_expr(parent), name(a_name), op1(a_op1), op2(a_op2)
    {
        addChild(a_op1);
        addChild(a_op2);
    }
};

#endif // AST_NODES_OPS_HPP
