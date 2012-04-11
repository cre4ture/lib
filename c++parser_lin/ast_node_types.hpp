#ifndef AST_NODE_HPP
#define AST_NODE_HPP

#include <cstdio>
#include <string>
#include "Symbols.h"
#include "ast_node.h"

class ast_node_global_def: public ast_node
{
public:
    ast_node_global_def(ast_node* parent)
        : ast_node(parent, annt_vardecl)
    {}
    virtual void compile() = 0;
};

class ast_node_statement: public ast_node
{
public:
    ast_node_statement(ast_node* parent)
        : ast_node(parent, annt_statement)
    {}
    // erzeugt beliebigen code
    virtual void compile() = 0;
};

class ast_node_value_expr: public ast_node
{
public:
    // legt den Rückgabewert ins Register A bzw. R0
    virtual void compile_value() = 0;

    // virtual SymbolType* getTypeOf() = 0;

    ast_node_value_expr(ast_node* parent)
        : ast_node(parent, annt_expression)
    {}
};

class ast_node_lvalue_expr: public ast_node_value_expr
{
	public:
		// legt die Addresse der lvalue ins Register A
		virtual void compile_address() = 0;
        ast_node_lvalue_expr(ast_node* parent)
            : ast_node_value_expr(parent) {}
};

#endif
