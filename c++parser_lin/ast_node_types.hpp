#ifndef AST_NODE_HPP
#define AST_NODE_HPP

#include <cstdio>
#include <string>
#include "Symbols.h"
#include "ast_node.h"

extern FILE* compile_output; // default output file

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
private:
    SymbolType* const type;
public:
    // legt den Rückgabewert ins Register A bzw. R0
    virtual void compile_value() = 0;

    SymbolType* getTypeOf()
    {
        return type;
    }

    ast_node_value_expr(SymbolType* a_type, ast_node* parent)
        : ast_node(parent, annt_expression), type(a_type)
    {}
};

class ast_node_lvalue_expr: public ast_node_value_expr
{
	public:
		// legt die Addresse der lvalue ins Register A
		virtual void compile_address() = 0;
        ast_node_lvalue_expr(SymbolType* a_type, ast_node* parent)
            : ast_node_value_expr(a_type, parent) {}
};

#endif
