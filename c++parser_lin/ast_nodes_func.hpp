#ifndef AST_NODES_FUNC_HPP
#define AST_NODES_FUNC_HPP
#include <string>
#include "ast_node_types.hpp"
#include "ast_nodes_impl.hpp"

class ast_node_parlist: public ast_node
{
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "parlist");
    }

public:
    virtual void compile_value();
    ast_node_parlist(ast_node* parent)
        :ast_node(parent, annt_buildintypedecl)
    {}

    size_t addChild(ast_node_declaration_var* decl)
    {
        return ast_node::addChild(decl);
    }

    int count_members()
    {
        return child_nodes.size();
    }
};

class ast_node_function_def: public ast_node_global_def
{
private:
    std::string name;
    SymbolType* type;
    ast_node_parlist* parlist;
    ast_node_statement* stmtList;
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "function_def");
        writer.addAttribute("name", name);
        writer.addAttribute("resulttype", type->getName());
    }
public:
    virtual void compile();
    ast_node_function_def(std::string a_name, SymbolType* a_type,
                          ast_node_parlist* a_parlist, ast_node_statement* a_stmtList, ast_node* parent)
        : ast_node_global_def(parent), name(a_name), type(a_type), parlist(a_parlist), stmtList(a_stmtList)
    {
        addChild(a_parlist);
        addChild(a_stmtList);
    }
};

class ast_node_expression; // prototype
class ast_node_exprlist; // prototype

class ast_node_functioncall: public ast_node_value_expr
{
private:
    SymbolFunc* func;
    ast_node_exprlist* exprlist;
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "functioncall");
        writer.addAttribute("name", func->getName());
    }
public:
    virtual void compile_value();
    ast_node_functioncall(SymbolFunc* a_func, ast_node_exprlist* a_exprlist, ast_node* parent);
};

class ast_node_term; // prototype

class ast_node_expression: public ast_node_value_expr
{
private:
    ast_node_value_expr* term;
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "expression");
    }
public:
    virtual void compile_value();
    ast_node_expression(ast_node_value_expr* a_term, ast_node* parent)
        : ast_node_value_expr(parent), term(a_term)
    {
        addChild(a_term);
    }
};

class ast_node_exprlist: public ast_node
{
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "exprlist");
    }

public:
    virtual void compile_value();
    ast_node_exprlist(ast_node* parent)
        : ast_node(parent, annt_buildintypedecl)
    {}

    size_t addChild(ast_node_expression* expr)
    {
        return ast_node::addChild(expr);
    }

    int count_members()
    {
        return child_nodes.size();
    }
};

class ast_node_return: public ast_node_statement
{
private:
    ast_node_value_expr* expression;

protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "return");
    }

public:
    virtual void compile();
    ast_node_return(ast_node_value_expr* _expression, ast_node* parent)
        : ast_node_statement(parent)
    {
        expression = _expression;
        addChild(_expression);
    }
};

inline ast_node_functioncall::ast_node_functioncall(SymbolFunc* a_func, ast_node_exprlist* a_exprlist, ast_node* parent)
    : ast_node_value_expr(parent), func(a_func), exprlist(a_exprlist)
{
    addChild(a_exprlist);
}

#endif // AST_NODES_FUNC_HPP
