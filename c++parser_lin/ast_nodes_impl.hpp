#ifndef AST_NODES_IMPL_HPP
#define AST_NODES_IMPL_HPP
#include <string>
#include <stdexcept>
#include "ast_node_types.hpp"
#include "xmlparser/html_parser.h"

class ast_node_type: public ast_node
{
private:
    std::string name;
    int pointerLevel;

protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "type");
        writer.addAttribute("name", name);
        writer.addAttribute("pointerlevel", pointerLevel);
    }

public:
    ast_node_type(const std::string& a_name, const int a_pointerLevel, ast_node* parent)
        : ast_node(parent, annt_buildintypedecl), name(a_name), pointerLevel(a_pointerLevel)
    {}

    ast_node_type(SymbolType* a_type, ast_node* parent)
        : ast_node(parent, annt_buildintypedecl),
          name(a_type->pointerBaseType()->getName()),
          pointerLevel(a_type->pointerLevel())
    {}
};

class ast_node_declaration_var;

class ast_node_global_def_var_def: public ast_node_global_def
{
private:
    ast_node_declaration_var* varDef;
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "globaldef_var");
    }
public:
    virtual void compile();
    ast_node_global_def_var_def(ast_node_declaration_var* a_varDef, ast_node* parent);
};

class ast_node_stmtBlock: public ast_node_statement
{
private:
    ast_node_statement* body;
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "stmtblock");
    }
public:
    virtual void compile();
    ast_node_stmtBlock(ast_node_statement* a_body, ast_node* parent)
        : ast_node_statement(parent), body(a_body)
    {
        addChild(a_body);
    }
};

class ast_node_statement_value_expr: public ast_node_statement
{
private:
    ast_node_value_expr* expr;
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "statement_value_expr");
    }

public:
    virtual void compile();
    ast_node_statement_value_expr(ast_node_value_expr* a_expr, ast_node* parent)
        : ast_node_statement(parent), expr(a_expr)
    {
        addChild(a_expr);
    }
};

class ast_node_statement_var_def: public ast_node_statement
{
private:
    ast_node_declaration_var* varDef;
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "statement_var_def");
    }

public:
    virtual void compile();
    ast_node_statement_var_def(ast_node_declaration_var* a_varDef, ast_node* parent);
};

class ast_node_statementlist: public ast_node_statement
{
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "statementlist");
    }

public:
    virtual void compile();
    ast_node_statementlist(ast_node* parent)
        : ast_node_statement(parent)
    {}

    size_t addChild(ast_node_statement* stmt)
    {
        return ast_node::addChild(stmt);
    }
};

class ast_node_constIntList: public ast_node
{
private:
    ast_node_constIntList* constList;
    int constInt;

protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "constIntList");
    }

public:
    virtual void compile_value();
    ast_node_constIntList(ast_node_constIntList* a_constList, int a_constInt, ast_node* parent)
        : ast_node(parent, annt_buildintypedecl), constList(a_constList), constInt(a_constInt)
    {}

    int count_members()
    {
        int count = 0;
        if(constList != NULL) count = constList->count_members();
        count++;
        return count;
    }
};

class ast_node_declaration_var: public ast_node
{
private:
    std::string name;
    ast_node_type* type;
    ast_node_constIntList* initValue;
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "declaration_var");
        writer.addAttribute("name", name);
    }
public:
    void compile_decl(bool isGlobal);
    ast_node_declaration_var(const std::string& _name, ast_node_type* _type, ast_node_constIntList* a_initValue, ast_node* parent);
};

class ast_node_array_declaration: public ast_node_declaration_var
{
private:
    int size;
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "array_declaration");
    }
public:
    virtual void compile_value();
    ast_node_array_declaration(const char* _name, ast_node_type* _type, int _size, ast_node* parent)
        : ast_node_declaration_var(_name, _type, NULL, parent)
    {
        size = _size;
    }
};

class ast_node_identifier: public ast_node_lvalue_expr
{
private:
    SymbolVar* var;

protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "identifier");
        writer.addAttribute("name", var->getName());
    }
public:
    virtual void compile_value();
    virtual void compile_address();

    std::string getName()
    {
        return var->getName();
    }

    ast_node_identifier(SymbolVar* a_var, ast_node* parent)
        : ast_node_lvalue_expr(parent), var(a_var)
    {
        // addChild(a_var);
    }
};

class ast_node_deref_op: public ast_node_lvalue_expr
{
private:
    ast_node_value_expr* pointerExpr;

    static SymbolType* getTargetPointer(SymbolType* typ)
    {
        if (typ->isPointer() == false)
            throw std::runtime_error("derefer non pointer type!");
        return ((SymbolTypePtr*)typ)->getTargetType();
    }
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "deref_op");
    }

public:
    virtual void compile_value();
    virtual void compile_address();
    ast_node_deref_op(ast_node_value_expr* a_pointerExpr, ast_node* parent)
        : ast_node_lvalue_expr(parent), pointerExpr(a_pointerExpr)
    {
        addChild(a_pointerExpr);
    }
};

class ast_node_addr_op: public ast_node_value_expr
{
private:
    ast_node_lvalue_expr* lvalue;
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "addr_op");
    }

public:
    virtual void compile_value();
    ast_node_addr_op(ast_node_lvalue_expr* a_lvalue, ast_node* parent)
        : ast_node_value_expr(parent), lvalue(a_lvalue)
    {
        addChild(a_lvalue);
    }
};

class ast_node_constant_int: public ast_node_value_expr
{
private:
    int const_value;
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "constant_int");
        writer.addAttribute("value", const_value);
    }
public:
    virtual void compile_value();

    int getIntValue()
    {
        return const_value;
    }

    ast_node_constant_int(int a_value, ast_node* parent)
        : ast_node_value_expr(parent), const_value(a_value)
    {
    }
};

class ast_node_assignment: public ast_node_value_expr
{
private:
    ast_node_lvalue_expr* dest;
    ast_node_value_expr* src;

    void compile_value_int();
    void compile_value_vec();
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "assignment");
    }

public:
    virtual void compile_value();
    ast_node_assignment(ast_node_lvalue_expr* a_dest, ast_node_value_expr* a_src, ast_node* parent)
        : ast_node_value_expr(parent), dest(a_dest), src(a_src)
    {
        addChild(a_dest);
        addChild(a_src);
    }
};

inline ast_node_global_def_var_def::ast_node_global_def_var_def(ast_node_declaration_var* a_varDef, ast_node* parent)
    : ast_node_global_def(parent), varDef(a_varDef)
{
    addChild(a_varDef);
}

inline ast_node_statement_var_def::ast_node_statement_var_def(ast_node_declaration_var* a_varDef, ast_node* parent)
    : ast_node_statement(parent), varDef(a_varDef)
{
    addChild(a_varDef);
}

#endif // AST_NODES_IMPL_HPP
