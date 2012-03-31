#ifndef AST_NODES_FLOW_H
#define AST_NODES_FLOW_H

#include "ast_node_types.hpp"
#include "ast_nodes_impl.hpp"

class ast_node_while: public ast_node_statement
{
private:
    ast_node_value_expr* condition;
    ast_node_statement* body;
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "while");
    }
public:
    virtual void compile();
    ast_node_while(ast_node_value_expr* a_condition, ast_node_statement* a_body, ast_node* parent)
        : ast_node_statement(parent), condition(a_condition), body(a_body)
    {
        addChild(a_condition);
        addChild(a_body);
    }
};

class ast_node_for: public ast_node_statement
{
private:
    ast_node_statement* initial;
    ast_node_value_expr* condition;
    ast_node_statement* assign;
    ast_node_statement* body;
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "for");
    }
public:
    virtual void compile();
    ast_node_for(ast_node_statement* a_initial, ast_node_value_expr* a_condition, ast_node_statement_value_expr* a_assign, ast_node_statement* a_body, ast_node* parent)
        : ast_node_statement(parent), initial(a_initial), condition(a_condition), assign(a_assign), body(a_body)
    {
        addChild(a_initial);
        addChild(a_condition);
        addChild(a_assign);
        addChild(a_body);
    }
};

class ast_node_do: public ast_node_statement
{
private:
    ast_node_value_expr* condition;
    ast_node_statement* body;
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "do");
    }
public:
    virtual void compile();
    ast_node_do(ast_node_value_expr* a_condition, ast_node_statement* a_body, ast_node* parent)
        : ast_node_statement(parent), condition(a_condition), body(a_body)
    {
        addChild(a_condition);
        addChild(a_body);
    }
};

class ast_node_if_else: public ast_node_statement
{
private:
    ast_node_value_expr* condition;
    ast_node_statement* if_body;
    ast_node_statement* else_body;
protected:
    virtual void writeAttributes(xmlwriter& writer)
    {
        writer.addAttribute("type", "if");
    }

    virtual void writeChildTags(xmlwriter& writer)
    {
        writer.beginTag("condition");
        condition->writeToXML(writer);
        writer.endTag("condition");
        writer.beginTag("if_body");
        if_body->writeToXML(writer);
        writer.endTag("if_body");
        if (else_body != NULL)
        {
            writer.beginTag("else_body");
            else_body->writeToXML(writer);
            writer.endTag("else_body");
        }
    }

public:
    virtual void compile();
    ast_node_if_else(ast_node_value_expr* a_condition, ast_node_statement* a_if_body, ast_node_statement* a_else_body, ast_node* parent)
        : ast_node_statement(parent), condition(a_condition), if_body(a_if_body), else_body(a_else_body)
    {
        addChild(a_condition);
        addChild(a_if_body);
        if (a_else_body != NULL)
        {
            addChild(a_else_body);
        }
    }
};

#endif // AST_NODES_FLOW_H
