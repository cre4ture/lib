#include "factory_ast_node.h"
#include "ast_nodes_impl.hpp"
#include "ast_nodes_func.hpp"
#include "ast_nodes_ops.hpp"
#include "ast_nodes_flow.hpp"

typedef ast_node* (*creatorFuncAN)(creax::htmlparser& xml);
static std::map<std::string, creatorFuncAN> lookupTable;

#define THROW_RUNTIME_ERROR(MSG) throw std::runtime_error(std::string(__PRETTY_FUNCTION__) + ": " + MSG)

static ast_node* creatorFuncAN_declaration_var(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_EmptyTag)
        THROW_RUNTIME_ERROR("empty tag expected!");

    std::string tpname = xml.getAttribute("vartype");
    std::string name = xml.getAttribute("name");
    int pointerLevel = xml.getAttributeInt("pointerlevel");
    SymbolType* tp = dynamic_cast<SymbolType*>(symbContext->find(tpname));
    if (tp == NULL)
        THROW_RUNTIME_ERROR("unknown type: " + tpname);

    for (int i = 0; i < pointerLevel; i++)
    {
        tp = new SymbolTypePtr(tp);
    }

    SymbolVar* varSym = new SymbolVar(name, tp, symbContext->getParent() == NULL);
    symbContext->addSymbol(varSym);

    ast_node_declaration_var* var =
            new ast_node_declaration_var(name,
                                         tp, NULL, NULL);
    return var;
}

static ast_node* creatorFuncAN_assignment(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_StartTag)
        THROW_RUNTIME_ERROR("start tag expected!");

    xml.parseToNextTag();
    ast_node_lvalue_expr* lvalue =
            dynamic_cast<ast_node_lvalue_expr*>(factory_ast_node::createFromXML(xml));

    xml.parseToNextTag();
    ast_node_value_expr* value =
            dynamic_cast<ast_node_value_expr*>(factory_ast_node::createFromXML(xml));

    if (lvalue == NULL)
        THROW_RUNTIME_ERROR("failed to cast lvalue!");
    if (value == NULL)
        THROW_RUNTIME_ERROR("failed to cast value!");

    xml.parseToNextTag();
    if (xml.curTagType != creax::tt_EndTag)
        THROW_RUNTIME_ERROR("expected end tag!");

    return new ast_node_assignment(lvalue, value, NULL);
}

static ast_node* creatorFuncAN_identifier(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_EmptyTag)
        THROW_RUNTIME_ERROR("empty tag expected!");

    std::string name = xml.getAttribute("name");
    SymbolVar* id = dynamic_cast<SymbolVar*>(symbContext->find(name));

    if (id == NULL)
        THROW_RUNTIME_ERROR("identifier not found: " + name);

    return new ast_node_identifier(id, NULL);
}

static ast_node* creatorFuncAN_op2(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_StartTag)
        THROW_RUNTIME_ERROR("start tag expected!");

    std::string name = xml.getAttribute("name");

    xml.parseToNextTag();
    ast_node_value_expr* op1 =
            dynamic_cast<ast_node_value_expr*>(factory_ast_node::createFromXML(xml));

    xml.parseToNextTag();
    ast_node_value_expr* op2 =
            dynamic_cast<ast_node_value_expr*>(factory_ast_node::createFromXML(xml));

    if (op1 == NULL)
        THROW_RUNTIME_ERROR("failed to cast op1!");
    if (op2 == NULL)
        THROW_RUNTIME_ERROR("failed to cast op2!");

    xml.parseToNextTag();
    if (xml.curTagType != creax::tt_EndTag)
        THROW_RUNTIME_ERROR("expected end tag!");

    return new ast_node_op(name, op1, op2, NULL);
}

template<class resultT, class childT>
static ast_node* creatorFuncAN_templateChild1(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_StartTag)
        THROW_RUNTIME_ERROR("start tag expected!");

    xml.parseToNextTag();
    childT* child =
            dynamic_cast<childT*>(factory_ast_node::createFromXML(xml));

    if (child == NULL)
        THROW_RUNTIME_ERROR("failed to cast child!");

    xml.parseToNextTag();
    if (xml.curTagType != creax::tt_EndTag)
        THROW_RUNTIME_ERROR("expected end tag!");

    return new resultT(child, NULL);
}

static ast_node* creatorFuncAN_if(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_StartTag)
        THROW_RUNTIME_ERROR("start tag expected!");

    xml.parseToNextTag();
    ast_node_value_expr* condition =
            dynamic_cast<ast_node_value_expr*>(factory_ast_node::createFromXML(xml));

    xml.parseToNextTag();
    ast_node_statement* if_body =
            dynamic_cast<ast_node_statement*>(factory_ast_node::createFromXML(xml));

    if (condition == NULL)
        THROW_RUNTIME_ERROR("failed to cast condition!");
    if (if_body == NULL)
        THROW_RUNTIME_ERROR("failed to cast if_body!");

    xml.parseToNextTag();
    ast_node_statement* else_body = NULL;
    if (xml.curTagName == "else_body")
    {
        else_body = dynamic_cast<ast_node_statement*>(factory_ast_node::createFromXML(xml));
        xml.parseToNextTag();
        if (else_body == NULL)
            THROW_RUNTIME_ERROR("failed to cast else_body!");
    }

    if (xml.curTagType != creax::tt_EndTag)
        THROW_RUNTIME_ERROR("expected end tag!");

    return new ast_node_if_else(condition, if_body, else_body, NULL);
}

static ast_node* creatorFuncAN_stmtblock(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_StartTag)
        THROW_RUNTIME_ERROR("start tag expected!");

    xml.parseToNextTag();
    ast_node_statement* stmt =
            dynamic_cast<ast_node_statement*>(factory_ast_node::createFromXML(xml));

    if (stmt == NULL)
        THROW_RUNTIME_ERROR("failed to cast returnvalue!");

    xml.parseToNextTag();
    if (xml.curTagType != creax::tt_EndTag)
        THROW_RUNTIME_ERROR("expected end tag!");

    return new ast_node_stmtBlock(stmt, NULL);
}

static ast_node* creatorFuncAN_constant_int(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_EmptyTag)
        THROW_RUNTIME_ERROR("empty tag expected!");

    int value = xml.getAttributeInt("value");

    return new ast_node_constant_int(value, NULL);
}

static ast_node* creatorFuncAN_deref_op(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_StartTag)
        THROW_RUNTIME_ERROR("start tag expected!");

    xml.parseToNextTag();
    ast_node_value_expr* pointer =
            dynamic_cast<ast_node_value_expr*>(factory_ast_node::createFromXML(xml));

    if (pointer == NULL)
        THROW_RUNTIME_ERROR("failed to cast child!");

    xml.parseToNextTag();
    if (xml.curTagType != creax::tt_EndTag)
        THROW_RUNTIME_ERROR("expected end tag!");

    return new ast_node_deref_op(pointer, NULL);
}

static ast_node* creatorFuncAN_functioncall(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_StartTag)
        THROW_RUNTIME_ERROR("start tag expected!");

    std::string name = xml.getAttribute("name");
    SymbolFunc* sf = dynamic_cast<SymbolFunc*>(symbContext->find(name));
    if (sf == NULL)
        THROW_RUNTIME_ERROR("Function not found: " + name);

    xml.parseToNextTag();
    ast_node_exprlist* exprlist =
            dynamic_cast<ast_node_exprlist*>(factory_ast_node::createFromXML(xml));

    if (exprlist == NULL)
        THROW_RUNTIME_ERROR("failed to cast child!");

    xml.parseToNextTag();
    if (xml.curTagType != creax::tt_EndTag)
        THROW_RUNTIME_ERROR("expected end tag!");

    return new ast_node_functioncall(sf, exprlist, NULL);
}

template<class listT, class elemT>
static ast_node* creatorFuncAN_templatelist(creax::htmlparser& xml)
{
    listT* alist = new listT(NULL);

    switch (xml.curTagType)
    {
    case creax::tt_EmptyTag:
        return alist; // if no childs -> return empty alist!

    case creax::tt_StartTag:
        while (xml.parseToNextTag() && (xml.curTagType != creax::tt_EndTag))
        {
            elemT* a_elem =
                    dynamic_cast<elemT*>(
                        factory_ast_node::createFromXML(xml)
                        );
            if (a_elem == NULL)
                THROW_RUNTIME_ERROR("failed to cast child node!");
            alist->addChild(a_elem);
        }
        break;
    default:
        THROW_RUNTIME_ERROR("unexpected tag type!");
    }

    if (xml.curTagType != creax::tt_EndTag)
        THROW_RUNTIME_ERROR("expected end tag!");

    return alist;
}

ast_node* factory_ast_node::createFromXML(creax::htmlparser& xml)
{
    if (lookupTable.size() == 0)
    {
        lookupTable["declaration_var"] = &creatorFuncAN_declaration_var;
        lookupTable["parlist"] = &creatorFuncAN_templatelist<ast_node_parlist, ast_node_declaration_var>;
        lookupTable["statementlist"] = &creatorFuncAN_templatelist<ast_node_statementlist, ast_node_statement>;
        lookupTable["statement_var_def"] = &creatorFuncAN_templateChild1<ast_node_statement_var_def, ast_node_declaration_var>;
        lookupTable["statement_value_expr"] = &creatorFuncAN_templateChild1<ast_node_statement_value_expr, ast_node_value_expr>;
        lookupTable["assignment"] = &creatorFuncAN_assignment;
        lookupTable["identifier"] = &creatorFuncAN_identifier;
        lookupTable["op2"] = &creatorFuncAN_op2;
        lookupTable["return"] = &creatorFuncAN_templateChild1<ast_node_return, ast_node_value_expr>;
        lookupTable["addr_op"] = &creatorFuncAN_templateChild1<ast_node_addr_op, ast_node_lvalue_expr>;
        lookupTable["if"] = &creatorFuncAN_if;
        lookupTable["stmtblock"] = &creatorFuncAN_stmtblock;
        lookupTable["constant_int"] = &creatorFuncAN_constant_int;
        lookupTable["deref_op"] = &creatorFuncAN_deref_op;
        lookupTable["functioncall"] = &creatorFuncAN_functioncall;
        lookupTable["exprlist"] = &creatorFuncAN_templatelist<ast_node_exprlist, ast_node_expression>;
        lookupTable["expression"] = &creatorFuncAN_templateChild1<ast_node_expression, ast_node_value_expr>;
    }

    std::string type = xml.getAttribute("type");
    creatorFuncAN func = lookupTable[type];
    ast_node* result;

    if (func != NULL)
    {
        result = (*func)(xml);
    }
    else
    {
        THROW_RUNTIME_ERROR("no entry for type: " + type);
    }

    return result;
}
