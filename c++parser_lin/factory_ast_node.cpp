#include "factory_ast_node.h"
#include "ast_nodes_impl.hpp"
#include "ast_nodes_func.hpp"
#include "ast_nodes_ops.hpp"
#include "ast_nodes_flow.hpp"
#include "ast_node_global_def_include.h"

typedef ast_node* (*creatorFuncAN)(creax::htmlparser& xml);
static std::map<std::string, creatorFuncAN> lookupTable;

#define THROW_RUNTIME_ERROR(MSG) throw std::runtime_error(std::string(__PRETTY_FUNCTION__) + ": " + MSG)

template<class resultT, class childT>
static ast_node* creatorFuncAN_templateNameChild1(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_StartTag)
        THROW_RUNTIME_ERROR("start tag expected!");

    std::string name = xml.getAttribute("name");

    xml.parseToNextTag();
    childT* child = dynamic_cast<childT*>(factory_ast_node::createFromXML(xml));

    if (child == NULL)
        THROW_RUNTIME_ERROR("failed to cast child!");

    xml.parseToNextTag();
    if (xml.curTagType != creax::tt_EndTag)
        THROW_RUNTIME_ERROR("expected end tag!");

    resultT* var = new resultT(name, child, NULL, NULL);
    return var;
}

static ast_node* creatorFuncAN_type(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_EmptyTag)
        THROW_RUNTIME_ERROR("empty tag expected!");

    std::string name = xml.getAttribute("name");
    int pointerlevel = xml.getAttributeInt("pointerlevel");

    ast_node_type* var = new ast_node_type(name, pointerlevel, NULL);
    return var;
}

static ast_node* creatorFuncAN_identifier(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_EmptyTag)
        THROW_RUNTIME_ERROR("empty tag expected!");

    std::string name = xml.getAttribute("name");
    SymbolVar* id = dynamic_cast<SymbolVar*>(new SymbolVar(name, new SymbolType("int"), false));

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

template<class resT, class childT1, class childT2>
static ast_node* creatorFuncAN_templateChild2(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_StartTag)
        THROW_RUNTIME_ERROR("start tag expected!");

    xml.parseToNextTag();
    childT1* op1 =
            dynamic_cast<childT1*>(factory_ast_node::createFromXML(xml));

    xml.parseToNextTag();
    childT2* op2 =
            dynamic_cast<childT2*>(factory_ast_node::createFromXML(xml));

    if (op1 == NULL)
        THROW_RUNTIME_ERROR("failed to cast child1!");
    if (op2 == NULL)
        THROW_RUNTIME_ERROR("failed to cast child2!");

    xml.parseToNextTag();
    if (xml.curTagType != creax::tt_EndTag)
        THROW_RUNTIME_ERROR("expected end tag!");

    return new resT(op1, op2, NULL);
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

static ast_node* creatorFuncAN_constant_int(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_EmptyTag)
        THROW_RUNTIME_ERROR("empty tag expected!");

    int value = xml.getAttributeInt("value");

    return new ast_node_constant_int(value, NULL);
}

static ast_node* creatorFuncAN_functioncall(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_StartTag)
        THROW_RUNTIME_ERROR("start tag expected!");

    std::string name = xml.getAttribute("name");
    SymbolFunc* sf = dynamic_cast<SymbolFunc*>(new SymbolFunc(name, new SymbolType("int")));
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

static ast_node* creatorFuncAN_for(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_StartTag)
        THROW_RUNTIME_ERROR("start tag expected!");

    xml.parseToNextTag();
    ast_node_statement* initial =
            dynamic_cast<ast_node_statement*>(factory_ast_node::createFromXML(xml));
    if (initial == NULL)
        THROW_RUNTIME_ERROR("failed to cast initial tag!");

    xml.parseToNextTag();
    ast_node_value_expr* condition =
            dynamic_cast<ast_node_value_expr*>(factory_ast_node::createFromXML(xml));
    if (initial == NULL)
        THROW_RUNTIME_ERROR("failed to cast condition tag!");

    xml.parseToNextTag();
    ast_node_statement_value_expr* assign =
            dynamic_cast<ast_node_statement_value_expr*>(factory_ast_node::createFromXML(xml));
    if (initial == NULL)
        THROW_RUNTIME_ERROR("failed to cast assign tag!");

    xml.parseToNextTag();
    ast_node_statement* body =
            dynamic_cast<ast_node_statement*>(factory_ast_node::createFromXML(xml));
    if (initial == NULL)
        THROW_RUNTIME_ERROR("failed to cast body tag!");

    xml.parseToNextTag();
    if (xml.curTagType != creax::tt_EndTag)
        THROW_RUNTIME_ERROR("expected end tag!");

    return new ast_node_for(initial, condition, assign, body, NULL);
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

static ast_node* creatorFuncGD_include(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_EmptyTag)
        throw std::runtime_error("creatorFuncGD_include: empty tag expected");
    ast_node_global_def_include* inc =
            new ast_node_global_def_include(xml.getAttribute("filename"),
                                            xml.getAttribute("lib") != "0", NULL);
    return inc;
}

static ast_node* creatorFuncDG_var(creax::htmlparser& xml)
{
    if (xml.curTagType != creax::tt_StartTag)
        throw std::runtime_error("creatorFuncDG_var: start tag expected!");

    xml.parseToNextTag();
    ast_node_declaration_var* var = dynamic_cast<ast_node_declaration_var*>(factory_ast_node::createFromXML(xml));
    xml.parseToNextTag();
    if (xml.curTagType != creax::tt_EndTag)
        throw std::runtime_error("creatorFuncDG_var: expected end tag!");

    return new ast_node_global_def_var_def(var, NULL);
}

static ast_node* creatorFuncDG_function_def(creax::htmlparser& xml)
{
    std::string name = xml.getAttribute("name");
    std::string tpname = xml.getAttribute("resulttype");
    SymbolType* tp = dynamic_cast<SymbolType*>(new SymbolType(tpname));
    if (tp == NULL)
        throw std::runtime_error("creatorFuncDG_function_def: type not found: " + tpname);

    xml.parseToNextTag();
    ast_node_parlist* parlist = dynamic_cast<ast_node_parlist*>(factory_ast_node::createFromXML(xml));
    xml.parseToNextTag();
    ast_node_statementlist* stmtlist = dynamic_cast<ast_node_statementlist*>(factory_ast_node::createFromXML(xml));

    xml.parseToNextTag();
    if (xml.curTagType != creax::tt_EndTag)
        throw std::runtime_error("creatorFuncDG_function_def: expected end tag!");

    return new ast_node_function_def(name, tp, parlist, stmtlist, NULL);
}

ast_node* factory_ast_node::createFromXML(creax::htmlparser& xml)
{
    if (lookupTable.size() == 0)
    {
        lookupTable["declaration_var"] = &creatorFuncAN_templateNameChild1<ast_node_declaration_var, ast_node_type>;
        lookupTable["type"] = &creatorFuncAN_type;
        lookupTable["parlist"] = &creatorFuncAN_templatelist<ast_node_parlist, ast_node_declaration_var>;
        lookupTable["statementlist"] = &creatorFuncAN_templatelist<ast_node_statementlist, ast_node_statement>;
        lookupTable["statement_var_def"] = &creatorFuncAN_templateChild1<ast_node_statement_var_def, ast_node_declaration_var>;
        lookupTable["statement_value_expr"] = &creatorFuncAN_templateChild1<ast_node_statement_value_expr, ast_node_value_expr>;
        lookupTable["assignment"] = &creatorFuncAN_templateChild2<ast_node_assignment, ast_node_lvalue_expr, ast_node_value_expr>;
        lookupTable["identifier"] = &creatorFuncAN_identifier;
        lookupTable["op2"] = &creatorFuncAN_op2;
        lookupTable["return"] = &creatorFuncAN_templateChild1<ast_node_return, ast_node_value_expr>;
        lookupTable["addr_op"] = &creatorFuncAN_templateChild1<ast_node_addr_op, ast_node_lvalue_expr>;
        lookupTable["if"] = &creatorFuncAN_if;
        lookupTable["stmtblock"] = &creatorFuncAN_templateChild1<ast_node_stmtBlock, ast_node_statement>;
        lookupTable["constant_int"] = &creatorFuncAN_constant_int;
        lookupTable["deref_op"] = &creatorFuncAN_templateChild1<ast_node_deref_op, ast_node_value_expr>;
        lookupTable["functioncall"] = &creatorFuncAN_functioncall;
        lookupTable["exprlist"] = &creatorFuncAN_templatelist<ast_node_exprlist, ast_node_expression>;
        lookupTable["expression"] = &creatorFuncAN_templateChild1<ast_node_expression, ast_node_value_expr>;
        lookupTable["while"] = &creatorFuncAN_templateChild2<ast_node_while, ast_node_value_expr, ast_node_statement>;
        lookupTable["for"] = &creatorFuncAN_for;
        lookupTable["do"] = &creatorFuncAN_templateChild2<ast_node_do, ast_node_value_expr, ast_node_statement>;
        lookupTable["root"] = &creatorFuncAN_templateChild2<ast_node_wurzel, ast_node_define_depencies, ast_node_global_defList>;
        lookupTable["dependencies"] = &creatorFuncAN_templatelist<ast_node_define_depencies, ast_node_define_depencie>;
        lookupTable["global_deflist"] = &creatorFuncAN_templatelist<ast_node_global_defList, ast_node_global_def>;
        lookupTable["globaldef_include"] = &creatorFuncGD_include;
        lookupTable["globaldef_var"] = &creatorFuncDG_var;
        lookupTable["function_def"] = &creatorFuncDG_function_def;
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
