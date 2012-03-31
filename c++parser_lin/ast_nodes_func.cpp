#include "ast_nodes_func.hpp"
#include "ast_internal.hpp"
#include "ast_node_global_def_include.h"

ast_node_declaration_var::ast_node_declaration_var(const std::string& _name, SymbolType* _type, ast_node_constIntList* a_initValue, ast_node* parent)
    : ast_node(parent, annt_buildintypedecl), name(_name), type(_type)
{
    int elements = getSizeOfType(convHicoVecType(type));
    if (a_initValue == NULL)
    {
        initValue = NULL;
        for (int i = 0; i < elements; i++)
            initValue = new ast_node_constIntList(initValue, 0, NULL);
    }
    else
    {
        if (elements != a_initValue->count_members())
            throw std::runtime_error("Anzahl Vectorelemente in Initialisierungsliste falsch!");
        initValue = a_initValue;
    }

    //addChild(initValue);
}

void ast_node_declaration_var::compile_decl(bool isGlobal)
{
    if (isGlobal)
    {
        globals->addGlobal(this->name, this->initValue);
    }
    else
    {
        locals->push_variable(this->name, getSizeOfType(convHicoVecType(this->type)));
    }
}

void ast_node_wurzel::compile()
{
    globals = new global_vars();
    functions = new func_list();
    functions->addFunction("main", new function("main", getBuildInType("int"), NULL));

    Symbol* myMain = symbContext->find("main");
    if (myMain == NULL)
        throw std::runtime_error("function 'main' not found!");
    if (myMain->getSymbolType() != st_func)
        throw std::runtime_error("symbol 'main' is not a function!");

    fprintf(compile_output, "\t.org 0x0\n");

    fprintf(compile_output, "\t.start\n");
    fprintf(compile_output, "\tOR  Y, 0, STACK\t; Y = STACK  \n");
    ast_node_functioncall* main_call = new ast_node_functioncall((SymbolFunc*)myMain, NULL, NULL);
    main_call->compile_value();
    delete main_call;
    fprintf(compile_output, "\tHALT\n");

    if (defList != NULL) defList->compile_member();

    globals->defineGlobals();
    delete globals;
    globals = NULL;
    fprintf(compile_output, "STACK:\t.dc 0x0\t\t; dummy for stack begin  \n");
    fprintf(compile_output, "\t.end\n");

    delete functions;
    functions = NULL;
}

void ast_node_return::compile()
{
    expression->compile_value();
    hicovec_type type = convHicoVecType(expression->getTypeOf());

    int offset_result = locals->getOffsetOf("0_result");

    switch (type)
    {
    case HT_INT:
        fprintf(compile_output,"\tST [Y + %d], A \t; return: write result\n", offset_result);
        break;
    case HT_VECTOR:
        fprintf(compile_output,"\tOR A, 0, %d \t\t; return: offset\n", offset_result);
        fprintf(compile_output,"\tVST [Y + A], R0 \t\t; return: write result\n");
        break;
    default:
        break;
    }
}

void ast_node_function_def::compile()
{
    locals = new context(NULL, "Y");

    // assume calling code to push params to stack:
    if (parlist != NULL) parlist->compile_value();

    int rsize = getSizeOfType(convHicoVecType(this->type));

    // Platz für Ergebnis reservieren
    locals->push_variable("0_result", rsize); // verwende "0_" damit es nicht versehentlich mit einer anderen Variable kollidiert

    fprintf(compile_output,"FUNC_%s:\n", name.c_str());
    beginNewContext();
    // assume calling code places return address in register A (asm: JAL)
    int offset_addr = locals->push_variable("returnaddr", getSizeOfType(HT_INT));
    fprintf(compile_output,"\tST [Y + %d], A \t; func: save return address\n", offset_addr);
    if (stmtList != NULL) stmtList->compile();
    fprintf(compile_output,"\tLD X, [Y + %d] \t; func: load returnaddr -> X\n", offset_addr);
    endContext();
    fprintf(compile_output,"\tJMP [X + 0] \t; func: jump back to calling code\n");

    if (locals->getParent() != NULL)
        throw std::runtime_error("internal error: context count not 0!");
    delete locals;
    locals = NULL;

    functions->addFunction(name, new function(name, this->type, NULL));
}

void ast_node_functioncall::compile_value()
{
    int offset;

    beginNewContext();
    // parameter auf den Stack legen
    if (exprlist != NULL) exprlist->compile_value();

    hicovec_type rtyp = convHicoVecType(this->func->getType());
    int rsize = getSizeOfType(rtyp);

    // Platz für Ergebnis reservieren
    offset = locals->push_variable("0_result", rsize); // verwende "0_" damit es nicht versehentlich mit einer anderen Variable kollidiert

    // Sprung durchführen (Funktion springt am Ende automatisch zurück)
    fprintf(compile_output,"\tJAL A, [0 + FUNC_%s] \t; call: jump!\n", func->getName().c_str());

    // Ergebnis ins register A bzw. R0 laden
    switch (rtyp) {
    case HT_INT: fprintf(compile_output,"\tLD A, [Y + %d] \t; call: load result to A\n", offset);
        break;
    case HT_VECTOR:
        fprintf(compile_output,"\tOR A, 0, %d \t\t; call: calc offset of result\n", offset);
        fprintf(compile_output,"\tVLD R0, [Y + A] \t; call: load result to R0\n");
        break;
    default: // VOID
        break;
    }
    locals->pop_variable("0_result");
    endContext(); // delete params from Stack
}

void ast_node_parlist::compile_value()
{
    for (size_t i = 0; i < child_nodes.size(); i++)
    {
        (static_cast<ast_node_declaration_var*>(child_nodes[i]))->compile_decl(false);
    }
}

void ast_node_exprlist::compile_value()
{
    for (size_t i = 0; i < child_nodes.size(); i++)
    {
        ((ast_node_expression*)child_nodes[i])->compile_value();
    }
}

void ast_node_expression::compile_value()
{
    this->term->compile_value();

    hicovec_type htyp = convHicoVecType(this->getTypeOf());
    int rsize = getSizeOfType(htyp);
    int offset = locals->push_variable("paramX", rsize);

    // ergebnis auf Stack schieben
    switch (htyp) {
    case HT_INT: fprintf(compile_output,"\tST [Y + %d], A \t\t; store param from A\n", offset);
        break;
    case HT_VECTOR:
        fprintf(compile_output,"\tOR A, 0, %d \t\t; offset\n", offset);
        fprintf(compile_output,"\tVST [Y + A], R0 \t; store param R0\n");
        break;
    default: // VOID
        break;
    }
}
