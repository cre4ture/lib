#include "ast_nodes_impl.hpp"
#include "ast_internal.hpp"

void ast_node_statement_value_expr::compile()
{
    expr->compile_value();
}

void ast_node_global_def_var_def::compile()
{
    varDef->compile_decl(true);
}

void ast_node_stmtBlock::compile()
{
    beginNewContext();
    body->compile();
    endContext();
}

void ast_node_statement_var_def::compile()
{
    varDef->compile_decl(false /* statements are only in functions, not global */);
}

void ast_node_statementlist::compile()
{
    for (size_t i = 0; i < child_nodes.size(); i++)
    {
        ((ast_node_statement*)child_nodes[i])->compile();
    }
}

void ast_node_identifier::compile_value()
{
    // get address
    this->compile_address();
    // load value
    fprintf(compile_output, "\tLD A, [ 0 + A ]\t\t; A = %s\n", var->getName().c_str());
}

void ast_node_identifier::compile_address()
{
    if (var->isGlobal())
    {
        globals->compile_address(var->getName());
    }
    else
    {
        locals->compile_address(var->getName());
    }
}

void ast_node_deref_op::compile_address()
{
    this->pointerExpr->compile_value();
}

void ast_node_deref_op::compile_value()
{
    this->pointerExpr->compile_value();

    SymbolType* styp = pointerExpr->getTypeOf();
    if (styp->isPointer() == false)
    {
        throw std::runtime_error("derefer non pointer-type!");
    }

    SymbolTypePtr* sptrtyp = (SymbolTypePtr*)styp;

    hicovec_type htyp = convHicoVecType(sptrtyp->getTargetType());

    switch (htyp)
    {
    case HT_INT:
        // load address from stack
        fprintf(compile_output, "\tLD A, [ 0 + A ]\t; load from address in A\n");
        break;
    case HT_VECTOR:
        // load address from stack
        fprintf(compile_output, "\tVLD R0, [ 0 + A ]\t; load from address in A\n");
        break;
    default:
        throw std::runtime_error("deref: Unknown Type!");
    }
}

void ast_node_constant_int::compile_value()
{
    if (const_value > 65535)
    {
        fprintf(compile_output, "\tOR X, 0, %d\t\t; set factor to %d\n", 0x8000, 0x8000);
        // set value to A
        fprintf(compile_output, "\tOR A, 0, %d\t\t; A = %d\n", const_value >> 16, const_value >> 16);
        fprintf(compile_output, "\tMUL A, A, X\t\t; A = 2^15*X\n");
        fprintf(compile_output, "\tLSL A, A\t\t; A = 2*A\n");

        fprintf(compile_output, "\tOR X, 0, %d\t\t; X = %d\n", const_value & 0xffff, const_value & 0xffff);
        fprintf(compile_output, "\tADD A, A, X\n");
    }
    else
    {
        fprintf(compile_output, "\tOR A, 0, %d\t\t; A = %d\n", const_value, const_value);
    }
}

void ast_node_assignment::compile_value()
{
    switch (convHicoVecType(this->getTypeOf()))
    {
    case HT_INT:
        compile_value_int();
        break;
    case HT_VECTOR:
        compile_value_vec();
        break;
    default:
        throw std::runtime_error("unsupported type!");
        break;
    }
}

void ast_node_assignment::compile_value_int()
{
    dest->compile_address();
    // save address on stack
    int offset = locals->push_variable("address",1);  // 1 ^= 32bit
    fprintf(compile_output, "\tST [ Y + %d ], A\t;\n", offset);
    src->compile_value(); // returns scalar in A
    // load address from stack
    fprintf(compile_output, "\tLD X, [ Y + %d ]\t;\n", offset);
    // store value in A to address in X
    fprintf(compile_output, "\tST [ 0 + X ], A\t\t; (*X) = A\n");
    locals->pop_variable("address");
}

void ast_node_assignment::compile_value_vec()
{
    dest->compile_address();
    // save address on stack
    int offset = locals->push_variable("address",1);  // 1 ^= 32bit
    fprintf(compile_output, "\tST [ Y + %d ], A\t;\n", offset);
    src->compile_value(); // returns vector in R0
    // load address from stack
    fprintf(compile_output, "\tLD X, [ Y + %d ]\t;\n", offset);
    // store value in A to address in X
    fprintf(compile_output, "\tVST [ 0 + X ], R0\t\t; (*X) = A\n");
    locals->pop_variable("address");
}

void ast_node_global_defList::compile_member()
{
    for (size_t i = 0; i < child_nodes.size(); i++)
    {
        ((ast_node_global_def*)child_nodes[i])->compile();
    }
}

void ast_node_constIntList::compile_value()
{
    if (constList != NULL) constList->compile_value();
    fprintf(compile_output, "\t.dc 0x%x\n", constInt);
}

void ast_node_addr_op::compile_value()
{
    lvalue->compile_address();
}
