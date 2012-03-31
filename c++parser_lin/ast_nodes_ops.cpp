
#include "ast_nodes_ops.hpp"
#include "ast_internal.hpp"

void ast_node_op::compile_value()
{
}

/*void ast_node_sub::compile_int_value()
{
    op1->compile_value();
    int offset =  locals->push_variable("op1",1); // 1 ^= 32 bit
    fprintf(compile_output, "\tST [ Y + %d ], A\t; push(A) // save op1\n", offset);
    op2->compile_value();
    fprintf(compile_output, "\tLD X, [ Y + %d ]\t; pop(A) // load op1 to X\n", offset);
    fprintf(compile_output, "\tSUB A, X, A\t\t; A = X - A // subtraction\n");
    locals->pop_variable("op1");
}

void ast_node_sub_compile_vec_value()
{
    op1->compile_value();

    int offset =  locals->push_variable("op1",getSizeOfType(ET_VECTOR));

    fprintf(compile_output, "\tOR X, 0, %d\t\t; store offset in X\n", offset);
    fprintf(compile_output, "\tVST [ Y + X ], R0\t; push(R0) // save op1\n");

    op2->compile_value();

    fprintf(compile_output, "\tOR X, 0, %d\t\t; store offset in X\n", offset);
    fprintf(compile_output, "\tVLD R1, [ Y + X ]\t; pop(R1) // load op1 to R1\n");

    locals->pop_variable("op1");

    fprintf(compile_output, "\tVSUB.DW R0, R1, R0\t\t; R0 = R1 + R0\n");
}

void ast_node_add::compile_int_value()	//Fei
{
    op1->compile_value();
    int offset =  locals->push_variable("op1",1);
    fprintf(compile_output, "\tST [ Y + %d ], A\t; push(A) // save op1\n", offset);
    op2->compile_value();
    fprintf(compile_output, "\tLD X, [ Y + %d ]\t; pop(A) // load op1 to X\n", offset);
    fprintf(compile_output, "\tADD A, X, A\t\t; A = X + A // addition\n");
    locals->pop_variable("op1");
}

void ast_node_add::compile_vec_value()
{
    op1->compile_value();

    int offset =  locals->push_variable("op1",getSizeOfType(ET_VECTOR));

    fprintf(compile_output, "\tOR X, 0, %d\t\t; store offset in X\n", offset);
    fprintf(compile_output, "\tVST [ Y + X ], R0\t; push(R0) // save op1\n");

    op2->compile_value();

    fprintf(compile_output, "\tOR X, 0, %d\t\t; store offset in X\n", offset);
    fprintf(compile_output, "\tVLD R1, [ Y + X ]\t; pop(R1) // load op1 to R1\n");

    locals->pop_variable("op1");

    fprintf(compile_output, "\tVADD.DW R0, R1, R0\t; R0 = R1 + R0\n");
}

void ast_node_mul::compile_int_value()  //Fei
{
    op1->compile_value();
    int offset =  locals->push_variable("op1",1);
    fprintf(compile_output, "\tST [ Y + %d ], A\t; push(A) // save op1\n", offset);
    op2->compile_value();
    fprintf(compile_output, "\tLD X, [ Y + %d ]\t; pop(A) // load op1 to X\n", offset);
    fprintf(compile_output, "\tMUL A, X, A\t\t; A = X * A // multiplication\n");
    locals->pop_variable("op1");
}

void ast_node_mul::compile_vec_value()
{
    op1->compile_value();

    int offset =  locals->push_variable("op1",getSizeOfType(ET_VECTOR));

    fprintf(compile_output, "\tOR X, 0, %d\t\t; store offset in X\n", offset);
    fprintf(compile_output, "\tVST [ Y + X ], R0\t; push(R0) // save op1\n");

    op2->compile_value();

    fprintf(compile_output, "\tOR X, 0, %d\t\t; store offset in X\n", offset);
    fprintf(compile_output, "\tVLD R1, [ Y + X ]\t; pop(R1) // load op1 to R1\n");

    locals->pop_variable("op1");

    fprintf(compile_output, "\tVMUL.W R0, R1, R0\t\t; R0 = R1 + R0\n");
}

void ast_node_div::compile_int_value()    //Fei
{
    op1->compile_value();
    int offset =  locals->push_variable("op1",1);
    fprintf(compile_output, "\tST [ Y + %d ], A\t; push(A) // save op1\n", offset);
    op2->compile_value();
    fprintf(compile_output, "\tLD X, [ Y + %d ]\t; pop(A) // load op1 to X\n", offset);
    fprintf(compile_output, "\tDIV A, X, A\t\t; A = X / A // subtraction\n");
    locals->pop_variable("op1");
}

void ast_node_div::compile_vec_value()
{
    // TODO
}


void ast_node_cmpeq::compile_int_value()
{
    std::string l_false = newLabel("CMPEQ_RETURN_FALSE");
    std::string l_end = newLabel("CMPEQ_END");
    op1->compile_value();
    int offset = locals->push_variable("op1", 1);  // 1 ^= 32bit
    fprintf(compile_output, "\tST [ Y + %d ], A\t; push(A) // save op1\n", offset);
    op2->compile_value();
    fprintf(compile_output, "\tLD X, [ Y + %d ]\t; pop(A) // load op1 to X\n", offset);
    locals->pop_variable("op1");
    fprintf(compile_output, "\tSUB 0, X, A\t\t; A = X - A // subtraction\n");
    fprintf(compile_output, "\tJNZ [ 0 + %s ]\t; check result\n", l_false.c_str());
    fprintf(compile_output, "\tADD A, 0, 1\n");
    fprintf(compile_output, "\tJMP [ 0 + %s ]\n", l_end.c_str());
    fprintf(compile_output, "%s:\t\t\t\n", l_false.c_str());
    fprintf(compile_output, "\tADD A, 0, 0\n");
    fprintf(compile_output, "%s:\t\t\t\n", l_end.c_str());
}

void ast_node_cmpg::compile_int_value()
{
    std::string l_false = newLabel("CMPG_RETURN_FALSE");
    std::string l_end = newLabel("CMPG_END");
    op2->compile_value();
    int offset = locals->push_variable("op2", 1);  // 1 ^= 32bit
    fprintf(compile_output, "\tST [ Y + %d ], A\t; push(A) // save op2\n", offset);
    op1->compile_value();
    fprintf(compile_output, "\tLD X, [ Y + %d ]\t; pop(A) // load op2 to X\n", offset);
    locals->pop_variable("op2");
    fprintf(compile_output, "\tSUB 0, X, A\t\t; A = X - A // subtraction\n");
    fprintf(compile_output, "\tJNC [ 0 + %s ]\t; check result\n", l_false.c_str());
    fprintf(compile_output, "\tADD A, 0, 1\n");
    fprintf(compile_output, "\tJMP [ 0 + %s ]\n", l_end.c_str());
    fprintf(compile_output, "%s:\t\t\t\n", l_false.c_str());
    fprintf(compile_output, "\tADD A, 0, 0\n");
    fprintf(compile_output, "%s:\t\t\t\n", l_end.c_str());
}

void ast_node_cmpl::compile_int_value()
{
    std::string l_false = newLabel("CMPL_RETURN_FALSE");
    std::string l_end = newLabel("CMPL_END");
    op1->compile_value();
    int offset = locals->push_variable("op1", 1);  // 1 ^= 32bit
    fprintf(compile_output, "\tST [ Y + %d ], A\t; push(A) // save op1\n", offset);
    op2->compile_value();
    fprintf(compile_output, "\tLD X, [ Y + %d ]\t; pop(A) // load op1 to X\n", offset);
    locals->pop_variable("op1");
    fprintf(compile_output, "\tSUB A, X, A\t\t; A = X - A // subtraction\n");
    fprintf(compile_output, "\tJNC [ 0 + %s ]\t; check result\n", l_false.c_str());
    fprintf(compile_output, "\tADD A, 0, 1\n");
    fprintf(compile_output, "\tJMP [ 0 + %s ]\n", l_end.c_str());
    fprintf(compile_output, "%s:\t\t\t\n", l_false.c_str());
    fprintf(compile_output, "\tADD A, 0, 0\n");
    fprintf(compile_output, "%s:\t\t\t\n", l_end.c_str());
}

void ast_node_cmpge::compile_int_value()
{
    std::string l_false = newLabel("CMPGE_RETURN_FALSE");
    std::string l_end = newLabel("CMPGE_END");
    op1->compile_value();
    int offset = locals->push_variable("op1", 1);  // 1 ^= 32bit
    fprintf(compile_output, "\tST [ Y + %d ], A\t; push(A) // save op1\n", offset);
    op2->compile_value();
    fprintf(compile_output, "\tLD X, [ Y + %d ]\t; pop(A) // load op1 to X\n", offset);
    locals->pop_variable("op1");
    fprintf(compile_output, "\tSUB 0, X, A\t\t; A = X - A // subtraction\n");
    fprintf(compile_output, "\tJC [ 0 + %s ]\t; check result\n", l_false.c_str());
    fprintf(compile_output, "\tADD A, 0, 1\n");
    fprintf(compile_output, "\tJMP [ 0 + %s ]\n", l_end.c_str());
    fprintf(compile_output, "%s:\t\t\t\n", l_false.c_str());
    fprintf(compile_output, "\tADD A, 0, 0\n");
    fprintf(compile_output, "%s:\t\t\t\n", l_end.c_str());
}

void ast_node_cmple::compile_int_value()
{
    std::string l_false = newLabel("CMPLE_RETURN_FALSE");
    std::string l_end = newLabel("CMPLE_END");
    op2->compile_value();
    int offset = locals->push_variable("op2", 1);  // 1 ^= 32bit
    fprintf(compile_output, "\tST [ Y + %d ], A\t; push(A) // save op2\n", offset);
    op1->compile_value();
    fprintf(compile_output, "\tLD X, [ Y + %d ]\t; pop(A) // load op2 to X\n", offset);
    locals->pop_variable("op2");
    fprintf(compile_output, "\tSUB 0, X, A\t\t; A = X - A // subtraction\n");
    fprintf(compile_output, "\tJC [ 0 + %s ]\t; check result\n", l_false.c_str());
    fprintf(compile_output, "\tADD A, 0, 1\n");
    fprintf(compile_output, "\tJMP [ 0 + %s ]\n", l_end.c_str());
    fprintf(compile_output, "%s:\t\t\t\n", l_false.c_str());
    fprintf(compile_output, "\tADD A, 0, 0\n");
    fprintf(compile_output, "%s:\t\t\t\n", l_end.c_str());
}

void ast_node_cmpne::compile_int_value()
{
    std::string l_end = newLabel("CMPNE_END");
    op1->compile_value();
    int offset = locals->push_variable("op1", 1);  // 1 ^= 32bit
    fprintf(compile_output, "\tST [ Y + %d ], A\t; push(A) // save op1\n", offset);
    op2->compile_value();
    fprintf(compile_output, "\tLD X, [ Y + %d ]\t; pop(A) // load op1 to X\n", offset);
    locals->pop_variable("op1");
    fprintf(compile_output, "\tSUB 0, X, A\t\t; A = X - A // subtraction\n");
    fprintf(compile_output, "\tJZ [ 0 + %s ]\t; check result\n", l_end.c_str());
    fprintf(compile_output, "\tADD A, 0, 1\n");
    fprintf(compile_output, "%s:\t\t\t\n", l_end.c_str());
}
*/

