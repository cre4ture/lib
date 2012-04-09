
#include "ast_nodes_flow.hpp"
#include "ast_internal.hpp"

static FILE* compile_output;

void ast_node_while::compile()
{
    std::string l_start = newLabel("start");
    std::string l_exit = newLabel("exit");
    fprintf(compile_output, "%s:\t\t\t; while cond start \n", l_start.c_str());
    condition->compile_value();
    fprintf(compile_output, "\tJZ [ 0 + %s ]\t; exit if false \n", l_exit.c_str());
    beginNewContext();
    body->compile();
    endContext();
    fprintf(compile_output, "\tJMP [ 0 + %s ]\t; check condition \n", l_start.c_str());
    fprintf(compile_output, "%s:\t\t\t; while loop exit \n", l_exit.c_str());
}

void ast_node_for::compile()
{
    std::string l_start = newLabel("start");
    std::string l_exit = newLabel("exit");
    initial->compile();
    fprintf(compile_output, "%s:\t\t\t; for cond start \n", l_start.c_str());
    condition->compile_value();
    fprintf(compile_output, "\tJZ [ 0 + %s ]\t; exit if false \n", l_exit.c_str());
    beginNewContext();
    body->compile();
    assign->compile();
    endContext();
    fprintf(compile_output, "\tJMP [ 0 + %s ]\t; check condition \n", l_start.c_str());
    fprintf(compile_output, "%s:\t\t\t; for loop exit \n", l_exit.c_str());
}

void ast_node_do::compile()
{
    std::string l_start = newLabel("start");
    std::string l_exit = newLabel("exit");
    fprintf(compile_output, "%s:\t\t\t; for cond start \n", l_start.c_str());
    beginNewContext();
    body->compile();
    endContext();
    condition->compile_value();
    fprintf(compile_output, "\tJZ [ 0 + %s ]\t; exit if false \n", l_exit.c_str());
    fprintf(compile_output, "\tJMP [ 0 + %s ]\t; check condition \n", l_start.c_str());
    fprintf(compile_output, "%s:\t\t\t; for loop exit \n", l_exit.c_str());
}

void ast_node_if_else::compile()
{
    std::string l_else = newLabel("else");
    condition->compile_value();
    fprintf(compile_output, "\tJZ [ 0 + %s ] \t; jump to else if false \n", l_else.c_str());
    beginNewContext();
    if_body->compile();
    endContext();
    fprintf(compile_output, "%s:\t\t\t; else start \n", l_else.c_str());
    if (else_body != NULL)
    {
        beginNewContext();
        else_body->compile();
        endContext();
    }
}
