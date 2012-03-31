
#include "ast_internal.hpp"

// definition of global variables
context* locals = NULL;
global_vars* globals = NULL;
func_list* functions = NULL;
int config_vec_elements = 0;

std::string newLabel(std::string name)
{
        static int label_count = 0;
        std::ostringstream ostr;
        ostr << "LA" << label_count << "_" << name;
        label_count++;
        return ostr.str();
}

void beginNewContext()
{
    context *newc = new context(locals, "Y");
    locals = newc;
}

void endContext()
{
    context *oldc = locals;
    locals = oldc->getParent();
    delete oldc;
}
