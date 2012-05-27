#ifndef LANCF_CONTEXT
#define LANCF_CONTEXT

#include <iostream>
#include <sstream>
#include "ast_node_global_def_include.h"
#include "Symbols.h"
#include "creax_threadfifo.h"

#include "LanCC_Context.h"
#include "basic_types.h"

class LanCF_Context;
int LanCF_parse(LanCF_Context*);

class LanCF_Context: public LanXX_Context
{
private:
    std::istringstream* is;
    ast_node_wurzel *wurzel;
    Symbols* symbContext;
    std::string m_namespace;

    // tmp parser data
    std::string c_name;

    // for decl
    std::vector<std::string> namespaces;
    std::string name;
    decl_end* _decl_end;
    atype* _type;


    void beginNewSymbContext()
    {
        Symbols *newc = new Symbols(symbContext);
        symbContext = newc;
    }

    void endSymbContext()
    {
        Symbols *oldc = symbContext;
        symbContext = oldc->getParent();
        delete oldc;
    }

public:
    void* scanner;
    ast_node_define_depencies* dependencies;
    int parser_result;
    int level;

    LanCF_Context(creax::threadfifo<code_piece>& a_fifo, int a_line, const std::string& a_namespace)
        : LanXX_Context(a_fifo, a_line), m_namespace(a_namespace)
	{
		init_scanner();
        wurzel = NULL;
        is = NULL;
        symbContext = NULL;
        beginNewSymbContext();
	}

    virtual ~LanCF_Context()
	{
        endSymbContext();
        while (symbContext != NULL)
        {
            endSymbContext();
            std::cerr << "warning: context not clean!" << std::endl;
        }
        //if (symbContext != NULL)
          //  throw std::runtime_error("Should reach the end here!");

		destroy_scanner();
	}

    void namespace_decl(const std::string& name, const std::string& block_code)
    {
        creax::threadfifo<code_piece> fifo;
        fifo.push_data(code_piece(block_code, blockstart));
        fifo.close_fifo();
        LanCF_Context tmp(fifo, blockstart, name);
        LanCF_parse(&tmp);
    }

    void decl(atype* t, var_name* n, decl_end* e)
    {
        std::cout << "decl: " << t->name << " ";
        n->fancy(std::cout);
        if (e->_func_decl_end != NULL)
            std::cout << "()";
        std::cout << std::endl;
    }

    void decl(atype* t, std::string ns, var_name* n, decl_end* e)
    {
        std::cout << "decl: " << t->name << " " << ns << "::";
        n->fancy(std::cout);
        if (e->_func_decl_end != NULL)
            std::cout << "()";
        std::cout << std::endl;
    }

    void include(std::string filename)
    {
        std::cout << "#include " << filename << std::endl;
    }

    // Defined in LanCF.l
    std::string getYYtext();
    // Defined in LanCF.l
    int getLineNo();


protected:
    // Defined in LanCF.l
    void init_scanner();
    // Defined in LanCF.l
	void destroy_scanner();
};

#endif // LANCD_CONTEXT
