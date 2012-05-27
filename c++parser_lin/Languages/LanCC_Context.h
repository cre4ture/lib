#ifndef LANCC_CONTEXT
#define LANCC_CONTEXT

#include <iostream>
#include <sstream>
#include "ast_node_global_def_include.h"
#include "Symbols.h"
#include "creax_threadfifo.h"

#include "basic_types.h"
#include "parser_class.h"

class LanCC_Context;
int LanCC_parse(LanCC_Context*);

class LanCC_Context: public LanXX_Context
{
private:
    std::istringstream* is;
    ast_node_wurzel *wurzel;
    Symbols* symbContext;
    std::string m_namespace;

    // for c_decl
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
    ast_node_define_depencies* dependencies;
    int parser_result;
    int level;

    LanCC_Context(creax::threadfifo<code_piece>& a_fifo, int a_line, const std::string& a_namespace)
        : LanXX_Context(a_fifo, a_line), m_namespace(a_namespace)
	{
        init_scanner();
        wurzel = NULL;
        is = NULL;
        symbContext = NULL;
        beginNewSymbContext();
        m_namespace = a_namespace;
	}

    virtual ~LanCC_Context()
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

    void c_decl(atype* t, std::string n, decl_end* e)
    {
        std::cout << "c_decl: " << t->name << " " << n << std::endl;
    }

    // Defined in LanCC.l
    std::string getYYtext();
    // Defined in LanCC.l
    int getLineNo();


protected:
    // Defined in LanCC.l
    void init_scanner();
    // Defined in LanCC.l
	void destroy_scanner();
};

#endif // LANCD_CONTEXT
