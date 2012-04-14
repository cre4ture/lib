#ifndef LANCD_CONTEXT
#define LANCD_CONTEXT

#include <iostream>
#include <sstream>
#include "ast_node_global_def_include.h"
#include "Symbols.h"
#include "threadfifo.h"

class LanCD_Context
{
public:
    void* scanner;
    std::istringstream* is;
    ast_node_wurzel *wurzel;
    ast_node_define_depencies* dependencies;
    int zeile;
    Symbols* symbContext;
    creax::threadfifo<std::string>& fifo;
    int parser_result;

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
    LanCD_Context(creax::threadfifo<std::string>& a_fifo)
        : fifo(a_fifo)
	{
		init_scanner();
        zeile = 1;
        wurzel = NULL;
        is = NULL;
        symbContext = NULL;
        beginNewSymbContext();
	}

	virtual ~LanCD_Context()
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

    // Defined in LanCD.l
    std::string getYYtext();
    // Defined in LanCD.l
    int getLineNo();


protected:
    // Defined in LanCD.l
    void init_scanner();
    // Defined in LanCD.l
	void destroy_scanner();
};

int LanCD_parse(LanCD_Context*);

#endif // LANCD_CONTEXT
