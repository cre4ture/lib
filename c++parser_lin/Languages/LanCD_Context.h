#ifndef LANCD_CONTEXT
#define LANCD_CONTEXT

#include <iostream>
#include <sstream>
#include "ast_node_global_def_include.h"
#include "Symbols.h"
#include "creax_threadfifo.h"

class LanCD_Context
{
public:
    void* scanner;
    std::istringstream* is;
    ast_node_wurzel *wurzel;
    ast_node_define_depencies* dependencies;
    int line;
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
        line = 0;
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

    void yy_error(const char* err)
    {
        std::cerr << "line:" << line + getLineNo()
          << ",\"" << getYYtext()
         << "\", msg: " << err << std::endl;
    }

    void yy_input(char* const buf, int& result, int max_size, const int eof_result)
    {
        bool done = false;
        while (!done)
        {
            if (is == NULL)
            {
                std::string buffer;
                if (fifo.pop_data(buffer))
                {
                    is = new std::istringstream(buffer);
                }
                else
                {
                    result = eof_result; // EOF
                    return;
                }
            }

            result = is->readsome(buf, max_size);
            done = (result != 0);

            if (!done)
            {
                delete is;
                is = NULL;
            }
        }
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
