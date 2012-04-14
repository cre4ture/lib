#ifndef LANCF_CONTEXT
#define LANCF_CONTEXT

#include <iostream>
#include <sstream>
#include "ast_node_global_def_include.h"
#include "Symbols.h"
#include "threadfifo.h"

#include "basic_types.h"

class LanCF_Context;
int LanCF_parse(LanCF_Context*);

class LanCF_Context
{
private:
    std::istringstream* is;
    ast_node_wurzel *wurzel;
    int line;
    Symbols* symbContext;
    creax::threadfifo<code_piece>& fifo;

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

    LanCF_Context(creax::threadfifo<code_piece>& a_fifo)
        : fifo(a_fifo)
	{
		init_scanner();
        line = 0;
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

    void yy_error(const char* err)
    {
        std::cerr << "CF: line:" << line + getLineNo()
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
                code_piece buffer;
                if (fifo.pop_data(buffer))
                {
                    is = new std::istringstream(buffer.code);
                    line = buffer.line - getLineNo();
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

    void namespace_decl(const std::string& block_code)
    {
        creax::threadfifo<code_piece> fifo;
        fifo.push_data(code_piece(block_code, line + getLineNo()));
        fifo.close_fifo();
        LanCF_Context tmp(fifo);
        LanCF_parse(&tmp);
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
