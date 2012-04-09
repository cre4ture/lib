#ifndef LANAB_CONTEXT
#define LANAB_CONTEXT

#include <iostream>
#include "ast_nodes_func.hpp"
#include "LanCD_Context.h"

using namespace std;

class LanAB_Context
{
public:
	void* scanner;
	int result;
	istream* is;
	int esc_depth;
    int preprocessor_result;

    definelist defines;
    int level_on;
    int level_off;
    creax::threadfifo<std::string> codefifo;

    LanCD_Context cdcontext;

public:
    LanAB_Context(istream* is)
        : cdcontext(codefifo)
	{
		init_scanner();
		this->is = is;
        level_on = 0;
        level_off = 0;
	}

	virtual ~LanAB_Context()
	{
		destroy_scanner();
	}

    void if_def(const std::string& name, bool not_def = false)
    {
        if (level_off > 0)
        {
            level_off++;
        }
        else
        {
            defines.addDependency(name);

            bool defined = defines.isSet(name);
            if (not_def) defined = !defined;
            if (defined)
            {
                level_on++;
            }
            else
            {
                level_off++;
            }
        }
    }

    void if_n_def(const std::string& name)
    {
        if_def(name, true);
    }

    void else_if()
    {
        if (level_off >= 2)
        {
            // nothing
        }
        else if (level_off == 1)
        {
            level_off = 0;
            level_on++;
        }
        else // level_off == 0
        {
            level_on--;
            level_off = 1;
        }
    }

    void end_if()
    {
        if (level_off > 0)
        {
            level_off--;
        }
        else
        {
            level_on--;
        }
    }

// Defined in LanAB.l
protected:
	void init_scanner();	
	void destroy_scanner();
};

int LanAB_parse(LanAB_Context*);

#endif // LANAB_CONTEXT
