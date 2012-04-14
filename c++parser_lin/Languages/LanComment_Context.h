#ifndef LANCOMMENT_CONTEXT
#define LANCOMMENT_CONTEXT

#include <iostream>
#include "threadfifo.h"

#include "basic_types.h"

class LanComment_Context
{
public:
	void* scanner;
    std::istream& is;
    creax::threadfifo<text_type>& fifo;
    int result;

    enum ttType {
        TTCODE = 0,
        TTLINECOMMENT = 1,
        TTBLOCKCOMMENT = 2
    };

public:
    LanComment_Context(std::istream& in, creax::threadfifo<text_type>& out)
        : is(in), fifo(out)
	{
		init_scanner();
	}

    virtual ~LanComment_Context()
	{
		destroy_scanner();
	}

    void text(const std::string& a_text)
    {
        fifo.push_data(text_type(a_text, TTCODE, getLineNo()));
    }

    void linecomment(const std::string& a_text)
    {
        fifo.push_data(text_type(a_text, TTLINECOMMENT, getLineNo()));
    }

    void blockcomment(const std::string& a_text)
    {
        fifo.push_data(text_type(a_text, TTBLOCKCOMMENT, getLineNo()));
    }

// Defined in LanComment.l
protected:
    int getLineNo();
    void init_scanner();
	void destroy_scanner();
};

int LanComment_parse(LanComment_Context*);

#endif // LANAB_CONTEXT
