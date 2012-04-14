#ifndef LANAB_CONTEXT
#define LANAB_CONTEXT

#include <iostream>
#include "ast_nodes_func.hpp"
#include "ast_nodes_impl.hpp"

#include "LanComment_Context.h"

#include "LanCD_Context.h"

#include "basic_types.h"

class definelist
{
private:
    std::map<std::string, std::string> defines;
    std::map<std::string, int> depends;

public:

    void saveDependencies(ast_node_define_depencies* dependencies)
    {
        for (std::map<std::string, int>::iterator i = depends.begin();
             i != depends.end(); i++)
        {
            if (i->second > 0)
            {
                std::string value;
                bool isset = getValue(i->first, value);
                dependencies->addChild(new ast_node_define_depencie(i->first, value, isset));
            }
        }
    }

    void addDependency(const std::string& define)
    {
        depends[define]++;
    }

    void setDefine(const std::string& name, const std::string& value)
    {
        defines[name] = value;
    }

    void unsetDefine(const std::string& name)
    {
        defines.erase(name);
    }

    bool getValue(const std::string name, std::string& value)
    {
        std::map<std::string, std::string>::iterator i = defines.find(name);
        if (i != defines.end())
        {
            value = i->second;
            return true;
        }
        return false;
    }

    bool isSet(const std::string name)
    {
        std::string value;
        return getValue(name, value);
    }

    void loadDefines(std::map<std::string, std::string> map)
    {
        defines.insert(map.begin(), map.end());
    }

};

class LanAB_Context
{
public:
	void* scanner;
	int result;
	int esc_depth;
    int preprocessor_result;
    std::istringstream* is;

    definelist defines;
    int level_on;
    int level_off;

    int line;

    creax::threadfifo<text_type>& input_fifo;
    creax::threadfifo<code_piece>& output_fifo;

    LanCD_Context* cdcontext;

public:
    LanAB_Context(creax::threadfifo<text_type>& a_input_fifo, creax::threadfifo<code_piece>& a_output_fifo)
        : is(NULL), input_fifo(a_input_fifo), output_fifo(a_output_fifo), cdcontext(NULL)
	{
		init_scanner();
        level_on = 0;
        level_off = 0;
        line = 0;
	}

    void setCDContext(LanCD_Context* a_context)
    {
        cdcontext = a_context;
    }

	virtual ~LanAB_Context()
	{
		destroy_scanner();
	}

    void text(const std::string& code)
    {
        if (level_off == 0)
            output_fifo.push_data(code_piece(code, line + getLineNo()));
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

    void yy_input(char* const  buf, int& result, const int max_size, const int eof_result)
    {
        bool done = false;
        while (!done)
        {
            if (is == NULL)
            {
                text_type buffer;
                do {
                    if (! input_fifo.pop_data(buffer))
                    {
                        result = eof_result; // EOF
                        return;
                    }
                } while (buffer.type != LanComment_Context::TTCODE);

                is = new std::istringstream(buffer.text);
                line = buffer.line - getLineNo();
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
    int getLineNo();

// Defined in LanAB.l
protected:
	void init_scanner();	
	void destroy_scanner();
};

int LanAB_parse(LanAB_Context*);

#endif // LANAB_CONTEXT
