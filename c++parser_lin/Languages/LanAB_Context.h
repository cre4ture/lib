#ifndef LANAB_CONTEXT
#define LANAB_CONTEXT

#include <iostream>
#include "ast_nodes_func.hpp"
#include "ast_nodes_impl.hpp"

#include "LanComment_Context.h"

#include "LanCD_Context.h"

#include "basic_types.h"

#include "ParamParser.h"

#include "cpp_parser.h"

enum condition_type
{
    cot_defined,
    cot_variable,
    cot_macro,
    cot_const,
    cot_add,
    cot_sub,
    cot_not,
    cot_and,
    cot_or,
    cot_eq,
    cot_gt,
    cot_st,
    cot_ge,
    cot_se
};

class condition
{
public:
    std::string name;
    condition_type type;
public:
    std::vector<condition*> prms;

    condition(condition_type _type, std::string _name, std::vector<condition*>* _prms = NULL)
        : name(_name), type(_type)
    { if (_prms != NULL) prms.assign(_prms->begin(), _prms->end()); }
    condition(condition_type _type, condition* _child1)
        : name(""), type(_type)
    { prms.push_back(_child1); }
    condition(condition_type _type, condition* _child1, condition* _child2)
        : name(""), type(_type)
    { prms.push_back(_child1); prms.push_back(_child2); }
};

inline int readIntFromString(const char* str, const char* trim_start = " \t\n\r")
{
    // skip all chars from ignore at begining of string
    while ((str[0] != 0) && (strchr(trim_start, str[0]) != NULL)) str++;

    // sign?
    bool negative;
    switch (str[0])
    {
    case '-':
        negative = true;
        str++;
        break;
    case '+':
        negative = false;
        str++;
        break;
    }

    // buffer number:
    std::string tmp;
    for (char c = str++[0]; c != 0; c = str++[0])
    {
       if ((c >= '0') && (c <= '9'))
       {
           tmp += c;
       }
       else
       {
           break;
       }
    }

    return atoi(tmp.c_str()) * (negative ? -1 : 1);
}

class LanAB_Context
{
public:
	void* scanner;
	int esc_depth;
    int preprocessor_result;
    std::istringstream* is;

private:
    definelist& defines;

public:
    int level_on;
    int level_off;
    std::vector<int> if_level_stack;

    int startline;
    int additional_lines;

    // for defines:
    int define_line_count;

    creax::threadfifo<text_type>& input_fifo;
    creax::threadfifo<code_piece>& output_fifo;

    LanCD_Context* cdcontext;

    cpp_parser* parent;

public:
    LanAB_Context(creax::threadfifo<text_type>& a_input_fifo,
                  int a_startline,
                  creax::threadfifo<code_piece>& a_output_fifo,
                  cpp_parser* _parent)
        : is(NULL),
          input_fifo(a_input_fifo),
          output_fifo(a_output_fifo),
          cdcontext(NULL),
          parent(_parent),
          defines(_parent->defines)
	{
		init_scanner();
        level_on = 0;
        level_off = 0;
        startline = a_startline-1;
        additional_lines = 0;
        define_line_count = 0;
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
        {
            std::string add;
            while (additional_lines > 0)
            {
                add += "\n";
                additional_lines--;
            }

            output_fifo.push_data(code_piece(add + code, startline + getLineNo()));
        }
        else
        {
            // only take the newlines!
            const char* c = code.c_str();
            while ((*c) != 0)
            {
                if ((*c) == '\n')
                    additional_lines++;
                c++;
            }
        }
    }

    void endline()
    {
        additional_lines++;
    }

    void define(const std::string& name, const std::string& value)
    {
        if (level_off > 0) return;

        defines.setDefine(name, value);
        std::string replacement;
        for (int i = 0; i < define_line_count; i++)
        {
            replacement += "\n";
        }
        text(replacement);
        define_line_count = 0;
    }

    void ident(const std::string& name)
    {
        // defines.addDependency(name); TODO: shall or shall we not ?
        if (defines.isSet(name))
        {
            // replace with defined value:
            std::string value;
            defines.getValue(name, value);
            text(value);
            // TODO MACROS!
        }
        else
        {
            // treat like normal text:
            text(name);
        }
    }

    void undef(const std::string& name)
    {
        if (level_off > 0) return;

        defines.unsetDefine(name);
    }

    void if_def(const std::string& name, bool not_def = false)
    {
        if_level_stack.push_back(level_on + level_off);

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
            level_off = 0; // --
            level_on++;
        }
        else // level_off == 0
        {
            level_on--;
            level_off = 1; // ++
        }
    }

    void end_if()
    {
        while ((level_on + level_off) > (*if_level_stack.rbegin()))
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

        if_level_stack.pop_back();
    }

    void if_condition(condition* cond, bool from_elif = false)
    {
        if (!from_elif)
            if_level_stack.push_back(level_on + level_off);

        if (level_off > 0)
        {
            level_off++;
        }
        else
        {
            int result = evaluate_condition(cond);
            if (result != 0)
            {
                level_on++;
            }
            else
            {
                level_off++;
            }
        }
    }

    void elif_condition(condition* cond)
    {
        else_if();
        if_condition(cond, true);
    }

    int evaluate_condition(condition* cond)
    {
        switch (cond->type)
        {
        case cot_defined:
            return defines.isSet(cond->name);
        case cot_variable:
            {
                std::string value;
                if (defines.getValue(cond->name, value))
                {
                    return readIntFromString(value.c_str());
                }
                return 0;
            }
        case cot_macro: return false; // TODO: implement macro feature!
        case cot_const: return readIntFromString(cond->name.c_str());

        case cot_add: return evaluate_condition(cond->prms[0]) +  evaluate_condition(cond->prms[1]);
        case cot_sub: return evaluate_condition(cond->prms[0]) -  evaluate_condition(cond->prms[1]);
        case cot_not: return ! evaluate_condition(cond->prms[0]);
        case cot_and: return evaluate_condition(cond->prms[0]) && evaluate_condition(cond->prms[1]);
        case cot_or:  return evaluate_condition(cond->prms[0]) || evaluate_condition(cond->prms[1]);
        case cot_eq:  return evaluate_condition(cond->prms[0]) == evaluate_condition(cond->prms[1]);
        case cot_gt:  return evaluate_condition(cond->prms[0]) >  evaluate_condition(cond->prms[1]);
        case cot_st:  return evaluate_condition(cond->prms[0]) <  evaluate_condition(cond->prms[1]);
        case cot_ge:  return evaluate_condition(cond->prms[0]) >= evaluate_condition(cond->prms[1]);
        case cot_se:  return evaluate_condition(cond->prms[0]) <= evaluate_condition(cond->prms[1]);
        default:
            throw std::runtime_error("internal error: unknown preprocessor condition!");
        }
    }

    void error(std::string message)
    {
        if (level_off > 0) return;

        message = "user code error: " + message;
        this->yy_error(message.c_str());
    }

    void yy_input(char* buf, int& result, const int max_size, const int eof_result)
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

                    if (buffer.type == LanComment_Context::TTCODE)
                    {
                        break;
                    }
                    else if (buffer.lines > 0)
                    {
                        buffer.text = "\n";
                        for (int i = 1; i < buffer.lines; i++)
                        {
                            buffer.text += "\n";
                        }
                        break;
                    }

                } while (true);

                is = new std::istringstream(buffer.text);
                // line = buffer.line - getLineNo();
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

    void include(bool is_lib, std::string filename)
    {
        if (level_off > 0) return;

        std::cout << "include: " << filename << std::endl;
        std::string full = parent->searchInclude(is_lib, filename);
        std::ifstream input(full.c_str());
        if (!input.is_open())
            throw std::runtime_error("include file not found: " + filename);
        try
        {
            parent->preprocessor(&input, output_fifo);
        }
        catch (...)
        {
            std::cerr << "error in file " << filename << " included from line " << startline + getLineNo() << std::endl;
            throw;
        }
    }

    void yy_error(const char* err)
    {
        std::ostringstream ostr;
        ostr << "preproc line:" << startline + getLineNo()
             << ",\"" << getYYtext()
             << "\", msg: " << err << std::endl;
        throw std::runtime_error(ostr.str());
    }

    // Defined in LanAB.l
    std::string getYYtext();
    // Defined in LanAB.l
    int getLineNo();

    // Defined in LanAB.l
protected:
	void init_scanner();	
	void destroy_scanner();
};

int LanAB_parse(LanAB_Context*);

#endif // LANAB_CONTEXT
