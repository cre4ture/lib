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
public:
    var_name _var_name;
    decl_end* _decl_end;
    parameter_list* _parameter_list;
    func_decl_end* _func_decl_end;
    atype _type;
    bool is_ctor;
    bool is_dtor;

private:
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
        _decl_end = NULL;
        _parameter_list = NULL;
        _func_decl_end = NULL;
        cleanup_decl();
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

    void create_decl()
    {
        if (is_ctor)
        {
            std::cout << "decl_ctor: ";
            _var_name.fancy(std::cout);
            std::cout << "()" << std::endl;
        }
        else
            if (is_dtor)
            {
                std::cout << "decl_dtor: ";
                _var_name.fancy(std::cout);
                std::cout << "()" << std::endl;
            }
            else
                if (_decl_end->_func_decl_end != NULL)
                {
                    std::cout << "decl_func: ";
                    _type.fancy(std::cout);
                    std::cout << " ";
                    _var_name.fancy(std::cout);
                    std::cout << "()" << std::endl;
                }
                else
                {
                    std::cout << "decl: ";
                    _type.fancy(std::cout);
                    std::cout << " ";
                    _var_name.fancy(std::cout);
                    std::cout << std::endl;
                }
    }

    void cleanup_decl()
    {
        _var_name.namespaces.clear();
        _var_name.name = "";
        _var_name.vector_size.clear();
        if (_decl_end != NULL) delete _decl_end;
        _decl_end = NULL;
        if (_parameter_list != NULL) delete _parameter_list;
        _parameter_list = NULL;
        if (_func_decl_end != NULL) delete _func_decl_end;
        _func_decl_end = NULL;
        _type.is_reference = false;
        _type.name = "";
        _type.namespaces.clear();
        _type.pointer_level = 0;
        if (_type.template_params != NULL) delete _type.template_params;
        _type.template_params = NULL;
        is_ctor = false;
        is_dtor = false;
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
