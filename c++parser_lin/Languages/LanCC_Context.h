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

enum visibility_t
{
    vis_private,
    vis_protected,
    vis_public
};

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
    bool decl_virtual;
    bool decl_static;
    // current visibility while parsing
    visibility_t visibility;

    LanCC_Context(creax::threadfifo<code_piece>& a_fifo, int a_line, const std::string& a_namespace, cpp_parser* a_parent)
        : LanXX_Context(a_fifo, a_line, a_parent), m_namespace(a_namespace)
	{
        init_scanner();
        wurzel = NULL;
        is = NULL;
        symbContext = NULL;
        beginNewSymbContext();
        m_namespace = a_namespace;
        visibility = vis_private;
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

    void c_constructor(atype* t, parameter_list* pl, func_decl_end* fe)
    {
        std::cout << "c_constructor: " << t->name << "()" << std::endl;
    }

    void c_destructor(const std::string& name, parameter_list* pl, func_decl_end* fe)
    {
        std::cout << "c_destructor: ~" << name << "()" << std::endl;
    }

    void c_operator(atype* t, decl_operator_end* oe)
    {
        std::cout << "c_operator: ";
        t->fancy(std::cout);
        std::cout << oe->name << "(";
        std::cout << ")" << std::endl;
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
