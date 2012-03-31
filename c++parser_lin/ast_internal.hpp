#ifndef ASM_INTERNAL_HPP
#define ASM_INTERNAL_HPP

#include "ast_node_types.hpp"
#include "ast_nodes_impl.hpp"

#include <map>
#include <vector>
#include <stdexcept>
#include <sstream>

class type
{
public:
    std::string name;
    type(std::string a_name)
        :name(a_name)
    {}

    virtual type* getAliasType()
    { return this; }
};

class type_buildin_int: public type
{
private:
    int bytes;
    int signed_value;
public:
    type_buildin_int(std::string name, int a_bytes, bool a_signed)
        :type(name), bytes(a_bytes), signed_value(a_signed)
    {}
};

class type_buildin_float: public type
{
private:
    int bytes;
};

class type_pointer: public type
{
    type_pointer(type* pointsTo)
        :type(pointsTo->name + "*")
    {}
};

class type_alias: public type
{
private:
    type* const alias;
    const bool explicit_cast;

public:
    type_alias(std::string a_name, type* a_alias, bool a_explicit_cast = true)
        :type(a_name), alias(a_alias), explicit_cast(a_explicit_cast)
    {}

    virtual type* getAliasType()
    {
        if (explicit_cast)  // type has to be explicitly casted into is alias?
            return this;
        else
            return alias;
    }
};

class variable
{
	public:
		std::string name;
		int size;
		bool no_code;

		variable(std::string a_name, int a_size, bool a_no_code)
			:name(a_name), size(a_size), no_code(a_no_code)
		{}
};

class id_store
{
public:
    virtual void compile_address(std::string name) = 0;
};

class global_vars: public id_store
{
private:
    std::map<std::string, ast_node_constIntList*> globals;

public:
    void addGlobal(std::string name, ast_node_constIntList* initValue)
    {
		globals[name] = initValue;
    }
    
    virtual void compile_address(std::string name)
    {
        ast_node_constIntList* node = globals[name];
		if (node == NULL)
            throw std::runtime_error("global variable not defined!");
        // set A to address
		fprintf(compile_output, "\tOR  A, 0, GVAR_%s\t\t; A = &%s\n", name.c_str(), name.c_str());
    }

    void defineGlobals()
    {
        for (std::map<std::string, ast_node_constIntList*>::const_iterator i = globals.begin(); i != globals.end(); i++)
        {
			std::string name = i->first;
			fprintf(compile_output, "GVAR_%s:\t\t\t; global var %s \n", name.c_str(), name.c_str());
			i->second->compile_value();
        }
    }

};

class context: public id_store
{
private:
    context* parent_context;
    std::vector<variable> locals;
    std::string root_name;

    void incStack()
    {
        fprintf(compile_output, "\tADD Y, Y, %d\t\t; increment stack\n", getSize());
    }

    void decStack()
    {
        fprintf(compile_output, "\tSUB Y, Y, %d\t\t; decrement stack\n", getSize());
    }

public:
    context(context* parent, std::string a_root_name)
        : parent_context(parent), root_name(a_root_name)
    {
        if (parent != NULL) parent->incStack();
    }

    ~context()
    {
        if (parent_context != NULL) parent_context->decStack();
    }

    context* getParent()
    {
        return parent_context;
    }

    int push_variable(std::string name, int size)
    {
        int offset = getSize();
		locals.push_back(variable(name, size, false));
        return offset;
    }

    void pop_variable(std::string name)
    {
        if (locals.size() == 0)
        {
            throw std::runtime_error("keine variablen mehr auf dem stack!");
        }

        if (locals.rbegin()->name != name)
        {
            throw std::runtime_error("variablenname falsch!");
        }

        locals.pop_back();
    }

    int getOffsetOf(std::string name)
    {
        int offset = 0;

        // first search locals
        for (size_t i = 0; i < locals.size(); i++)
        {
            if (locals[i].name == name)
            {
                return offset;
            }
            offset += locals[i].size;
        }

        // then search parent
        if (parent_context != NULL)
        {
            offset = parent_context->getOffsetOf(name) - parent_context->getSize();
            return offset;
        }
        else
        {
            // variable nicht gefunden!
            throw std::runtime_error("Variable not found: " + name + "!");
        }
    }

    int getSize()
    {
        int size = 0;
        for (size_t i = 0; i < locals.size(); i++)
        {
            size += locals[i].size;
        }
        return size;
    }

    virtual void compile_address(std::string name)
    {
        // get address
        int offset = this->getOffsetOf(name);
		fprintf(compile_output, "\tADD A, Y, %d\t\t; A = &%s\n", offset, name.c_str());
    }
};

class function
{
private:
	std::string name;
    SymbolType* type;
	context* params;
public:
    function(std::string a_name, SymbolType* a_type, context* a_params)
		: name(a_name), type(a_type), params(a_params)
	{}

    SymbolType* getResultType()
	{
		return type;
	}
};

class func_list
{
private:
	std::map<std::string, function*> funcs;

public:

	void addFunction(std::string name, function* func)
	{
		funcs[name] = func;
	}

	function* getFunction(std::string name)
	{
		function* result;
		result = funcs[name];
		if (result == NULL)
			throw new std::runtime_error("function not defined!");
		return result;
	}
};

class operator_list_internal
{
private:
    std::map<std::string, std::map<type*, std::map<type*, function*> > > list;
public:
    function* getOperator2Function(std::string name, type* typ1, type* typ2)
    {
        function* result = list[name][typ1->getAliasType()][typ2->getAliasType()];
        if (result == NULL)
            throw std::runtime_error("Operator function not defined: " + typ1->name + " " + name + ", " + typ2->name);
        return result;
    }
    void registerNewOperatorFunction(std::string name, type* typ1, type* typ2, function* op_func)
    {
        list[name][typ1->getAliasType()][typ2->getAliasType()] = op_func;
    }
};

extern context* locals;
extern global_vars* globals;
extern func_list* functions;
extern int config_vec_elements;

extern std::string newLabel(std::string name);
extern void beginNewContext();
extern void endContext();

enum hicovec_type
{
    HT_VOID,
    HT_INT,
    HT_VECTOR
};

inline hicovec_type convHicoVecType(SymbolType* typ)
{
    if (typ->isPointer())
    {
        return HT_INT;
    }

    std::string typname = typ->getName();
    if (typname == "int")
    {
        return HT_INT;
    }
    if (typname == "int_vec")
    {
        return HT_VECTOR;
    }
    if (typname == "void")
    {
        return HT_VOID;
    }

    throw std::runtime_error("Unknown Type!");
}

inline int getSizeOfType(hicovec_type typ)
{
    switch (typ)
    {
        case HT_VOID:
            return 0;
        case HT_INT:
            return 1;
        case HT_VECTOR:
            return config_vec_elements;
    }
    throw std::runtime_error("Unknown Type!");
}

#endif
