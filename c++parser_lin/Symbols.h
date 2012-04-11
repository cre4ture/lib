#ifndef SYMBOLS_H
#define SYMBOLS_H

#include <string>
#include <map>
#include <stdexcept>
#include <stdlib.h>
#include "nr2tokens.h"

enum TypeOfSymbol
{
	st_none,
    st_type,
    st_var,
    st_func
};

class Symbol
{
private:
    const std::string name;

public:
    const std::string& getName()
    {
        return name;
    }

    virtual TypeOfSymbol getSymbolType(void) = 0;
    Symbol(const std::string a_name)
        : name(a_name)
    {
    }
};

class SymbolType: public Symbol
{
public:

    static SymbolType* errInstance(const std::string& name)
    {
        return new SymbolType(name);
    }

    bool isPointer()
    {
        return (pointerLevel() != 0);
    }

    virtual int pointerLevel()
    {
        return 0;
    }

    virtual SymbolType* pointerBaseType()
    {
        return this;
    }

    virtual TypeOfSymbol getSymbolType(void)
	{
		return st_type;
	}

    SymbolType(const std::string a_name)
        : Symbol(a_name)
    {
    }
};

class SymbolTypePtr: public SymbolType
{
private:
    SymbolType* origType;

public:

    virtual int pointerLevel()
    {
        return origType->pointerLevel()+1;
    }

    virtual SymbolType* pointerBaseType()
    {
        return origType->pointerBaseType();
    }

    virtual TypeOfSymbol getSymbolType(void)
    {
        return st_type;
    }

    SymbolType* getTargetType()
    {
        return origType;
    }

    SymbolTypePtr(SymbolType* a_origType)
        : SymbolType(a_origType->getName() + "*"), origType(a_origType)
    {
    }
};

class SymbolWithType: public Symbol
{
private:
    SymbolType* type;

protected:
    SymbolWithType(const std::string& name)
        : Symbol(name)
    {
        type = new SymbolType("[unknown]");
    }

public:
    SymbolType* getType()
    {
        return type;
    }

    virtual TypeOfSymbol getSymbolType(void)
	{
		return st_var;
	}
    SymbolWithType(const std::string a_name, SymbolType* a_type)
        : Symbol(a_name), type(a_type)
    {
    }
};

class SymbolVar: public SymbolWithType
{
private:
    bool global;

    SymbolVar(const std::string& name)
        : SymbolWithType(name), global(false)
    {}

public:

    static SymbolVar* errInstance(const std::string& name)
    {
        return new SymbolVar(name);
    }

    void setIsGlobal(bool value)
    {
        global = value;
    }

    bool isGlobal()
    {
        return global;
    }

    SymbolVar(const std::string a_name, SymbolType* a_type, bool isGlobal)
        : SymbolWithType(a_name, a_type), global(isGlobal)
    {
    }
};

class SymbolFunc: public SymbolWithType
{
private:
    SymbolFunc(const std::string& a_name)
        : SymbolWithType(a_name)
    {}

public:

    static SymbolFunc* errInstance(const std::string& name)
    {
        return new SymbolFunc(name);
    }

    virtual TypeOfSymbol getSymbolType(void)
	{
		return st_func;
	}

    SymbolFunc(const std::string a_name, SymbolType* a_type)
        : SymbolWithType(a_name, a_type)
    {
    }
};

class operator_list
{
private:
    std::map<std::string, std::map<SymbolType*, std::map<SymbolType*, SymbolFunc*> > > list;
public:
    SymbolFunc* getOperator2Function(std::string name, SymbolType* typ1, SymbolType* typ2)
    {
        SymbolFunc* result = list[name][typ1][typ2];
        if (result == NULL)
            throw std::runtime_error("Operator function not defined: " + typ1->getName() + " " + name + ", " + typ2->getName());
        return result;
    }
    void registerNewOperatorFunction(std::string name, SymbolType* typ1, SymbolType* typ2, SymbolFunc* op_func)
    {
        list[name][typ1][typ2] = op_func;
    }
};


class Symbols
{
private:
	std::map<std::string, Symbol*> symbols;
    std::map<std::string, std::map<SymbolType*, std::map<SymbolType*, SymbolFunc*> > > op2_list;
    Symbols * const parent;

public:

    SymbolFunc* findOperator2Function(std::string name, SymbolType* typ1, SymbolType* typ2)
    {
        SymbolFunc* sym = op2_list[name][typ1][typ2];
        if ((sym == NULL) && (parent != NULL))
        {
            sym = parent->findOperator2Function(name, typ1, typ2);
        }
        return sym;
    }

    void registerNewOperator2Function(std::string name, SymbolType* typ1, SymbolType* typ2, SymbolFunc* op_func)
    {
        SymbolFunc* symb = op2_list[name][typ1][typ2];
        if (symb != NULL)
        {
            throw std::runtime_error("Operator already defined: " + typ1->getName() + " " + name + " " + typ2->getName());
        }
        op2_list[name][typ1][typ2] = op_func;
    }

    Symbols(Symbols * const a_parent)
		: parent(a_parent)
	{}

	Symbols* getParent(void)
	{
		return parent;
	}

	Symbol* find(const std::string &name)
	{
        Symbol* sym = symbols[name];
        if ((sym == NULL) && (parent != NULL))
        {
            sym = parent->find(name);
        }
        return sym;
	}

    template<class SymbolT>
    SymbolT* findSymbolT(const std::string& name)
    {
        SymbolT* result = dynamic_cast<SymbolT*>(find(name));
        if (result == NULL)
        {
            result = SymbolT::errInstance(name);
        }
        return result;
    }

    void addSymbol(Symbol* const newSymbol)
	{
        Symbol* symb = symbols[newSymbol->getName()];
		if (symb != NULL)
		{
            throw std::runtime_error("Symbol already defined: " + newSymbol->getName());
		}
        symbols[newSymbol->getName()] = newSymbol;
	}

    SymbolType* getBuildInType(std::string name)
    {
        Symbol* sym = this->find(name);
        if (sym == NULL)
        {
            throw std::runtime_error("'" + name + "' is not a defined symbol!");
        }
        if (sym->getSymbolType() != st_type)
        {
            throw std::runtime_error("'" + name + "' is not a defined type!");
        }
        return (SymbolType*)sym;
    }

};

#endif
