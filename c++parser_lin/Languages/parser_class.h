#ifndef PARSER_CLASS_H
#define PARSER_CLASS_H

#include "creax_stringinput.h"
#include "creax_threadfifo.h"
#include "basic_types.h"

#include <iostream>
#include <sstream>

class atype;

typedef std::vector<atype*> atype_list;

class atype
{
public:
    std::vector<std::string> namespaces; // namespace::name
    std::string name; // name
    atype_list* template_params; // name<params>
    size_t pointer_level; // name *
    bool is_reference;   // name &
public:
    atype()
        : template_params(NULL),
          pointer_level(0),
          is_reference(false)
    {}

    void fancy(std::ostream& ostr)
    {
        for (size_t i = 0; i < namespaces.size(); i++)
        {
            ostr <<  namespaces[i] << "::";
        }
        ostr << name;
        if (template_params != NULL)
        {
            std::cout << "<>";
        }
        for (size_t i = 0; i < pointer_level; i++)
        {
            ostr << "*";
        }
        if (is_reference)
        {
            ostr << "&";
        }
    }
};

typedef std::vector<atype*> atype_list;

class array_decl
{
public:
    std::vector<int> sizes;
public:
    array_decl(int first)
    {
        sizes.push_back(first);
    }

    void fancy(std::ostream& ostr)
    {
        for (size_t i = 0; i < sizes.size(); i++)
        {
            if (sizes[i] == -1)
            {
                ostr << "[" << "]";
            }
            else
            {
                ostr << "[" << sizes[i] << "]";
            }
        }
    }
};

class var_name
{
public:
    std::string name;
    array_decl* _array_decl;
    std::vector<std::string> namespaces;
public:
    var_name()
        : _array_decl(NULL)
    {}

    void fancy(std::ostream& ostr)
    {
        for (size_t i = 0; i < namespaces.size(); i++)
        {
            ostr << namespaces[i] << "::";
        }
        ostr << name;
        if (_array_decl != NULL)
            _array_decl->fancy(ostr);
    }
};

class var_decl
{
public:
    atype* _atype;
    var_name* _var_name;
    array_decl* _array_decl;
public:
    var_decl()
        : _atype(NULL),
          _var_name(NULL)
    {}
};

typedef var_decl param_decl;

class init_assignment
{
public:

public:
    init_assignment() {}
};

typedef std::vector<param_decl*> parameter_list;

typedef parameter_list parameter_list_intern;

class func_decl_end
{

};

class decl_end
{
public:
    init_assignment* _init_assignment;
    parameter_list* _parameter_list;
    func_decl_end* _func_decl_end;
    array_decl* _array_decl;

public:
    decl_end()
        : _init_assignment(NULL),
          _parameter_list(NULL),
          _func_decl_end(NULL),
          _array_decl(NULL)
    {}
};

class exprA
{
public:
};

typedef exprA exprB;
typedef exprB exprC;
typedef exprC exprD;
typedef exprD exprE;

class expr_op2: public exprE
{
public:
    const char* op_name;
    exprB* op1;
    exprB* op2;
public:
    expr_op2(const char* _op_name, exprB* _op1, exprB* _op2)
        : op_name(_op_name), op1(_op1), op2(_op2)
    {}
};

class expr_constant_int: public exprE
{
public:
    long long value;
public:
    expr_constant_int(long long _value)
        : value(_value)
    {}
};

class expr_addr_op: public exprE
{
public:
    exprA* expr;
public:
    expr_addr_op(exprA* _expr)
        : expr(_expr)
    {}
};

class assignment: public exprA
{
public:
    exprA* lvalue;
    exprA* rvalue;
public:
    assignment(exprA* lval, exprA* rval)
        : lvalue(lval),
          rvalue(rval)
    {}
};

typedef std::vector<exprA*> expr_list;

class symbol_expr: public exprA
{
public:
    std::string name;
    expr_list* _expr_list;
public:
    symbol_expr()
        : _expr_list(NULL)
    {}
};

class LanXX_Context
{
private:
    creax::stringinput* is;
    int startline;
    creax::threadfifo<code_piece>& fifo;

    // for class
    std::string c_name;
    std::vector<std::string> inheritance;

protected:
    std::string m_namespace;
    int blockstart;

public:
    void* scanner;
    int level;

    LanXX_Context(creax::threadfifo<code_piece>& a_fifo, int a_line)
        : fifo(a_fifo)
    {
        startline = a_line-1;
        is = NULL;
    }

    virtual ~LanXX_Context()
    {
    }

    void yy_error(const char* err)
    {
        std::ostringstream ostr;
        ostr << "block<" << m_namespace << ">: line:" << startline + getLineNo()
          << ",\"" << getYYtext()
         << "\", msg: " << err << std::endl;
        throw std::runtime_error(ostr.str());
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
                    is = new creax::stringinput(buffer.code);
                    //line = buffer.line - getLineNo();
                }
                else
                {
                    result = eof_result; // EOF
                    return;
                }
            }

            result = is->read(buf, max_size);
            done = (result != 0);

            if (is->eof())
            {
                delete is;
                is = NULL;
            }
        }
    }

    void class_kw(bool is_struct)
    {
        c_name = "";
        inheritance.clear();
    }

    void class_name(const std::string* name)
    {
        c_name = *name;
    }

    void class_inheritance(const std::string& type, const std::string& name)
    {
        inheritance.push_back(type + " " + name);
    }

    void class_decl_body(const std::string& block_code);

    void block_start()
    {
        blockstart = startline + getLineNo();
    }

    void class_decl(const std::string& name, const std::string& block_code);

    virtual std::string getYYtext() = 0;
    virtual int getLineNo() = 0;

protected:
    virtual void init_scanner() = 0;
    virtual void destroy_scanner() = 0;
};

#endif // PARSER_CLASS_H
