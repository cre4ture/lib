#ifndef PARSER_CLASS_H
#define PARSER_CLASS_H

#include "creax_stringinput.h"
#include "creax_threadfifo.h"
#include "basic_types.h"

#include <iostream>

class atype;

typedef std::vector<atype*> atype_list;

class atype
{
public:
    std::vector<std::string> namespaces; // namespace::name
    std::string name; // name
    atype_list* template_params; // name<params>
    size_t instances; // name[nummer]
    size_t pointer_level; // name *
    bool is_reference;   // name &
public:
    atype()
        : template_params(NULL),
          instances(1),
          pointer_level(0),
          is_reference(false)
    {}
};

typedef std::vector<atype*> atype_list;

class var_name
{
public:
    std::string name;
    std::vector<int> vector_size;
public:
    var_name()
    {}

    void fancy(std::ostream& ostr)
    {
        ostr << name;
        for (size_t i = 0; i < vector_size.size(); i++)
        {
            if (vector_size[i] == -1)
            {
                ostr << "[" << "]";
            }
            else
            {
                ostr << "[" << vector_size[i] << "]";
            }
        }
    }
};

class var_decl
{
public:
    atype* _atype;
    var_name* _var_name;
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

public:
    decl_end()
        : _init_assignment(NULL),
          _parameter_list(NULL),
          _func_decl_end(NULL)
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
        std::cerr << "block<" << m_namespace << ">: line:" << startline + getLineNo()
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
