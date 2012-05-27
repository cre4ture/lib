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
    std::vector<size_t> vector_size;
public:
    var_name()
    {}
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

class decl
{
public:
    std::vector<std::string> namespaces;
    std::string name;
    decl_end* _decl_end;
    atype* _type;

public:
    decl()
        : _decl_end(NULL),
          _type(NULL)
    {}
};

typedef enum {
    ckw_class,
    ckw_struct
} class_kw;

class LanXX_Context
{
private:
    creax::stringinput* is;
    int startline;
    creax::threadfifo<code_piece>& fifo;

    // for class
    std::string c_name;
    std::vector<std::string> inheritance;

    // for atype
    std::string m_atype_name;
    int m_atype_p_level;

    // for var_name
    std::string m_var_name;

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

    void atype_name(const std::string& name)
    {
        m_atype_name = name;
        m_atype_p_level = 0;
    }

    void atype_add_pointer_level(int levels = 1)
    {
        m_atype_p_level += levels;
    }

    void var_name(const std::string& name)
    {
        m_var_name = name;
    }

    virtual std::string getYYtext() = 0;
    virtual int getLineNo() = 0;

protected:
    virtual void init_scanner() = 0;
    virtual void destroy_scanner() = 0;
};

#endif // PARSER_CLASS_H
