#ifndef BASIC_TYPES_H
#define BASIC_TYPES_H

#include <map>
#include <string>

class code_piece
{
public:
    int line;
    std::string code;

    code_piece()
    {}

    code_piece(const std::string& a_code, int a_line)
        : line(a_line), code(a_code)
    {}

    code_piece(const code_piece& a)
        : line(a.line), code(a.code)
    {}

};

class text_type
{
public:
    std::string text;
    int type;
    int lines;

    text_type()
    {}

    text_type(const std::string& a_text, int a_type, int a_lines)
        : text(a_text), type(a_type), lines(a_lines)
    {}

    text_type(const text_type& a)
        : text(a.text), type(a.type), lines(a.lines)
    {}
};

class definelist
{
private:
    std::map<std::string, std::string> defines;
    std::map<std::string, int> depends;

public:

/*    void saveDependencies(ast_node_define_depencies* dependencies)
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
*/
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

    void loadDefines(const std::map<std::string, std::string>& map)
    {
        defines.insert(map.begin(), map.end());
    }

    void saveDefines(std::map<std::string, std::string>& map)
    {
        map.insert(defines.begin(), defines.end());
    }

};

#endif // BASIC_TYPES_H
