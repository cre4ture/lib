#ifndef XMLWRITER_H
#define XMLWRITER_H

#include <ostream>
#include <sstream>
#include <stack>
#include <stdexcept>

class xmlwriter
{
private:
    std::ostream& ostr;
    bool attrs;
    std::stack<std::string> tags;
    std::string spaces;

public:
    xmlwriter(std::ostream& output)
        :ostr(output)
    {
        attrs = false;
    }

    void beginTag(const std::string name)
    {
        if (attrs)
            ostr << ">" << std::endl;

        ostr << spaces << "<" << name;
        attrs = true;
        tags.push(name);
        spaces.push_back(' ');
    }

    void endTag(const std::string name)
    {
        if (tags.empty())
            throw std::runtime_error("xmlwriter::endTag(): all tags are closed. Tried to close tag: " + name);
        if (tags.top() != name)
            throw std::runtime_error("xmlwriter::endTag(): name differs! Tried to close tag: " + name + " expected: " + tags.top());

        tags.pop();
        spaces.resize(spaces.size()-1);

        if (attrs)
        {
            ostr << "/>" << std::endl;
            attrs = false;
        }
        else
        {
            ostr <<  spaces << "</" << name << ">" << std::endl;
        }
    }

    void addAttribute(const std::string& name, const std::string& value)
    {
        if (!attrs)
            throw std::runtime_error("xmlwriter::addAttribute(): Failed to add attribute: " + name + ". Starttag not open!");

        ostr << " " << name << "=\"" << value << "\"";
    }

    void addAttribute(const std::string &name, const int value)
    {
        std::ostringstream ostr;
        ostr << value;
        addAttribute(name, ostr.str());
    }
};

#endif // XMLWRITER_H
