#ifndef CPP_PARSER_H
#define CPP_PARSER_H

#include <iostream>
#include <map>

class cpp_parser
{
private:
    std::map<std::string,std::string> defines;
    std::string workdir;

public:
    cpp_parser(const std::string& a_workdir);

    void setDefines(std::map<std::string,std::string> a_defines);

    void parse_stream(std::istream *const input);

    void parse_file(const std::string& filename);
};

#endif // CPP_PARSER_H
