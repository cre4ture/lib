#ifndef CPP_PARSER_H
#define CPP_PARSER_H

#include <iostream>
#include <map>
#include <set>
#include <vector>

class cpp_parser
{
private:
    std::map<std::string,std::string> defines;
    std::string workdir;

public:
    std::set<std::string> src_includes;
    std::set<std::string> lib_includes;

    cpp_parser(const std::string& a_workdir);

    void setDefines(std::map<std::string,std::string>& a_defines);
    std::map<std::string,std::string>& getDefines();

    void addIncludePaths(const std::set<std::string>& includes);
    void addLibSearchPaths(const std::set<std::string> &includes);
    void addLibSearchPaths(const std::vector<std::string> &includes);

    std::string searchInclude(bool is_lib, const std::string& search_filename);

    bool parse_stream(std::istream *const input);
    bool parse_file(const std::string& filename);

    std::string getWorkdir() { return workdir; }
};

#endif // CPP_PARSER_H
