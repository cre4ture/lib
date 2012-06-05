#ifndef CPP_PARSER_H
#define CPP_PARSER_H

#include <iostream>
#include <map>
#include <set>
#include <vector>

#include "creax_threadfifo.h"
#include "Languages/basic_types.h"

class cpp_parser
{
public:
    definelist defines;
    std::string workdir;

public:
    std::set<std::string> src_includes;
    std::set<std::string> lib_includes;

    cpp_parser(const std::string& a_workdir);

    void setDefines(const std::map<std::string,std::string>& a_defines);
    void getDefines(std::map<std::string,std::string>& map);

    void addIncludePaths(const std::set<std::string>& includes);
    void addLibSearchPaths(const std::set<std::string> &includes);
    void addLibSearchPaths(const std::vector<std::string> &includes);

    std::string searchInclude(bool is_lib, const std::string& search_filename);

    // does not close fifo at end
    void preprocessor(std::istream *const input, creax::threadfifo<code_piece> output_fifo);

    bool parse_stream(std::istream *const input);
    bool parse_file(const std::string& filename);

    std::string getWorkdir() { return workdir; }
};

#endif // CPP_PARSER_H
