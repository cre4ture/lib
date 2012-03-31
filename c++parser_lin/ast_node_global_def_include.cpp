
#include <sys/mman.h>
#include <fcntl.h>

#include "ast_node_global_def_include.h"
#include "ast_nodes_func.hpp"
#include "xmlparser/html_parser.h"

ast_node_global_def_include::ast_node_global_def_include(const std::string a_filename, const bool a_lib, ast_node *parent)
    : ast_node_global_def(parent)
{
    filename = a_filename;
    lib = a_lib;

    void *addr;
    int fd;
    struct stat statbuf;
    std::string cxml = filename + ".cxml";

    fd = open(cxml.c_str(), O_RDONLY);
    if (fd == -1)
        throw std::runtime_error("#include: File not found: " + cxml);

    /* find size of input file */
     if (fstat (fd,&statbuf) < 0)
        throw std::runtime_error("fstat error");

    addr = mmap(NULL, statbuf.st_size, PROT_READ, MAP_SHARED, fd, 0);

    creax::htmlparser xml((const char*)addr);

    try
    {
        if (!xml.parseToNextTag())
            throw std::runtime_error("empty file!");

        if ((xml.curTagType != creax::tt_StartTag) &&
                (xml.curTagName != "root"))
            throw std::runtime_error("root tag missing!");

        xml.parseToNextTag();

        ast_node_global_defList* gdefList = new ast_node_global_defList(NULL);
        gdefList->parseXML(xml);

        if (!xml.parseToNextTag())
            throw std::runtime_error("last tag missing!");

        if ((xml.curTagType != creax::tt_EndTag) &&
                (xml.curTagName != "root"))
            throw std::runtime_error("closing root tag missing!");

        addChild(gdefList);
    }
    catch (std::runtime_error& e)
    {
        munmap(addr, statbuf.st_size);
        close(fd);

        throw e;
    }

    munmap(addr, statbuf.st_size);
    close(fd);
}
