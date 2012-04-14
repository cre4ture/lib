
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/wait.h>

#include "ast_node_global_def_include.h"
#include "ast_nodes_func.hpp"
#include "xmlparser/html_parser.h"
#include "factory_ast_node.h"
#include "creax_fd.h"

class mmap_file
{
private:
    void* addr;
    size_t size;

public:
    mmap_file(int fd, size_t size)
    {
        addr = mmap(NULL, size, PROT_READ, MAP_SHARED, fd, 0);
        if (addr == NULL)
            throw std::runtime_error("mmap_file: mmap failed!");
    }

    ~mmap_file()
    {
        munmap(addr, size);
    }

    void* getAddr()
    {
        return addr;
    }
};

ast_node_wurzel* parseCXML(const std::string& filename)
{
    creax::fd fd(filename);  // automatic close at and of function

    struct stat statbuf;
    fd.getStat(statbuf);

    return parseCXML(fd.getFd(), statbuf.st_size);
}

ast_node_wurzel* parseCXML(int fd, size_t size)
{
    mmap_file fmap(fd, size);
    creax::htmlparser xml((const char*)fmap.getAddr());

    xml.parseToNextTag();
    ast_node_wurzel* result = dynamic_cast<ast_node_wurzel*>(factory_ast_node::createFromXML(xml));

    if (result == NULL)
    {
        throw std::runtime_error("Error parsing file: invalid root tag!");
    }

    return result;
}

static bool tryUpdateCXML(std::string origfile, std::string cxmlfile)
{
    if (fork() == 0) { // wenn kind
        execlp("./compiler", "", origfile.c_str(), cxmlfile.c_str(), NULL);
        throw std::runtime_error("fork-child: execlp failed!");
    }
    __WAIT_STATUS status;
    wait(&status);
    return (WEXITSTATUS(status) == 0);
}

ast_node_global_def_include::ast_node_global_def_include(const std::string a_filename, const bool a_lib, ast_node *parent)
    : ast_node_global_def(parent)
{
    filename = a_filename;
    lib = a_lib;

    int fdorig, fdcxml;
    struct stat statbuforig, statbufcxml;
    std::string cxml = filename + ".cxml";

    fdorig = open(filename.c_str(), O_RDONLY);
    fdcxml = open(cxml.c_str(), O_RDONLY);

    if (fdorig != -1)
    {
        /* find size of input file */
        if (fstat (fdorig,&statbuforig) < 0)
            throw std::runtime_error("fstat error");
    }

    if (fdcxml != -1)
    {
        /* find size of input file */
        if (fstat (fdcxml,&statbufcxml) < 0)
            throw std::runtime_error("fstat error");
    }

    if ((fdorig == -1) && (fdcxml == -1)) // beide dateien nicht da: fehler
        throw std::runtime_error("#include: File not found: " + filename + ", cxml also missing!");
    else
    {
        if ((fdorig != -1) && (fdcxml == -1)) // wenn cxml nicht vorhanden: erstellen und nochmal öffnen!
        {
            close(fdorig);
            if (!tryUpdateCXML(filename, cxml))
            {
                throw std::runtime_error("Failed to create " + cxml + " from " + filename);
            }
            fdcxml = open(cxml.c_str(), O_RDONLY);
            if (fdcxml == -1)
                throw std::runtime_error("#include: File not found after create: " + cxml);
        }
        else
        {
            if ((fdorig != -1) && (fdcxml != -1)) // wenn beide vorhanden
            {
                // TODO check nanosec also:
                if (statbuforig.st_mtim.tv_sec > statbufcxml.st_mtim.tv_sec) // modification time of orig > cxml?
                {
                    close(fdcxml);
                    if (!tryUpdateCXML(filename, cxml))
                    {
                        throw std::runtime_error("Failed to update " + cxml + " from " + filename);
                    }
                    fdcxml = open(cxml.c_str(), O_RDONLY);
                    if (fdcxml == -1)
                        throw std::runtime_error("#include: File not found after update: " + cxml);
                }
                close(fdorig);
            }
            else
            {    // wenn nur cxml da: -> einfach laden und weitermachen!
                // TODO: evtl. warnung ausgeben?
            }
        }

        /* find size of input file */
        if (fstat (fdcxml,&statbufcxml) < 0)
            throw std::runtime_error("fstat error");

        ast_node_wurzel* root = parseCXML(fdcxml, statbufcxml.st_size);
        addChild(root);
    }

    close(fdcxml);
}
