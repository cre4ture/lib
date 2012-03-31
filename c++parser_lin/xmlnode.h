#ifndef XMLNODE_H
#define XMLNODE_H

#include <string>
#include "auto_ptr_vector.h"

class xmlnode
{
private:
    mefu::auto_ptr_vector<xmlnode> childs;

    addChild(xmlnode* child)
    {
        childs.push_back(child);
    }

public:
    xmlnode(std::string name, xmlnode* parent)
    {
        parent->addChild(this);
    }



};

#endif // XMLNODE_H
