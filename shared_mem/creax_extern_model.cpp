#include "creax_extern_model.h"

#include <stdio.h>
#include <sys/mman.h>
#include <sys/wait.h>

extern_interface::extern_interface(const std::string &name)
{
    to_child = name + "_to_child.txt";
    to_parent = name + "_to_parent.txt";
    shared = name + "_shared.txt";

    { // create and truncate files
        creax::fd a_shared_fd(shared, O_RDWR | O_CREAT);
        creax::fd a_to_child_fd(to_child, O_RDWR | O_CREAT);
        creax::fd a_to_parent_fd(to_parent, O_RDWR | O_CREAT);

        struct stat buf;
        a_shared_fd.getStat(buf);
        if (buf.st_size < REGION_SIZE)
        for (size_t i = 0; i < (REGION_SIZE / sizeof(i)); i++)
            write(a_shared_fd.getFd(), &i, sizeof(i));

        a_to_child_fd.getStat(buf);
        if (buf.st_size < REGION_SIZE)
        for (size_t i = 0; i < (REGION_SIZE / sizeof(i)); i++)
            write(a_to_child_fd.getFd(), &i, sizeof(i));

        a_to_parent_fd.getStat(buf);
        if (buf.st_size < REGION_SIZE)
        for (size_t i = 0; i < (REGION_SIZE / sizeof(i)); i++)
            write(a_to_parent_fd.getFd(), &i, sizeof(i));
    }

    // create full shared mem region directly!
    mr_fd[MR_SHARED].reset(new creax::fd(shared, O_RDWR));
    memregion[MR_SHARED] = mmap(NULL, REGION_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, mr_fd[MR_SHARED]->getFd(), 0);
}

bool extern_interface::start()
{
    pid = fork();
    bool child = isChild();
    if (child)
    {
        mr_fd[MR_TO_PARENT].reset(new creax::fd(to_parent, O_RDWR));
        memregion[MR_TO_PARENT] = mmap(NULL, REGION_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, mr_fd[MR_TO_PARENT]->getFd(), 0);
        mr_fd[MR_TO_CHILD].reset(new creax::fd(to_child, O_RDONLY));
        memregion[MR_TO_CHILD] = mmap(NULL, REGION_SIZE, PROT_READ, MAP_SHARED, mr_fd[MR_TO_CHILD]->getFd(), 0);
    }
    else
    {
        mr_fd[MR_TO_PARENT].reset(new creax::fd(to_parent, O_RDONLY));
        memregion[MR_TO_PARENT] = mmap(NULL, REGION_SIZE, PROT_WRITE, MAP_SHARED, mr_fd[MR_TO_PARENT]->getFd(), 0);
        mr_fd[MR_TO_CHILD].reset(new creax::fd(to_child, O_RDWR));
        memregion[MR_TO_CHILD] = mmap(NULL, REGION_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, mr_fd[MR_TO_CHILD]->getFd(), 0);
    }

    return child;
}

bool extern_interface::wait_stop()
{
    __WAIT_STATUS status;
    wait(&status);
    return (WEXITSTATUS(status) == 0);
}
