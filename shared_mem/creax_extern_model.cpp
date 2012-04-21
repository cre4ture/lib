#include "creax_extern_model.h"

#include <stdio.h>
#include <sys/mman.h>
#include <sys/wait.h>

shared_mem_stack::shared_mem_stack(std::string& a_name, size_t a_size)
    : name(a_name),
      read_only(true),
      map_size(a_size),
      map_fd(NULL),
      map_ptr(NULL),
      master(false)
{
    // create file and set size!
    // close file again after that!
    creax::fd a_shared_fd(name, O_RDWR | O_CREAT);
    struct stat buf;
    a_shared_fd.getStat(buf);
    if (buf.st_size < (int)map_size)
    for (size_t i = 0; i < (map_size / sizeof(i)); i++)
        write(a_shared_fd.getFd(), &i, sizeof(i));
}

shared_mem_stack::~shared_mem_stack()
{
    if (map_ptr != NULL)
        munmap(map_ptr, map_size);

    if (map_fd != NULL)
        delete(map_fd);
}

void shared_mem_stack::map_memory(bool a_read_only, bool a_master)
{
    if (map_ptr != NULL)
        throw std::runtime_error("shared_mem_stack::map_memory(): already mapped!");

    read_only = a_read_only;
    master = a_master;

    int fd_flags = 0;
    int map_flags = 0;
    if (read_only)
    {
        fd_flags = O_RDONLY;
        map_flags = PROT_READ;
    }
    else
    {
        fd_flags = O_RDWR;
        map_flags = PROT_READ | PROT_WRITE;
    }

    map_fd = new creax::fd(name, fd_flags);
    map_ptr = mmap(NULL, map_size, map_flags, MAP_SHARED, map_fd->getFd(), 0);

    if (map_ptr == NULL)
        throw std::runtime_error("shared_mem_stack::map_memory():mapping failed!");
}


child_process_interface::child_process_interface()
    : child_pid(1) // init pid != 0 sice we are the parent right now!
{
}

bool child_process_interface::start()
{
    child_pid = fork();
    bool child = isChild();
    return child;
}

bool child_process_interface::wait_stop()
{
    __WAIT_STATUS status;
    wait(&status);
    return (WEXITSTATUS(status) == 0);
}

size_t extern_module::register_shared_mem(const std::string &name, size_t size, bool write_by_parent, bool write_by_child)
{
    size_t result = registered_mem.size();
    registered_shared_mem mem;
    mem.name = name;
    mem.size = size;
    mem.write_by_parent = write_by_parent;
    mem.write_by_child = write_by_child;
    registered_mem.push_back(mem);

    shared_mem_stack* stack = new shared_mem_stack(mem.name, mem.size);
    shared_mem.push_back(stack);
    if (mem.write_by_parent && mem.write_by_child)
        stack->map_memory(false, !iface.isChild());

    return result;
}

void extern_module::initSharedDataBefore()
{
}

void extern_module::initSharedDataAfter()
{
    // do all mappings
    for (size_t i = 0; i < registered_mem.size(); i++)
    {
        registered_shared_mem& mem = registered_mem[i];
        shared_mem_stack* stack = shared_mem[i];

        if (mem.write_by_parent && mem.write_by_child)
            stack->setMaster(!iface.isChild());
        else if (mem.write_by_parent)
            stack->map_memory(iface.isChild(), !iface.isChild());
        else if (mem.write_by_child)
            stack->map_memory(!iface.isChild(), iface.isChild());
        else
            throw std::runtime_error("unsupported memmap style!");
    }
}
