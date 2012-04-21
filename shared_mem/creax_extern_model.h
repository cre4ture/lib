#ifndef CREAX_EXTERN_MODEL_H
#define CREAX_EXTERN_MODEL_H

#include <string>
#include <string.h>
#include <memory>
#include <vector>
#include "../compiler_export/Sprachumfang/creax_fd.h"
#include "../compiler_export/Sprachumfang/auto_ptr_vector.h"

typedef size_t ResID;

#define DEFAULT_REGION_SIZE   (1024*1024*5)

class shared_mem_stack
{
private:
    std::string name;
    bool read_only;
    size_t map_size;
    creax::fd* map_fd;
    void* map_ptr;
    std::vector<size_t> stack;
    bool master;

    void* getMemPtr(size_t pos)
    {
        void* result = map_ptr;

        for (size_t i = 0; i < pos; i++)
        {
            result = (char*)result + stack[i];
        }

        return result;
    }

    void* register_element(size_t size)
    {
        size_t pos = stack.size();
        stack.push_back(size);
        void* result = getMemPtr(pos);
        // check size
        if (((size_t)result - (size_t)map_ptr) + size > map_size)
            throw std::runtime_error("extern_interface::register_element(): region size to small!");
        return result;
    }

public:
    shared_mem_stack(std::string& a_name, size_t a_size);

    ~shared_mem_stack();

    void map_memory(bool a_read_only, bool a_master);

    void setMaster(bool is_master)
    {
        master = is_master;
    }

    void* mallocShared(size_t size)
    {
        return register_element(size);
    }

    template<class Tp, class P1, class P2>
    Tp* createShared(P1 param1, P2 param2)
    {
        return new (register_element(sizeof(Tp))) Tp(param1, param2);
    }

    template<class Tp, class P1>
    Tp* createShared(P1 param1)
    {
        return new (register_element(sizeof(Tp))) Tp(param1);
    }

    template<class Tp>
    Tp* createShared()
    {
        return new (register_element(sizeof(Tp))) Tp;
    }
};

class child_process_interface
{
private:
    __pid_t child_pid;

public:
    child_process_interface();

    bool start(); // returns if child or not
    bool wait_stop(); // wait till module terminates

    bool isChild()
    {
        return (child_pid == 0);
    }

    __pid_t getChildPID()
    {
        return child_pid;
    }
};

class extern_module
{
private:
    child_process_interface& iface;
    mefu::auto_ptr_vector<shared_mem_stack> shared_mem;

    struct registered_shared_mem
    {
        size_t size;
        std::string name;
        bool write_by_parent;
        bool write_by_child;
    };

    std::vector<registered_shared_mem> registered_mem;

public:
    extern_module(child_process_interface& a_iface)
        : iface(a_iface)
    {}

    size_t register_shared_mem(const std::string &name, size_t size, bool write_by_parent, bool write_by_child);

    shared_mem_stack& getSharedMem(size_t index)
    {
        return *shared_mem[index];
    }

    // inits stack for data readable by parent and child
    // this is done before child was started
    virtual void initSharedDataBefore();

    // inits stack for data going to child (readonly for child)
    // or data going to parent (readonly for parent)
    // this is done after child was started
    virtual void initSharedDataAfter();

    void start()
    {
        initSharedDataBefore();
        iface.start();
        initSharedDataAfter();

        if (iface.isChild())
        {
            // child
        }
        else
        {
            // parent
        }
    }

};

class extern_module_mgr
{
private:
    mefu::auto_ptr_vector<extern_module> modules;

public:
    extern_module_mgr()
    {}

    ~extern_module_mgr()
    {}
};

#endif // CREAX_EXTERN_MODEL_H
