#ifndef CREAX_EXTERN_MODEL_H
#define CREAX_EXTERN_MODEL_H

#include <string>
#include <memory>
#include <vector>
#include "../compiler_export/Sprachumfang/creax_fd.h"

typedef size_t ResID;

#define REGION_SIZE   (1024*1024*5)

class extern_interface
{
public:
    enum MemRegion
    {
        MR_TO_CHILD = 0,
        MR_TO_PARENT = 1,
        MR_SHARED = 2
    };

private:
    std::auto_ptr<creax::fd> mr_fd[3];
    void* memregion[3];
    std::vector<size_t> memStack[3];
    __pid_t pid;

    std::string to_child;
    std::string to_parent;
    std::string shared;

    void* getMemPtr(MemRegion region, size_t pos)
    {
        void* result = memregion[region];
        std::vector<size_t>& st = memStack[region];

        for (size_t i = 0; i < pos; i++)
        {
            result = (char*)result + st[i];
        }

        return result;
    }

    void* register_element(MemRegion region, size_t size)
    {
        std::vector<size_t> &tSt = memStack[region];
        size_t pos = tSt.size();
        tSt.push_back(size);
        void* result = getMemPtr(region, pos);
        // check size
        if (((size_t)result - (size_t)memregion[region]) + size > REGION_SIZE)
            throw std::runtime_error("extern_interface::register_element(): region size to small!");
        return result;
    }

public:

    extern_interface(const std::string& name);

    bool start(); // returns if child or not
    bool wait_stop(); // wait till module terminates

    bool isChild()
    {
        return (pid == 0);
    }

    void* mallocShared(size_t size, MemRegion region = MR_SHARED)
    {
        return register_element(region, size);
    }

    template<class Tp, class P1, class P2>
    Tp* createShared(MemRegion region, P1 param1, P2 param2)
    {
        return new (register_element(region, sizeof(Tp))) Tp(param1, param2);
    }

    template<class Tp, class P1>
    Tp* createShared(MemRegion region, P1 param1)
    {
        return new (register_element(region, sizeof(Tp))) Tp(param1);
    }

    template<class Tp>
    Tp* createShared(MemRegion region = MR_SHARED)
    {
        return new (register_element(region, sizeof(Tp))) Tp;
    }
};

#endif // CREAX_EXTERN_MODEL_H
