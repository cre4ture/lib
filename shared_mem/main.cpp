#include <iostream>
#include "creax_extern_model.h"

#include <string.h>

#include "../compiler_export/Sprachumfang/creax_mutex.h"

#define MSG_T_SIZE 100

struct msg_t
{
    char txt[MSG_T_SIZE];

    void setMsg(const char* text)
    {
        strncpy(txt, text, MSG_T_SIZE);
    }
};

int main()
{
    try
    {
        extern_interface module("mod_test");
        std::cout << "module started" << std::endl;
        creax::mutex* mM = module.createShared<creax::mutex>();
        int* a_sh_p = module.createShared<int>();
        msg_t* msg = module.createShared<msg_t>();
        char* buffer = module.mallocShared(640*480);
        std::cout << "created shared ressources" << std::endl;

        volatile int& a_sh = *a_sh_p;
        a_sh = 3;

        if (module.start()) // returns if child or not
        {
            // child
            std::cout << "Child" << std::endl;
            a_sh = a_sh * 3 + 1;
            msg->setMsg("HALLO VON GAST!");
            std::cout << "Child result: " << a_sh << std::endl;
        }
        else
        {
            // parent
            std::cout << "Parent" << std::endl;
            module.wait_stop();
            std::cout << "Parent result: " << a_sh << ", msg: " << msg->txt << std::endl;
        }

    }
    catch (std::exception& e)
    {
        std::cerr << "exception: " << e.what() << std::endl;
        return -1;
    }
    catch (...)
    {
        std::cerr << "unknown exception occured" << std::endl;
        return -1;
    }

    return 0;
}

