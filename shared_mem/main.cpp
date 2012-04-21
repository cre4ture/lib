#include <iostream>
#include "creax_extern_model.h"

#include <string.h>
#include <opencv2/highgui/highgui.hpp>
#include <opencv2/imgproc/imgproc.hpp>

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
        child_process_interface iface;
        extern_module module(iface);
        module.register_shared_mem("mem_test1",DEFAULT_REGION_SIZE,true,true);
        shared_mem_stack& sh_stack = module.getSharedMem(0);
        creax::mutex* mM = sh_stack.createShared<creax::mutex>();
        int* a_sh_p = sh_stack.createShared<int>();
        msg_t* msg = sh_stack.createShared<msg_t>();
        char* buffer = (char*)sh_stack.mallocShared(640*480);
        cv::Mat img(480,640,CV_8UC3,buffer);
        std::cout << "created shared ressources" << std::endl;
        module.start();
        std::cout << "module started" << std::endl;

        volatile int& a_sh = *a_sh_p;
        a_sh = 3;

        if (iface.isChild()) // returns if child or not
        {
            // child
            std::cout << "Child" << std::endl;
            a_sh = a_sh * 3 + 1;
            msg->setMsg("HALLO VON GAST!");
            std::cout << "Child result: " << a_sh << std::endl;

            cv::VideoCapture cam(0);
            for (int i = 0; i < 10; i++)
            {
                cv::waitKey(30);
                cv::Mat tmp;
                cam.read(tmp);
                tmp.copyTo(img);
            }
            //cv::imshow("test_child",img);
            //cv::waitKey(0);
        }
        else
        {
            // parent
            std::cout << "Parent" << std::endl;
            iface.wait_stop();
            std::cout << "Parent result: " << a_sh << ", msg: " << msg->txt << std::endl;
            cv::imshow("test_parent",img);
            cv::waitKey(0);
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

