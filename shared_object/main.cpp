#include <iostream>

#include "shared_object_ext.h"
#include <cstring>

#include <opencv2/highgui/highgui.hpp>
#include <opencv2/imgproc/imgproc.hpp>

#include <signal.h>

#define MYTEXT_MAX_LEN 250

struct mytext
{
    char text[MYTEXT_MAX_LEN];

    mytext(const char* a_text)
    {
        assign(a_text);
    }

    void assign(const char* a_text)
    {
        strncpy(text, a_text, MYTEXT_MAX_LEN);
    }

    const char* operator ()()
    {
        return text;
    }
};

class shared_test
{
private:
    mytext hello;

public:

    shared_test()
        :hello("HELLO")
    {}

    int sayHello()
    {
        std::cout << hello() << " Object from " << getpid() << std::endl;
        return 1234;
    }

    int sayHello2(const mytext name)
    {
        std::cout << hello() << " " << name.text << " from " << getpid() << std::endl;
        return 1234;
    }

    int sayHello3(const mytext name, float f)
    {
        std::cout << hello() << " " << f << " : " << name.text << " from " << getpid() << std::endl;
        return 1234;
    }

    int readText(mytext* text)
    {
        std::cout << hello() << " please type some message to " << getpid() << std::endl;
        std::string buffer;
        std::cin >> buffer;
        text->assign(buffer.c_str());
        return 0;
    }

    int captureImage(cv::Mat buffer)
    {
        cv::VideoCapture cap(0);
        cv::Mat frame;
        cap >> frame;
        usleep(200000);
        cap >> frame;
        frame.copyTo(buffer);
        return 0;
    }
};

struct sigaction old_act;
void sigchld_hanlder(int signum, siginfo_t* info, void* ctx)
{
    std::cout << "child process terminated" << std::endl;
}

using namespace std;

#define SHARED_TEST true
#define SHARED_PSIZE 300
#define SHARED_SEGCOUNT 4

typedef shared_object_ext<shared_test, SHARED_TEST, SHARED_SEGCOUNT, SHARED_PSIZE> my_shobj;
//typedef shared_object<shared_test, SHARED_TEST, SHARED_PSIZE> my_shobj;

int main()
{
    struct sigaction action;
    action.sa_sigaction = &sigchld_hanlder;
    action.sa_flags = SA_SIGINFO;
    sigaction(SIGCHLD, &action, &old_act);

    cout << "Hello World from " << getpid() << endl;

    // create extern object
    my_shobj* obj = new my_shobj();
    obj->startSlave();

    int result;
    //result = obj.sayHello();
    result = obj->call_function<int>(&shared_test::sayHello);
    cout << "return value: " << result << std::endl;

    std::string name;

    std::cin >> name;

    obj->call_function<int>(&shared_test::sayHello2, mytext(name.c_str()));
    obj->call_function<int>(&shared_test::sayHello3, mytext(name.c_str()), 0.7f);

    mytext* data = (mytext*)obj->createSharedDataSegment(sizeof(mytext), false, false);

    obj->call_function<int>(&shared_test::readText, data);

    std::cout << "read from slave: " << data->text << std::endl;

    char* img_data = obj->createSharedDataSegment(640*480*3, true, false);
    cv::Mat img(480, 640, CV_8UC3, img_data);

    obj->call_function(&shared_test::captureImage, img);

    cv::imshow("new", img);

    cv::waitKey(0);

    delete obj;

    return 0;
}

