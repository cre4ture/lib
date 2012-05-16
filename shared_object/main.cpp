#include <iostream>

#include "shared_object_ext.h"
#include <cstring>

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
};

using namespace std;

#define SHARED_TEST true
#define SHARED_PSIZE 300
#define SHARED_SEGCOUNT 4

typedef shared_object_ext<shared_test, SHARED_TEST, SHARED_SEGCOUNT, SHARED_PSIZE> my_shobj;
//typedef shared_object<shared_test, SHARED_TEST, SHARED_PSIZE> my_shobj;

int main()
{
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

    delete obj;

    return 0;
}

