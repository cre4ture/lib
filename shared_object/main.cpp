#include <iostream>

#include "shared_object.h"
#include <cstring>

#define MYTEXT_MAX_LEN 250

struct mytext
{
    char text[MYTEXT_MAX_LEN];

    mytext(const char* a_text)
    {
        strncpy(text, a_text, MYTEXT_MAX_LEN);
    }
};

class shared_test
{
public:
    int sayHello()
    {
        std::cout << "Hello Object from " << getpid() << std::endl;
        return 1234;
    }

    int sayHello2(const mytext name)
    {
        std::cout << "Hello " << name.text << " from " << getpid() << std::endl;
        return 1234;
    }
};

using namespace std;

#define SHARED_TEST true

int main()
{
    cout << "Hello World from " << getpid() << endl;

    shared_object<shared_test, SHARED_TEST, 300>* obj = new shared_object<shared_test, SHARED_TEST, 300>();
    obj->startSlave();
    int result;
    //result = obj.sayHello();
    result = obj->call_function_0<int>(&shared_test::sayHello);

    std::string name;

    std::cin >> name;

    obj->call_function_1<int>(&shared_test::sayHello2, mytext(name.c_str()));

    cout << "return value: " << result << std::endl;

    delete obj;

    return 0;
}

