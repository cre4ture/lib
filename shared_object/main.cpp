#include <iostream>

#include "shared_object.h"
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
};

using namespace std;

#define SHARED_TEST true
#define SHARED_PSIZE 300

int main()
{
    cout << "Hello World from " << getpid() << endl;

    // create extern object
    shared_object<shared_test, SHARED_TEST, SHARED_PSIZE>* obj = new shared_object<shared_test, SHARED_TEST, SHARED_PSIZE>();
    obj->startSlave();

    int result;
    //result = obj.sayHello();
    result = obj->call_function<int>(&shared_test::sayHello);

    std::string name;

    std::cin >> name;

    obj->call_function<int>(&shared_test::sayHello2, mytext(name.c_str()));
    obj->call_function<int>(&shared_test::sayHello3, mytext(name.c_str()), 0.7f);

    cout << "return value: " << result << std::endl;

    delete obj;

    return 0;
}

