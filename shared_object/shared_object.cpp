#include "shared_object.h"

#include <iostream>
#include <sys/shm.h>
#include <errno.h>
#include <stdexcept>
#include <pthread.h>
#include <cstdlib>

using namespace std;


void * shared_object_base::operator new(unsigned int type_size)
{
    key_t key = 12035;

    int shmid = shmget(key, type_size, IPC_CREAT|0666);
    //int shmid = shmget(key, type_size, 0666);

    if (-1 == shmid )
    {
       throw std::runtime_error("Critical Error while creating shared mem!");
    }

    shared_object_base* new_obj = (shared_object_base*)shmat(shmid,NULL,0);

    if (NULL == new_obj)
    {
        throw std::runtime_error("Critical Error while creating shared mem2!");
    }

    // give allocated object his shmid
    new_obj->m_shmid = shmid;

    std::cout << "created shmem at " << new_obj << std::endl;

    return new_obj;
}

void shared_object_base::operator delete(void *p)
{
    shared_object_base* new_obj = (shared_object_base*)p;
    shmdt(new_obj);
    std::cout << "deleted shmem at " << new_obj << std::endl;
}

shared_object_base::shared_object_base()
{

}

shared_object_base::~shared_object_base()
{

}

shared_object_execute_base::shared_object_execute_base()
    : terminate(false)
{
    pthread_mutexattr_t attrs;
    pthread_mutexattr_init(&attrs);
    pthread_mutexattr_setpshared(&attrs, PTHREAD_PROCESS_SHARED);

    pthread_mutex_init(&mutex_command_pending, &attrs);
    pthread_mutex_init(&mutex_result_pending, &attrs);

    pthread_mutexattr_destroy(&attrs);
}

shared_object_execute_base::~shared_object_execute_base()
{
    terminate = true;
    pthread_mutex_unlock(&mutex_command_pending);
    usleep(10000); // wait 10ms
    pthread_mutex_destroy(&mutex_command_pending);
    pthread_mutex_destroy(&mutex_result_pending);
}

void shared_object_execute_base::startSlave()
{
    std::cout << "startSlave!" << std::endl;
    // initial state:
    // mutex_command_pending: locked
    // mutex_result_pending: locked
    pthread_mutex_lock(&mutex_command_pending);
    pthread_mutex_lock(&mutex_result_pending);

    __pid_t pid = fork();
    if (pid == 0)
    {
        std::cout << "child!" << std::endl;
        // child: command loop
        while (!terminate)
        {
            // wait for new command master
            pthread_mutex_lock(&mutex_command_pending);
            if (!terminate)
            {
                execute_command();
            }
            pthread_mutex_unlock(&mutex_result_pending);
        }
        std::cout << "slave is terminating" << std::endl;
        exit(0);
    }

    std::cout << "parent!" << std::endl;
}

void shared_object_execute_base::start_execute()
{
    pthread_mutex_unlock(&mutex_command_pending);
}

void shared_object_execute_base::end_execute()
{
    pthread_mutex_lock(&mutex_result_pending);
}
