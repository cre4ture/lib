#ifndef CREAX_THREAD_H
#define CREAX_THREAD_H

#include <pthread.h>

namespace creax
{

    template<class Tp_param, class Tp_result>
    class thread
    {
    private:
        pthread_t my_thread;

    public:
        thread(Tp_result* (*thread_routine)(Tp_param*), Tp_param* param)
        {
            void* (*tmp)(void*) = (void*(*)(void*))thread_routine;
            pthread_create(&my_thread, NULL, tmp, (void*)param);
        }

        Tp_result* join()
        {
            Tp_result* result;
            pthread_join(my_thread, (void**)&result);
            return result;
        }
    };

} // end namespace creax

#endif // CREAX_THREAD_H
