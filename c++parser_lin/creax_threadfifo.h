#ifndef THREADFIFO_H
#define THREADFIFO_H

#include <pthread.h>
#include <queue>
#include "creax_mutex.h"

namespace creax
{

    template<class Tp>
    class threadfifo
    {
    private:
        creax::mutex data_available_mutex;
        creax::mutex data_list_mutex;

        bool data_available;
        bool terminated;
        std::queue<Tp> data_list;

    public:
        threadfifo()
        {
            terminated = false;
            data_available_mutex.lock(); // signal: no data available
        }

        void push_data(const Tp& data)
        {
            // lock list
            mutex_lock lock(data_list_mutex);

            if (data_list.empty())
                data_available_mutex.unlock(); // signal: data now available!

            data_list.push(data);
        }

        bool pop_data(Tp& data)
        {
            data_available_mutex.lock(); // wait for data available
            {
                // lock list
                mutex_lock list_lock(data_list_mutex);

                if (data_list.empty())
                {
                    if (terminated)
                        return false;
                    else
                        throw std::runtime_error("threadlist: internal error: this should never happen!");
                }

                data = data_list.front();
                data_list.pop();

                if ((!data_list.empty()) || terminated)
                    data_available_mutex.unlock(); // signal: data still available!

                return true;
            }
        }

        void close_fifo()
        {
            mutex_lock(data_list_mutex);
            terminated = true;
            if (data_list.empty())
                data_available_mutex.unlock();
        }

        ~threadfifo()
        {}
    };

}

#endif // THREADFIFO_H
