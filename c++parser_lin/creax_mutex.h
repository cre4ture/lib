#ifndef CREAX_MUTEX_H
#define CREAX_MUTEX_H

#include <pthread.h>
#include <errno.h> // EBUSY
#include <stdexcept>

namespace creax
{

    class mutex_lock;

    class mutex
    {
    private:
        pthread_mutex_t my_mutex;

    public:

        void lock()
        {
            int result = pthread_mutex_lock(&my_mutex);
            if (result != 0)
            {
                throw std::runtime_error("creax::mutex::lock(): Failed to lock mutex!");
            }
        }

        bool tryLock()
        {
            int result = pthread_mutex_trylock(&my_mutex);
            switch (result)
            {
            case EBUSY:
                return false;
            case 0:
                return true;
            default:
                throw std::runtime_error("creax::mutex::tryLock(): Error trying to lock mutex!");
            }
        }

        void unlock()
        {
            int result = pthread_mutex_unlock(&my_mutex);
            if (result != 0)
            {
                throw std::runtime_error("creax::mutex::unlock(): Failed to unlock mutex!");
            }
        }

        mutex()
        {
            pthread_mutex_init(&my_mutex, NULL);
        }

        ~mutex()
        {
            pthread_mutex_destroy(&my_mutex);
        }
    };

    // implement try ... finally for mutex
    class mutex_lock
    {
    private:
        mutex* lockedMutex;

    public:
        /// use assignLockedMutex to assign an already locked Mutex!
        mutex_lock()
            : lockedMutex(NULL)
        {}

        /// locks mutex and assigns it
        mutex_lock(mutex& a_mutex_to_lock)
            : lockedMutex(&a_mutex_to_lock)
        {
            a_mutex_to_lock.lock();
        }

        /// use it only in combination with default constructor
        void assignLockedMutex(mutex& a_locked_mutex)
        {
            if (lockedMutex != NULL)
                throw std::runtime_error("creax::mutex_lock::assignLockedMutex(): A mutex was already assigned!");

            lockedMutex = &a_locked_mutex;
        }

        /// unlocks assigned mutex
        ~mutex_lock()
        {
            if (lockedMutex != NULL)
            {
                lockedMutex->unlock();
            }
        }
    };

}

#endif // CREAX_MUTEX_H
