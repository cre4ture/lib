#include "shared_data.h"

#include <stdexcept>
#include <sys/shm.h>

void shared_data_create(size_t size, key_t& key, char*& data, bool read_only)
{
    data = NULL;
    int shmid;
    int flags = 0;
    key = 1234;

    if (read_only) flags = SHM_RDONLY;

    do
    {
        key++;
        if (key > 10000) throw std::runtime_error("shared_data_create(): unable to find a free shared mem key!");

        shmid = shmget(key, size, IPC_EXCL|IPC_CREAT|0600);
    }
    while ( -1 == shmid );

    data = (char*)shmat(shmid,NULL,flags);

    if (NULL == data)
    {
        throw std::runtime_error("shared_data_create(): failed mapping shared mem");
    }
}

shared_data::shared_data(size_t a_size, bool read_only)
{
    size = a_size;
    shared_data_create(a_size, key, data, read_only);
}

void shared_data_open(key_t a_key, size_t a_size, bool read_only, char* dest_ptr, char*& data)
{
    data = NULL;

    int shmid = shmget(a_key, a_size, 0600);

    if (-1 == shmid )
    {
       throw std::runtime_error("shared_data::shared_data(): failed access existing shared mem block");
    }

    if (read_only)
        data = (char*)shmat(shmid,dest_ptr,SHM_RDONLY);
    else
        data = (char*)shmat(shmid,dest_ptr,0);

    if (NULL == data)
    {
        throw std::runtime_error("shared_data::shared_data(): failed mapping shared mem");
    }

    if ((dest_ptr != NULL)&&(data != dest_ptr))
    {
        throw std::runtime_error("shared_data::shared_data(): failed mapping shared mem at desired position");
    }
}

shared_data::shared_data(key_t a_key, size_t a_size, bool read_only, char* dest_ptr)
{
    key = a_key;
    size = a_size;
    shared_data_open(key, size, read_only, dest_ptr, data);
}

void shared_data_detach(char* data)
{
    if (data != NULL)
        shmdt(data);
}

shared_data::~shared_data()
{
    shared_data_detach(data);
}
