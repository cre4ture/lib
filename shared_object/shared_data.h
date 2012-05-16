#ifndef SHARED_DATA_H
#define SHARED_DATA_H

#include <stdexcept>
#include <sys/types.h>

void shared_data_create(size_t size, key_t& key, char*& data, bool read_only);
void shared_data_open(key_t a_key, size_t a_size, bool read_only, char* dest_ptr, char*& data);
void shared_data_detach(char* data);

class shared_data
{
private:
    key_t key;
    char* data;
    size_t size;

public:
    // open existing shared mem
    shared_data(key_t a_key, size_t a_size, bool read_only, char *dest_ptr = NULL);
    // create new shared mem
    shared_data(size_t a_size, bool read_only);

    int getKey()
    {
        return key;
    }

    char* getData()
    {
        return data;
    }

    virtual ~shared_data();
};

#endif // SHARED_DATA_H
