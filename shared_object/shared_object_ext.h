#ifndef SHARED_OBJECT_EXT_H
#define SHARED_OBJECT_EXT_H

#include "shared_data.h"
#include "shared_object.h"

template<class _parentT, bool _is_shared, size_t _max_shared_data_segments = 8, size_t _param_buffer_size = 1024>
class shared_object_ext: public shared_object<_parentT, _is_shared, _param_buffer_size>
{
    struct shmem_data_seg_info
    {
        key_t key;
        size_t size;
        char* data_parent;
        char* data_child;
    };

    typedef shared_object_ext<_parentT, _is_shared, _max_shared_data_segments, _param_buffer_size> thisType;

private:
    shmem_data_seg_info ds_info[_max_shared_data_segments];
    size_t ds_used;

protected:
    char* mapSharedData(size_t i, bool read_only)
    {
        shmem_data_seg_info& dsi = ds_info[i];
        shared_data_open(dsi.key, dsi.size, read_only, dsi.data_child, dsi.data_child);
        return dsi.data_child;
    }

public:

    char* createSharedDataSegment(size_t size, bool parent_read_only, bool child_read_only)
    {
        size_t i = ds_used++;
        if (i >= _max_shared_data_segments)
            throw std::runtime_error("createSharedDataSegment(): max number reached (maybe you want to change template argument to allow more?)!");
        shmem_data_seg_info& dsi = ds_info[i];
        dsi.size = size;
        shared_data_create(dsi.size, dsi.key, dsi.data_parent, parent_read_only);
        dsi.data_child = dsi.data_parent;
        thisType& me = *this;
        me.call_function_charp(&thisType::mapSharedData, i, child_read_only);
        return dsi.data_parent;
    }

    shared_object_ext(): ds_used(0) {}

    virtual ~shared_object_ext()
    {
        for (size_t i = 0; i < ds_used; i++)
        {
            // child and parent should be the same pointer?
            shared_data_detach(ds_info[i].data_parent);
        }
    }
};

#endif // SHARED_OBJECT_EXT_H
