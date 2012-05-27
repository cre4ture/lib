#ifndef CREAX_STRINGINPUT_H
#define CREAX_STRINGINPUT_H

#include <string>

namespace creax
{
    class stringinput
    {
    private:
        std::string buffer;
        const char* pos;

    public:

        stringinput(const std::string& a_buffer)
        {
            buffer = a_buffer;
            pos = buffer.c_str();
        }

        int read(char* out, size_t count)
        {
            size_t out_count = 0;
            while ((pos[0] != 0) && (out_count < count))
            {
                out[out_count] = pos[0];
                pos++;
                out_count++;
            }
            return out_count;
        }

        bool eof()
        {
            return (*pos == 0);
        }

    };
}

#endif // CREAX_STRINGINPUT_H
