#ifndef CREAX_FILENAMEPATH_H
#define CREAX_FILENAMEPATH_H

#include <string>

inline std::string extractFilename(const std::string& filename)
{
    const char* c = filename.c_str();
    const char* last_slash = c;

    for (; (*c) != 0; c++)
    {
        switch (*c)
        {
        case '/':
            last_slash = c;
            break;
        default:
            break;
        }
    }
    last_slash++;
    return std::string(last_slash);
}

// returns path with '/' at the end
inline std::string extractFilepath(const std::string& filename)
{
    size_t last_slash = 0;

    for (size_t c = 0; c < filename.size(); c++)
    {
        if (filename[c] == '/')
        {
            last_slash = c;
        }
    }

    return filename.substr(0, last_slash+1);
}

inline bool isAbsolutePath(const std::string& path)
{
    return (path[0] == '/');
}

#endif // CREAX_FILENAMEPATH_H
