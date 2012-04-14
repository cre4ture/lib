#ifndef CREAX_FD_H
#define CREAX_FD_H

#include <fcntl.h>

namespace creax
{

    class fd
    {
    private:
        int mfd;

    public:
        fd(const std::string& filename)
        {
            mfd = open(filename.c_str(), O_RDONLY);
            if (mfd == -1)
                throw std::runtime_error("creax::fd(): failed to open file: " + filename);
        }

        int getFd()
        {
            return mfd;
        }

        void getStat(struct stat &statbuf)
        {
            if (fstat(mfd,&statbuf) < 0)
                throw std::runtime_error("creax::fd::getStat(): fstat error");
        }

        ~fd()
        {
            if (mfd != -1)
                close(mfd);
        }
    };

} // end namespace creax

#endif // CREAX_FD_H
