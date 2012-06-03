#ifndef CREAX_FD_H
#define CREAX_FD_H

#include <fcntl.h>
#include <stdexcept>

namespace creax
{

    inline bool fileexists(const std::string& filename)
    {
        int mfd;
        mfd = open(filename.c_str(), O_RDONLY);
        if (mfd == -1)
            return false;
        else
        {
            close(mfd);
            return true;
        }
    }

    class fd
    {
    private:
        int mfd;

    public:
        fd(const std::string& filename, int flags = O_RDONLY)
        {
            mfd = open(filename.c_str(), flags, S_IRUSR | S_IWUSR); // in case of O_CREATE set file permissions: S_IRUSR | S_IWUSR
            if (mfd == -1)
                throw std::runtime_error("creax::fd(): failed to open file: " + filename);
        }

        int getFd() const
        {
            return mfd;
        }

        void getStat(struct stat &statbuf) const
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
