
void abcd(int c)
{
}

#define HALLO adas

#ifdef HALLO
void dfgh()
{

}
#else
void gjrkgj()
{

}
#endif


int main(int argc, char* argv)
{
    char a;
    char b;
    abcd(
#ifndef HALLO
                a
#else
                b
#endif
                );
}
