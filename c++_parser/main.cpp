
#include <cstdio>
#include "hello_world.h"

int global_int;

void inc_global(int delta)
{
	return;
}

int* main(int argc, char* argv[])
{
	inc_global(5);
	return (&(global_int));
}