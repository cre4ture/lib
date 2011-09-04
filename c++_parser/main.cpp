
#include <cstdio>
#include "hello_world.h"

int global_int;
int gint2;

void inc_global(int delta)
{
	return;
}

int* main(int argc, char* argv[])
{
	inc_global(5 * global_int * 6 * 7 % argc);
	inc_global(global_int = 0);
	gint2 = 5;
	return (&(global_int));
}