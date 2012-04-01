
int x;

int add(int a, int b) 
{
	int c;
    while (a+c)
    {
        c = c + 1;
    }
	c = a + b;
	return c;
}

void foo(int a)
{
	int b;
	b = a + a;
}

int main()
{
	/* Deklaration */
	int a;
	int b;
    int c;

    int* ptr;

    ptr = &a;

    if (a == b)
    {
        c = 2;
    }

	/* Zuweisung */
	a = 3;
    b = (*ptr);
	x = 7; /* globale variable x */

	/* Addition */
	/* a = a + b; *//* a = 8 */
	/*c = c + 1; *//* c = (2 3 4)T */

	/* Methodenaufruf */
	c = 6;
	c = add(c, c); /* c = (2 4 6)T */

	return 0;
}
