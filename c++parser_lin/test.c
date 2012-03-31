#elements = 3;

int x;

int_vec add(int_vec a, int_vec b) 
{
	int_vec c;
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

	/* Deklaration (Vektor) */
	int_vec c;

	/* Zuweisung */
	a = 3;
	b = 5;
	x = 7;

	/* Zuweisung (Vektor) */
	c = { 1, 2, 3 };

	/* Addition */
	a = a + b; /* a = 8 */
	c = c + 1; /* c = (2 3 4)T */

	/* Matritzenmultiplikation */
	c = { 1, 2, 3 };
	c = c * 2; /* c = (2 4 6)T */

	/* Inline assembler, mit Zugriff auf Variablen des C-Kontext */
	asm {
		push 10
		pop a
		push 12
		pop b
		mul
	}

	/* Methodenaufruf */
	c = { 1, 2, 3 };
	c = add(c, c); /* c = (2 4 6)T */

	/* Array */
	int_vec d[10];
	d[0] = {1, 2, 3}; /* d[0] = (1 2 3)T */

	/* Schleife */
	int i;
	for (i = 0; (i < 10); i=i+1) {
		d[i] = 3; /* d[i] = (3 3 3)T */
	}

	/* Pointer */
	int *p;
	p = &a;
	int_vec *q;
	q = &c;

	return 0;
}
