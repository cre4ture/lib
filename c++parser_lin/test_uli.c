#elements = 4;

int main()
{
	int a;
	int b;
	int c;
	int x;

	a = 10;
	b = 2;
	c = 3;

	x = a * (b = c = 4);

	return x;
}
