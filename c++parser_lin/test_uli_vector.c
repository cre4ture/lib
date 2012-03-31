#elements = 4; /* muss durch 2 teilbar sein! */

int_vec gvec1 = {1, 2, 3, 4};
int_vec gvec2 = {2, 2, 2, 2};
int_vec gvec3 = {10, 20, 30, 40};

int main()
{
	/* Deklaration */
	int i;
    i = 1;
    int_vec vec;
    int_vec vecB;

    vec = vecB = (gvec1 + gvec3) * gvec2;

	return vec[0] + vec[i+0] + vec[2] + vec[3];
}
