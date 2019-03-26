//test memerror
/* erstwhile of brie's "Dr. H**bl's Compile Along Blog */
/* evaluation order, sideeffects, etc */
#include <stdlib.h>
int* test(int ** i)
{
        int * x;
        x = *i;
        *i = NULL;
        return x;
}

int main()
{
        int** i;

	i = malloc (sizeof(int*));
        *i = malloc (sizeof(int));


	return 0;
}
