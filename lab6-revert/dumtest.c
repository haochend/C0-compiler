//test div-by-zero 

#include <stdlib.h>

int main() {
    int a;
    int b;

    a = 5;
    int** array = malloc(sizeof(int*)*a); 
    array[0] = malloc(sizeof(int)*2);
    array[0][0] = 4;
    b = array[0][0];

    return 0;
}
