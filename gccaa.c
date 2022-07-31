#include <stdio.h>

typedef struct bit{
    int b : 4;
}bit;

int main() {
    bit a = {5};

    printf("%d\n", a.b);
    
    return -1;
}