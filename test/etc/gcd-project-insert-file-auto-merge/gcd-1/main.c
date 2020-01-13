#include <stdio.h>
#include <stdlib.h>

#include "gcd1.h"

int main(int argc, char *argv[]) {
    int a = atoi(argv[1]);
    int b = atoi(argv[2]);

    printf("gcd=%d\n", gcd(a, b));
    return 0;
}
