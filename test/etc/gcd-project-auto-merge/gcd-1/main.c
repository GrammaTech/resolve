#include <stdio.h>
#include <stdlib.h>

#include "gcd.h"

int main(int argc, char *argv[]) {
    int a = atoi(argv[1]);
    int b = atoi(argv[2]);

    printf("%d\n", gcd(a, b));
    return 0;
}
