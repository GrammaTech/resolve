#include <stdio.h>
#include <stdlib.h>

int gcd(int a, int b) {
    if (a == 0) return b;
    while (b != 0)
        if (a > b) a = a - b;
        else       b = b - a;

    return a;
}

int main(int argc, char *argv[]) {
    int a = atoi(argv[1]);
    int b = atoi(argv[2]);

    printf("gcd=%d\n", gcd(a, b));
    return 0;
}
