int gcd(int a, int b) {
    if (a == 0) return a;
    while (b != 0)
        if (a > b) a = a - b;
        else       b = b - a;

    return a;
}
