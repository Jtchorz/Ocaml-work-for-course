int main() {
    int n = 4;
    int r = 1;
    int a = 0;
    int b = 15;

    // First compute the factorial
    while (n > 0) {
        r = r * n;
        n--;
    }

    // Then compute gcd(n!, 15) naively
    a = r;
    while (a != b) {
        if (a > b) {
            a = a - b;
        } else {
            b = b - a;
        }
    }

    return a;
}