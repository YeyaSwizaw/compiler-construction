#include <stdio.h>

unsigned long fib_rec(unsigned long n, unsigned long acc1, unsigned long acc2) {
    if(n == 0) {
        return acc2;
    } else {
        unsigned long tmp = acc1 + acc2;
        return fib_rec(n - 1, tmp, acc1);
    }
}

unsigned long fib(unsigned long n) {
    return fib_rec(n, 0, 1);
}

int main() {
    printf("%d\n", fib(47));
}
