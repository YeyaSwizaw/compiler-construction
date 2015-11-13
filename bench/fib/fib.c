#include <stdio.h>

long fib_rec(long n, long acc1, long acc2) {
    if(n == 0) {
        return acc2;
    } else {
        long tmp = acc1 + acc2;
        return fib_rec(n - 1, tmp, acc1);
    }
}

long fib(long n) {
    return fib_rec(n, 0, 1);
}

int main() {
    printf("%d\n", fib(47));
}
