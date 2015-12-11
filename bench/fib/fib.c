#include <stdio.h>

unsigned long fib(unsigned long n) {
    if(n < 2) {
        return n;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}

int main() {
    printf("%d\n", fib(36));
}
