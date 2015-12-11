#include <stdio.h>

unsigned long fact(unsigned long n) {
    unsigned long result = 1;
    while(n > 0) {
        result *= n;
        n--;
    }
    return result;
}

int main() {
    for(int i = 0; i < 620; ++i) {
        printf("%d", fact(12));
    }
}
