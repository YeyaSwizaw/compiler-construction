#include "stack.h"
#include "output.h"

extern void sfl_entry();

int main(int argc, char* argv[]) {
    stack_init();
    sfl_entry();
    stack_free();
}
