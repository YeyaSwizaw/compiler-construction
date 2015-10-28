#include "stack.h"
#include "output.h"

extern void sfl_entry();

int main(int argc, char* argv[]) {
    stack_init();
    sfl_entry();
    print_int(stack_pop().int_value);
    stack_free();
}
