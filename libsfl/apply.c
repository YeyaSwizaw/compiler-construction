#include "stack.h"
#include "types.h"

#include <stdlib.h>

void apply_full() {
    sfl_fn fn_obj = stack_pop().fn_value;
    sfl_int* args = malloc(fn_obj.arg_count * sizeof(*args));
    for(int i = 0; i < fn_obj.arg_count; ++i) {
        args[i] = stack_pop().int_value;
    }

    stack_push(((sfl_int(*)(sfl_int*))(fn_obj.fn_ptr))(args));
    free(args);
}
