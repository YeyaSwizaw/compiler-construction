#include "ops.h"
#include "stack.h"

void op_add(sfl_object* args) {
    stack_push_int(args[0].int_value + args[1].int_value);
}

void op_sub(sfl_object* args) {
    stack_push_int(args[0].int_value - args[1].int_value);
}

void op_mul(sfl_object* args) {
    stack_push_int(args[0].int_value * args[1].int_value);
}

void op_div(sfl_object* args) {
    stack_push_int(args[0].int_value / args[1].int_value);
}
