#include "ops.h"

sfl_int op_add(sfl_int* args) {
    return args[0] + args[1];
}

sfl_int op_sub(sfl_int* args) {
    return args[0] - args[1];
}

sfl_int op_mul(sfl_int* args) {
    return args[0] * args[1];
}

sfl_int op_div(sfl_int* args) {
    return args[0] / args[1];
}
