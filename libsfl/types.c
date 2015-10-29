#include "types.h"

sfl_object obj_int(sfl_int value) {
    sfl_object res;
    res.tag = INT_VALUE;
    res.int_value = value;
    return res;
}
