#ifndef SFLLIB_TYPES_H
#define SFLLIB_TYPES_H

#include <stdint.h>

typedef int32_t sfl_int;
typedef char sfl_char;

typedef struct {
    sfl_int arg_count, args_applied;
    void* fn_ptr;
    struct sfl_object* args;
} sfl_fn;

struct sfl_object;

enum object_tag {
    INT_VALUE,
    CHAR_VALUE,
    FN_VALUE,
    PARTIAL_FN_VALUE,
};

typedef struct {
    enum object_tag tag;

    union {
        sfl_int int_value;
        sfl_char char_value;
        sfl_fn fn_value;
    };
} sfl_object;

#endif
