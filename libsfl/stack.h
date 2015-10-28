#ifndef SFLLIB_STACK_H
#define SFLLIB_STACK_H

#include "types.h"

struct stack_node;

struct stack {
    struct stack_node* head;
};

struct stack_data {
    union {
        sfl_int int_value;
        void* ptr_value;
    };
};

struct stack_node {
    struct stack_node* next;
    struct stack_data data;
};

static struct stack stack;

void stack_init();
void stack_free();

void stack_push(sfl_int);
struct stack_data stack_pop();

#endif
