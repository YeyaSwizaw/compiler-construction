#ifndef SFLLIB_STACK_H
#define SFLLIB_STACK_H

#include "types.h"

struct stack_node;

struct stack {
    struct stack_node* head;
};

struct stack_node {
    struct stack_node* next;
    sfl_object data;
};

static struct stack stack;

void stack_init();
void stack_free();

sfl_object stack_pop();

void stack_push_int(sfl_int);
void stack_push_fn(void*, sfl_int);
void stack_push_add();
void stack_push_sub();
void stack_push_mul();
void stack_push_div();

#endif
