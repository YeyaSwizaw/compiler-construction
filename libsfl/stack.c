#include "stack.h"
#include "ops.h"

#include <stdlib.h>
#include <string.h>

void stack_init() {
    stack.head = (struct stack_node*)0;
}

void stack_free() {
    struct stack_node* tmp;
    while((tmp = stack.head) != 0) {
        stack.head = tmp->next;
        free(tmp);
    }
}

sfl_object stack_pop() {
    struct stack_node tmp;
    memcpy(&tmp, stack.head, sizeof(tmp));
    free(stack.head);
    stack.head = tmp.next;
    return tmp.data;
}

void stack_push_nth_obj(sfl_object* objs, sfl_int n) {
    struct stack_node* new_node = malloc(sizeof(*new_node));
    new_node->data = objs[n];
    new_node->next = stack.head;
    stack.head = new_node;
}

void stack_push_int(sfl_int value) {
    struct stack_node* new_node = malloc(sizeof(*new_node));
    new_node->data.tag = INT_VALUE;
    new_node->data.int_value = value;
    new_node->next = stack.head;
    stack.head = new_node;
}

void stack_push_fn(void* fn, sfl_int args) {
    struct stack_node* new_node = malloc(sizeof(*new_node));
    new_node->data.tag = FN_VALUE;
    new_node->data.fn_value.arg_count = args;
    new_node->data.fn_value.fn_ptr = fn;
    new_node->next = stack.head;
    stack.head = new_node;
}

// Push operators
void stack_push_add() {
    stack_push_fn(op_add, 2);
}

void stack_push_sub() {
    stack_push_fn(op_sub, 2);
}

void stack_push_mul() {
    stack_push_fn(op_mul, 2);
}

void stack_push_div() {
    stack_push_fn(op_div, 2);
}
