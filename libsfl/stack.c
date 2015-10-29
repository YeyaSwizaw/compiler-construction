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

void stack_push(sfl_int value) {
    struct stack_node* new_node = malloc(sizeof(*new_node));
    new_node->data.tag = INT_VALUE;
    new_node->data.int_value = value;
    new_node->next = stack.head;
    stack.head = new_node;
}

sfl_object stack_pop() {
    struct stack_node tmp;
    memcpy(&tmp, stack.head, sizeof(tmp));
    free(stack.head);
    stack.head = tmp.next;
    return tmp.data;
}

// Push operators
void stack_push_add() {
    struct stack_node* new_node = malloc(sizeof(*new_node));
    new_node->data.tag = FN_VALUE;
    new_node->data.fn_value.arg_count = 2;
    new_node->data.fn_value.fn_ptr = (void*)op_add;
    new_node->next = stack.head;
    stack.head = new_node;
}

void stack_push_sub() {
    struct stack_node* new_node = malloc(sizeof(*new_node));
    new_node->data.tag = FN_VALUE;
    new_node->data.fn_value.arg_count = 2;
    new_node->data.fn_value.fn_ptr = (void*)op_sub;
    new_node->next = stack.head;
    stack.head = new_node;
}

void stack_push_mul() {
    struct stack_node* new_node = malloc(sizeof(*new_node));
    new_node->data.tag = FN_VALUE;
    new_node->data.fn_value.arg_count = 2;
    new_node->data.fn_value.fn_ptr = (void*)op_mul;
    new_node->next = stack.head;
    stack.head = new_node;
}

void stack_push_div() {
    struct stack_node* new_node = malloc(sizeof(*new_node));
    new_node->data.tag = FN_VALUE;
    new_node->data.fn_value.arg_count = 2;
    new_node->data.fn_value.fn_ptr = (void*)op_div;
    new_node->next = stack.head;
    stack.head = new_node;
}
