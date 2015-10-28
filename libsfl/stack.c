#include "stack.h"

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
    new_node->data.int_value = value;
    new_node->next = stack.head;
    stack.head = new_node;
}

struct stack_data stack_pop() {
    struct stack_node tmp;
    memcpy(&tmp, stack.head, sizeof(tmp));
    free(stack.head);
    stack.head = tmp.next;
    return tmp.data;
}
