#include <stdlib.h>
extern void plus();

void clever(void (*a_fptr)()) {
    a_fptr();
}
struct S1 {
  void (*FUNC)();
};

void moo() {
    //void (*a_fptr)() = plus;
    struct S1 *STRUCT_ptr = (struct S1 *)malloc(sizeof(struct S1));
    STRUCT_ptr->FUNC = plus;
    clever(STRUCT_ptr->FUNC);
}
// test12.c
/// 5 : plus
/// 14 : malloc
/// 16 : clever