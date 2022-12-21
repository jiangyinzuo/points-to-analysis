#include <stdlib.h>
extern int plus();

int clever(int (*a_fptr)()) {
    return a_fptr();
}
struct fptr
{
    int (*p_fptr)();
};

void moo() {
    int (*a_fptr)() = plus;
    struct fptr * t_fptr=(struct fptr *)malloc(sizeof(struct fptr));
    t_fptr->p_fptr = a_fptr;
    clever(t_fptr->p_fptr);
}
// test12.c
/// 11 : plus, minus
/// 21 : malloc
/// 30 : clever