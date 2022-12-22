#include <stdlib.h>

extern void plus();

extern void minus();

void foo(void(* a_fptr)())
{
    a_fptr();
}


void moo()
{
    void (*af_ptr)(void(*)())=foo;
    void (*pf_ptr)()=0;
        pf_ptr=plus;
        af_ptr(pf_ptr);
        pf_ptr=minus;
        af_ptr(pf_ptr);
}

//14 : plus, minus
//18 : foo
//20 : foo

