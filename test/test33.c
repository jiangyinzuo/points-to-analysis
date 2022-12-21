#include <stdlib.h>
struct fptr
{
  int (*p_fptr)(int, int);
};

int plus(int a, int b) {
   return a+b;
}

int minus(int a, int b) {
   return a-b;
}

int (*foo(int a, int b, struct fptr * c_fptr, struct fptr * d_fptr))(int, int) {
   return c_fptr->p_fptr;
}

int (*clever(int a, int b, struct fptr * c_fptr, struct fptr * d_fptr))(int, int) {
  if(a>0 && b<0)
  {
   return d_fptr->p_fptr;
  }

  return c_fptr->p_fptr;
}

int moo(char x, int op1, int op2) {
    struct fptr a_fptr;
    a_fptr.p_fptr= plus;
    struct fptr s_fptr;
    s_fptr.p_fptr= minus;

    int (* (*goo_ptr)(int, int, struct fptr *,struct fptr *))(int, int)=foo;
    int (*t_fptr)(int, int) = 0;

    if(x == '+')
    {
        t_fptr = goo_ptr(op1, op2, &a_fptr, &s_fptr);
        t_fptr(op1, op2);
    }else
    {
        goo_ptr=clever;
        t_fptr = goo_ptr(op1, op2, &s_fptr, &a_fptr);
        t_fptr(op1, op2);
    }
    t_fptr(op1, op2);
    return 0;
}

// 39 : foo
// 40 : plus
// 44 : clever
// 45 : plus, minus
// 47 : plus, minus