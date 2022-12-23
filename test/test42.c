#include <stdlib.h>
extern int plus();

int clever(int (*a_fptr)()) {
    return a_fptr();
}
struct S2 {

    int (*PTR)();
};
struct STRUCT3 {
	struct S2 s2;
};
struct fptr
{
	struct STRUCT3 s3;
};

void moo() {
    struct fptr t_fptr;
    t_fptr.s3.s2.PTR = plus;
    clever(t_fptr.s3.s2.PTR);
}

// 5 : plus
// 22 : clever