#include <stdlib.h>
struct fptr
{
	int (*p_fptr)(int, int);
};
struct fsptr
{
	struct fptr * sptr;
};
int plus(int a, int b);

int minus(int a, int b);

void moo(int x)
{
	struct fptr a_fptr;
	a_fptr.p_fptr=plus;
	// struct fptr b_fptr;
	// b_fptr.p_fptr=minus;

	struct fsptr s_fptr;
	s_fptr.sptr=&a_fptr;
	// struct fsptr r_fptr;
	// r_fptr.sptr=&b_fptr;

	struct fsptr* w_fptr=(struct fsptr*)malloc(sizeof(struct fsptr));
    
	*w_fptr=s_fptr;
	w_fptr->sptr->p_fptr(1, x);
}