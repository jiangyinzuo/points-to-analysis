#include <stdlib.h>
struct fptr
{
	void (*p_fptr)();
};
struct fsptr
{
	struct fptr * sptr;
};
void plus();

void minus();

void clever(struct fptr * c_fptr, struct fptr * d_fptr) {//minus,plus
	void (*t_fptr)()=c_fptr->p_fptr;//minus
	c_fptr->p_fptr=d_fptr->p_fptr;//plus
	d_fptr->p_fptr=t_fptr;//minus
  t_fptr();//minus
}
void foo(struct fsptr * c_fptr, struct fsptr * d_fptr) {//plus,minus
	struct fptr t_fptr=*(c_fptr->sptr);//plus
	c_fptr->sptr=d_fptr->sptr;//minus
    	clever(c_fptr->sptr,&t_fptr);//minus,plus. ret: plus, minus
    	return t_fptr.p_fptr();//minus
}
void moo(int x)
{
	struct fptr a_fptr;
	a_fptr.p_fptr=plus;
	struct fptr b_fptr;
	b_fptr.p_fptr=minus;

	struct fsptr s_fptr;
	s_fptr.sptr=&a_fptr;
	struct fsptr r_fptr;
	r_fptr.sptr=&b_fptr;

	struct fsptr* w_fptr=(struct fsptr*)malloc(sizeof(struct fsptr));
    
	*w_fptr=s_fptr;
	foo(w_fptr,&r_fptr);//plus, minus
}

// 18 : minus
// 23 : clever
// 24 : minus
// 38 : malloc
// 41 : foo


