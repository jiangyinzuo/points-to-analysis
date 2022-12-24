#include <stdlib.h>
struct fptr
{
int (*p_fptr)(int, int);
};
struct fsptr
{
struct fptr * sptr;
};
struct wfsptr
{
  struct fsptr * wfptr;
};
int plus(int a, int b);

int minus(int a, int b);
// 1) plus,minus. end with (minus,plus) or (minus,minus)
struct fptr * foo(int a, int b, struct wfsptr * a_fptr, struct wfsptr * b_fptr) { 
   if(a>0 && b<0)
   {
         struct wfsptr wftemp=*a_fptr; // 1) plus
     *a_fptr=*b_fptr;
     *b_fptr=wftemp;
    //b_fptr->wfptr->sptr->p_fptr(a,b);
    return a_fptr->wfptr->sptr; // 1) minus
   }
   a_fptr->wfptr->sptr->p_fptr=minus;
   return b_fptr->wfptr->sptr; // 1) minus
}

struct fptr * clever(int a, int b, struct fsptr * a_fptr, struct fsptr * b_fptr ) { // 1) plus,minus
   struct wfsptr t1_fptr;
	 t1_fptr.wfptr=a_fptr;// 1) plus
  //  make_simple_alias(&t1_fptr,a_fptr);
   struct wfsptr t2_fptr;
	 t2_fptr.wfptr=b_fptr;// 1) minus
  //  make_simple_alias(&t2_fptr,b_fptr);
   return foo(a,b,&t1_fptr,&t2_fptr);
}


int moo(char x, int op1, int op2) {
    struct fptr a_fptr ;
    a_fptr.p_fptr=plus;
    struct fptr s_fptr ;
    s_fptr.p_fptr=minus;

    struct fsptr m_fptr;
    m_fptr.sptr=&a_fptr;//plus
    struct fsptr n_fptr;
    n_fptr.sptr=&s_fptr;//minus

    struct wfsptr w_fptr;
		w_fptr.wfptr = &m_fptr;//plus
    // make_simple_alias(&w_fptr,&m_fptr);
    struct wfsptr x_fptr;
		x_fptr.wfptr = &n_fptr;//minus
    // make_simple_alias(&x_fptr,&n_fptr);

    struct fsptr k_fptr;
    struct wfsptr y_fptr;
    y_fptr.wfptr= & k_fptr;
		y_fptr.wfptr->sptr=x_fptr.wfptr->sptr;//minus
    // make_alias(&y_fptr,&x_fptr);

    clever(op1, op2, &m_fptr, y_fptr.wfptr);//plus,minus ret (minus,plus),minus
 		struct fptr* t_fptr = clever(op1,op2,y_fptr.wfptr,x_fptr.wfptr);//minus, (plus,minus)
    t_fptr->p_fptr(op1,op2);

    return 0;
}

// 39 : swap_w
// 40 : plus,minus
// 49 : make_simple_alias
// 51 : make_simple_alias
// 52 : foo
// 68 : make_simple_alias
// 70 : make_simple_alias
// 75 : make_alias
// 79 : clever
// 80 : clever
// 81 : plus, minus