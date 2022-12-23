#include <stdlib.h>
struct fptr
{
int (*p_fptr)();
};
struct fsptr
{
struct fptr * sptr;
};
struct wfsptr
{
  struct fsptr * wfptr;
};
int plus();

int minus();
void foo(int a, struct wfsptr * a_fptr, struct wfsptr * b_fptr) {
   if(a)
   {
    struct fsptr * temp=a_fptr->wfptr;
    a_fptr->wfptr->sptr = b_fptr->wfptr->sptr;
    b_fptr->wfptr->sptr =temp->sptr;
   }
}

void moo(char x, int op1, int op2) {
    struct fptr a_fptr ;
    a_fptr.p_fptr=plus;
    struct fptr s_fptr ;
    s_fptr.p_fptr=minus;

    struct fsptr m_fptr;
    m_fptr.sptr=&a_fptr;
    struct fsptr n_fptr;
    n_fptr.sptr=&s_fptr;

    struct wfsptr w_fptr;
    w_fptr.wfptr=&m_fptr;

    struct wfsptr x_fptr;
		
		x_fptr.wfptr = &n_fptr;
    // make_simple_alias(&x_fptr,&n_fptr); 

    struct fsptr k_fptr;
    struct wfsptr y_fptr;
    y_fptr.wfptr= & k_fptr;
		y_fptr.wfptr->sptr = x_fptr.wfptr->sptr;
    //make_alias(&y_fptr,&x_fptr); 

    n_fptr.sptr=&s_fptr;

	 struct wfsptr t1_fptr;
   t1_fptr.wfptr=&m_fptr;
   struct wfsptr t2_fptr;
   t2_fptr.wfptr=y_fptr.wfptr;
  //  if(op1)
  //  {
  //   struct fsptr * temp=(&t1_fptr)->wfptr;
  //   (&t1_fptr)->wfptr->sptr = (&t2_fptr)->wfptr->sptr;
  //   (&t2_fptr)->wfptr->sptr =temp->sptr;
  //  }
   foo(op1,&t1_fptr,&t2_fptr);
 	// clever(op1, &m_fptr, y_fptr.wfptr);
 
    //t_fptr->p_fptr(op1, op2);
    m_fptr.sptr->p_fptr();
}
// 63 : foo
// 67 : plus, minus
