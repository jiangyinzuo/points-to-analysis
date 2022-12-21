struct fptr
{
	int (*p_fptr)(int);
};
struct fsptr
{
struct fptr * sptr;
};
int plus(int a) {
   return 1+a;
}

void foo()
{
	struct fptr a_fptr;
	struct fsptr s_fptr;
	s_fptr.sptr=&a_fptr;
	
	a_fptr.p_fptr=plus;
	s_fptr.sptr->p_fptr(1);
}
//20 : plus
