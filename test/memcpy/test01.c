struct fptr
{
	int (*p_fptr)();
};
struct fsptr
{
	struct fptr * sptr;
};
int plus();

int minus();

void foo() {
	struct fsptr A, B;
	struct fptr A1, B1;
	A1.p_fptr =plus;
	B1.p_fptr =minus;
	A.sptr = &A1;
	B.sptr = &B1;
	
	*A.sptr = *B.sptr;
	A1.p_fptr();
	B1.p_fptr();
}