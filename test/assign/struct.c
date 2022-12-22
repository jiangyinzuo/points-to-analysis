struct fptr
{
int (*p_fptr)(int, int);
};
int plus(int a, int b) ;

int minus(int a, int b);

void clever() {
    int (*a_fptr)(int, int) = plus;
    int (*s_fptr)(int, int) = minus;
    struct fptr TTT={0};
		TTT.p_fptr = minus;
		TTT.p_fptr(1, 1);
}
// 14 : minus