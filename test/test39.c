#include <stdlib.h>
extern int plus(int a, int b);
extern int minus();
void foo(int x)
{
	int (**a_fptr)()=(int (**)())malloc(sizeof(int (*)(int,int)));
	
	{
		*a_fptr=minus;
	}
	x=(*a_fptr)();
}
// 6 : malloc
// 11 : minus