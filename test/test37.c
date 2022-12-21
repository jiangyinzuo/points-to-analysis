
extern int plus(int a, int b);
extern int minus(int a, int b);
struct FFF {

  int (*p66)(int,int);
  int (*p88)(int,int);
};
void bar() {
	struct FFF fff;
	fff.p66 = plus;
	fff.p88 = minus;
    int (*PAAR[1])(int, int) ;
		PAAR[0] = plus;

	int (*qq)(int,int) = PAAR[0];
}