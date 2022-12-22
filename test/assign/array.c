
extern int plus();
extern int minus();

void foo(int n, int m) {
  int (*p1[2])();
	p1[0]=minus;
	p1[n] = plus;
	int (*p2)() = p1[m];
	int (*p3)()= p2;
p3();
}