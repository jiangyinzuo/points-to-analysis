
extern int plus();
extern int minus();

void foo() {
  int (*p1)()=minus;
	p1 = plus;
	int (**p2)() = &p1;
	int (*p3)()= *p2;

p3();
	*p2 = minus;
	p1();
}
// 11 : plus
// 13 : minus