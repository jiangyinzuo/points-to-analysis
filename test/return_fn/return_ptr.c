extern void plus();
extern void minus();

void (**goo(int a,
 void(**p1)(),
 void(**p2)()
 ))() {
	if (a) {
		return p1;
	} else {
		return p2;
	}
}

void foo(int a) {
	void (*x1)() = plus;
	void (*x2)() = minus;
	void (**xx1)() = &x1;
	(*goo(a, xx1, &x2))();
}