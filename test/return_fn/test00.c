extern void plus();
extern void minus();

void (*goo(int a))() {
	if (a) {
		return plus;
	} else {
		return minus;
	}
}

void foo(int a) {
	goo(a)();
}
// 13 : plus, minus, goo