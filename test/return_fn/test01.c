extern void plus();
extern void minus();

void (*goo(int a,
 void(*p1)(),
 void(*p2)()
 ))() {
	if (a) {
		return p1;
	} else {
		return p2;
	}
}

void foo(int a) {
	goo(a, plus, minus)();
}
// 13 : plus, minus, goo