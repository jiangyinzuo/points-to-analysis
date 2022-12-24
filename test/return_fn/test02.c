extern void plus();
extern void minus();

void (*goo(
 void(*p1)(),
 void(*p2)()
 ))() {
	return p2;	
}

void foo(int a) {
	goo(plus, minus)();
}
// 12 : minus, goo