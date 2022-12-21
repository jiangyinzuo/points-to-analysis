extern void plus();

void (*goo())() {
	return plus;
}

void foo() {
	goo()();
}
// 8 : plus, goo