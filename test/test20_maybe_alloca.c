void plus();
void minus();

void (*choose(void (**ch_P)()))() {
	return *ch_P;
}
void foo(int x) {
	void (**p)();
	void (*a)() = plus;
	void (*b)() = minus;
	if (x) {
		p = &a;
	} else {
		p = &b;
	}
	choose(p)();
}
// 16 : plus, minus, choose