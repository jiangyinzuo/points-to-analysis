extern long plus99();

void foo() {
	long a = 0;
	long (*p)() = (long (*)())a;
	// p = plus99;
	p();
}