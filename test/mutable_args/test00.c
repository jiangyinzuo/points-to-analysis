extern void A();
extern void B();

void swap(void (**p1)(), void(**p2)() ) {
	void(*temp)() = *p1;
	*p1=*p2;
	*p2=temp;
}

void ENTRY() {
	void (*F1)() = A;
	void (*F2)() = B;
	swap(&F1, &F2);
	F1();
}