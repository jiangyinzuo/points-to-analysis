extern void A();
extern void B();

void ENTRY() {
	void (*F1)() = A;
	void (*F2)() = B;
	void (**p1)() = &F1;
	void (**p2)() = &F2;
	void(*temp)() = *p1;
	*p1=*p2;
	F1();
	F2();
	*p2=temp;
	F1();
	F2();
}