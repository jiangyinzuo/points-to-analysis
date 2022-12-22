struct S1 {
  int (*FUNC)();
};
struct S2 {
  struct S1 *S1_ptr;
};
int plus();
int minus();

struct S1 *clever(struct S2 *b2) {
  return b2->S1_ptr;
}

void moo(int x) {
  struct S1 MINUS1, PLUS1;
  MINUS1.FUNC = minus;
	PLUS1.FUNC = plus;

  struct S2 MINUS_PTR, PLUS_PTR;
  MINUS_PTR.S1_ptr = &MINUS1;
	PLUS_PTR.S1_ptr = &PLUS1;

	struct S2 *s2pointer = 0;
	if (x) {
		s2pointer = &MINUS_PTR;
	} else {
		s2pointer = &PLUS_PTR;
	}
  clever(s2pointer)->FUNC();
    
}