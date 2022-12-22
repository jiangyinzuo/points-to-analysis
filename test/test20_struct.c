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

void moo() {
  struct S1 MINUS1;
  MINUS1.FUNC = minus;

  struct S2 MINUS_PTR;
  MINUS_PTR.S1_ptr = &MINUS1;

  clever(&MINUS_PTR)->FUNC();
    
}

// 47 : foo, clever
// 48 : plus, minus