void plus();

void minus();

void (*foo( void (*a_fptr)(), void(*b_fptr)() ))() {
   return a_fptr;
}

void (*clever( void (*a_fptr)(), void(*b_fptr)() ))() {
   return b_fptr;
}

void moo(char c) {
    void(* (*goo_ptr)(void (*)(), void(*)()))();

    if (c == 'A') {
       goo_ptr = foo;
    } else {
       goo_ptr = clever;
    }

    goo_ptr(plus, minus)();
}

// 22 : foo, clever, plus, minus
