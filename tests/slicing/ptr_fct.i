/* run.config
  STDOPT: +"-main h -slice-return h -slicing-level 1 -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i "
 */

int X ;
typedef void (*PTF)(int);

void f1(int x) {
  X = x ;
}
void f2 (int y) ; /* no source */

PTF ptf = 0 ;

void g(int arg) {
  ptf = (arg > 0 ? &f1 : &f2) ;
  (*ptf)(arg) ;
}

PTF h (int a, int b) {
  if (b) {
    ptf = &g;
    (*ptf)(a);
    }
  return ptf;
}
