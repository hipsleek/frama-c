/*  run.config
STDOPT: +"-slice-return main -then-on 'Slicing export' -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i"
*/
void f(void) { return; }
int X = 1 ;
int main(void) {
 call: f();
  //@ assert X == \at(X,call);
  return X; 
}
