/*  run.config
STDOPT: +"-slice-assert main -then-on 'Slicing export' -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -no-deps"
*/

int X = 1 ;

int main(void) {
  int y;
L: y = 0;
   X++;
  //@ assert X > \at(X,L);
  return X;
}
