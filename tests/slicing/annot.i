/* run.config
   STDOPT: +"-main f1 -slice-assert f1  -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check "
   STDOPT: +"-main f2 -slice-assert f2  -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check "
*/

extern int x, z;
int t[10];

void f1() {
  int v = 3;
  x = x + x - x;
  int y = z;
  x = 3;
  int r = x;
  //@ assert x == \at(x, Pre);
}

void f2() {
  t[1] = 5;
  t[6] = 4;
  x = 2;
  x = 3;
  //@ assert \initialized(&t[x..9]);
}
