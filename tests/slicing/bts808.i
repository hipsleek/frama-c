/* run.config
*    STDOPT: +"-slice-return main -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i "
*/

int f0 (void) {
  int i = 0;
  int x; 
  if (i) { x = 1; L: x++; }
  else { x = 0; goto L; }
  return x;
}
int f1 (void) {
  int i = 1;
  int x; 
  if (i) { x = 1; goto L; }
  else { x = 0; L: x++; }
  return x;
}

int main (int n) {
  return f0 () + f1 ();
}
