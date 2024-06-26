/* run.config
   STDOPT: +"-calldeps -slice-return main -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -no-calldeps"
*/
/* Problem : f(1) should be sliced out. See BTS#326 */
int t[2] ;
int r;
void f (int i) {
  t[i] = i;
}

void g (void) {
  f(0) ;
  f(1) ;
}

int main (void) {
  g () ;
  r = t[0] ;
  return r;
}
