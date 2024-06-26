/* run.config
   STDOPT: +"-slice-return main -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  "
   STDOPT: +"-sparecode"
*/

int main() {
  int t[10] = {0, 1, 2};
  /*@ requires \valid(t + (0 .. 10 - 1));
    ensures ∀ ℤ i; 0 ≤ i < \old(10) ⇒ *(t + i) ≡ 0;
  */
  return t[5]+t[2];
}
