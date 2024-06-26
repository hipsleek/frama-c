/* run.config*
   STDOPT: -aorai-automata %{dep:@PTEST_DIR@/@PTEST_NAME@.ya} -aorai-acceptance
*/

void f() {}

void g() {}

//@ assigns \nothing;
int main(int c) {
  if (c<0) { c = 0; }
  if (c>5) { c = 5; }
  /*@ assert 0<=c<=5; */
  /*@ loop assigns c; */
  while (c) {
    f();
    g();
    c--;
  }
  return 0;
}

