/* run.config*
   STDOPT: -aorai-automata %{dep:@PTEST_DIR@/@PTEST_NAME@.ya}
*/

int X;
int Y;

void g(int x) {
  Y=x;
}

int f(int x) {
  X=x;
  g(X);
  X++;
  g(X);
  return 0;
}

int real_main (int c) {
  if (c) f(4);
  return 0;
}

int main (int c) {
  return real_main(c);
}
