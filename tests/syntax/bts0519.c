/* run.config
 ENABLED_IF: %{bin-available:gcc}
   STDOPT: +"-cpp-command='gcc -C -E -I.'" +"-cpp-frama-c-compliant"
 EXIT: 1
   STDOPT: +"-cpp-command='gcc -C -E -I. -DERR'" +"-cpp-frama-c-compliant"
 */
int t[4];

#ifdef ERR
int q[static 3];
#endif

void f(int a[static 3]) {
  a[2] = 3;
}

int main () {
  f(t);
  return 0;
}
