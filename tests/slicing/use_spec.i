/* run.config
 STDOPT: +" -eva-use-spec f -slice-return main -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i"
 STDOPT: +"-main main2 -slicing-level 3 -slice-undef-functions -eva-use-spec h -slice-return main2 -slicing-keep-annotations -then-on 'Slicing export' -set-project-as-default -print  -eva @EVA_OPTIONS@ -eva-use-spec='-@all'"



 */

int x, y, z, t;
int G1, G2;

//@ assigns x \from  \nothing;
void g(void);

//@ assigns x \from  \nothing;
int f() {
  x = 1;
  g();
}

/* When -eva-use-spec f is used, the body of f must not be kept (as it
   references the body of g, which is not kept since the body of f is not
   analyzed. */
int main() {
  f();
  return x;
}

//@ assigns G1 \from a; assigns G2 \from b; ensures G1 == a;
void h(int a, int b) {
  G1 = a;
  G2 = b;
}

/* Check that function specialization works well with -eva-use-spec. The result
   of -slicing-keep-annotations is a bit surprising, but in fact quite  good. */
int main2(int v1, int v2, int v3, int v4) {
  h(v1, v2);
  int tmp = G1;
  h(v3, v4);
  return tmp + G2;
}
