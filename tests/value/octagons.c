/* run.config*
   STDOPT: +"-deterministic -eva-domains octagon -eva-octagon-through-calls -eva-msg-key=d-octagon,-d-cvalue"
*/

#include "__fc_builtin.h"

volatile int undet;

/* Minimal example from the Eva user manual. */
void demo () {
  int y = undet;
  int k = Frama_C_interval(0, 10);
  int x = y - k;
  int r = x + 3 - y;  // r \in [-7..3]
  int t;
  if (y > 15)
    t = x;            // t \in [6..]
}

/* Same example as [demo] but with other integer types. */
void integer_types () {
  unsigned int k, x, y, r, t;
  y = Frama_C_interval(0, 65536);
  k = Frama_C_interval(0, 10);
  x = y + k;        // An octagon should be inferred despite unsigned types.
  r = x + 3 - y;
  if (y < 300) {
    t = x;
    Frama_C_show_each_reduced_unsigned(r, t);
  }
  y = Frama_C_interval(0, 65536);
  k = Frama_C_interval(0, 10);
  x = y - k;        // No octagon inferred as [y - k] may overflow.
  r = x + 3 - y;
  if (y < 300) {
    t = x;
    Frama_C_show_each_unreduced_unsigned(r, t);
  }
  char ck, cx, cy, cr, ct;
  cy = Frama_C_interval(-100, 100);
  ck = Frama_C_interval(0, 10);
  cx = cy - ck;    // An octagon should be inferred despite the casts to int.
  cr = cx + 3 - cy;
  if (cy > 15) {
    ct = cx;
    Frama_C_show_each_reduced_char(cr, ct);
  }
  cy = undet;
  ck = Frama_C_interval(0, 10);
  cx = cy - ck;    // No octagon should be inferred as the downcast may wrap.
  cr = cx + 3 - cy;
  if (cy > 15) {
    ct = cx;
    Frama_C_show_each_unreduced_char(cr, ct);
  }
  int ix, ir, it;
  cy = undet;
  ck = Frama_C_interval(0, 10);
  ix = cy - ck;    // An octagon should be inferred as there is no downcast.
  ir = ix + 3 - cy;
  if (cy > 15) {
    it = ix;
    Frama_C_show_each_reduced_int(ir, it);
  }
}

/* A test with multiple mathematical operations to complicate the inference
   and use of octagons. */
void arith () {
  int k = Frama_C_interval(0, 4);
  int a, b, x, y, r;
  /* 1. Infer octagons from assignments. */
  a = Frama_C_interval(5, 25);
  b = Frama_C_interval(-12, 12);
  x = 1 - (a + 2*k - 4);                                  // x + a ∈ [-3..5]
  y = 4*4 - k + (1 + b);                                  // y - b ∈ [13..17]
  /* 1.1 Use octagons in the evaluation of expressions. */
  r = 2 * (10 - (b - 1 - y) - (x - 2 + a));               // r ∈ [42..66]
  Frama_C_show_each_precise(r);
  r = 2 * (10 - (b + x - 3 - y + a));                    // r ∈ [42..66]
  Frama_C_show_each_imprecise(r);
  k = Frama_C_interval(0, 4);
  /* 1.1 Use octagons to propagate variable reductions. */
  if (12 - x < (k+1)*3) {                                 // x > -3
    r = 10 * a;                                           // so a < 8
    Frama_C_show_each(r);                                 // {50; 60; 70}
  }
  /* 2. Infer octagons from conditions. */
  a = Frama_C_interval(-1024, 1024);
  b = Frama_C_interval(-1024, 1024);
  if (20*k - a - 17 < 5 - b + (1 << 3)                    // a - b > -30
      && a + (k+6)/2 - b <= 32) {                         // a - b <= 29
    r = b - a;
    Frama_C_show_each(r);                                 // [-29..29]
  }
  if (a < b && b <= a)
    Frama_C_show_each_BOTTOM(a, b);
}

/* Tests the join of the octagon domain. */
void join () {
  int a, b, r;
  int k = Frama_C_interval(-1, 4);
  if (undet) {
    a = undet;
    b = a + k;
  } else {
    a = Frama_C_interval(-32, -10);
    b = k * 5;
  }
  // In both cases, we have b - a >= -1. The "else" branch was more precise.
  r = b - a + 1;
  Frama_C_show_each_join_positive(r);
  if (undet) {
    a = undet;
    b = - (a + k);
  } else {
    a = Frama_C_interval(-32, -10);
    b = k * 5;
  }
  // In both cases, we have b + a <= 10. The "then" branch was more precise.
  r = b + a - 10;
  Frama_C_show_each_join_negative(r);
}

/* Tests the octagon domain within loops. */
void loop () {
  int k = Frama_C_interval(-8, 8);
  int a = Frama_C_interval(-1024, 1024);
  int b = a + 1;
  int c = a + 1;
  int d = a + k;
  for (int i = 0; i < 421; i++) {
    a = a + 2;
    b = b + 2; // The relation between a and b should be maintained in the loop.
    c = c + 1; // The relation between a and c should be lost in the loop.
    d = a + k; // This relation should be maintained.
  }
  int d1 = b - a;
  int d2 = c - a;
  int d3 = d - a;
  Frama_C_show_each_singleton_1(d1);
  Frama_C_show_each_imprecise(d2);
  Frama_C_show_each_precise(d3);
}

/* Tests the soundness of the octagon domain in presence of pointers. */
void pointers () {
  int x, y, r;
  int *px = &x, *pr = &r;
  x = Frama_C_interval(-1024, 1024);
  y = x + 1;
  r = y - x;
  Frama_C_show_each_singleton_1(r);
  *px = Frama_C_interval(-1024, 1024);
  Frama_C_show_each_singleton_1(r);
  *pr = Frama_C_interval(-1024, 1024);
  Frama_C_show_each_unknown(r);
  r = y - x;
  Frama_C_show_each_unknown(r);
  y = x + 2;
  r = y - x;
  Frama_C_show_each_singleton_2(r);
}

/* Tests the saturation of octagons: inference of a relation between (x, z)
   from relations between (x, y) and between (y, z). */
void saturate () {
  int k = Frama_C_interval(-6, 4);
  int x = Frama_C_interval(-1024, 1024);
  int y = k - x;
  int z = y + 1;
  int result = - z - x;                  // result == k + 1
  Frama_C_show_each_saturate(result);    // ∈ [-5..5]
}

int diff (int x, int y) { return x - y; }
int neg (int x) { return -x; }

struct st { int i; char c; };

/* Tests interprocedural octagon domains on calls with non-integer arguments,
   which should be ignored if they are not supported. */
void test_non_integer_args (double d, int *p, struct st s) {
  int a1 = *p;
  int b1 = s.i;
  char c1 = s.c;
  double d1 = d;
  Frama_C_dump_each();
}

/* Tests the propagation of octagons through function calls. */
void interprocedural () {
  int a = Frama_C_interval(-4, 12);
  int b = Frama_C_interval(-4, 12);
  int neg_a = neg(a);
  int neg_b = neg(b);
  /* [r1] is the direct difference [a-b], [r2] uses the result of the function
     [neg] (and thus need the octagon inferred in [neg]), and [r3] calls the
     function [diff] (and the analysis of [diff] needs the octagons inferred
     here about a and b). */
  int r1, r2, r3;
  if (a > b) {
    r1 = a - b;
    r2 = a + neg_b;
    r3 = diff (a, b);
  } else {
    r1 = b - a;
    r2 = b + neg_a;
    r3 = diff (b, a);
  }
  /* With the interprocedural octagons, r1, r2 and r3 must be equally precise. */
  Frama_C_show_each_equal(r1, r2, r3);
  /* Tests a call with non-integer arguments: should not crash. */
  struct st s = {.i = 42, .c = 5 };
  test_non_integer_args(3.5, &a, s);
}

/* Prints the octagons state. */
void dump () {
  char k = Frama_C_interval(0, 8);
  int a = Frama_C_interval(-128, 128);
  int b = a + k;
  int c = b - k;
  Frama_C_dump_each();
}

/* Example initially provided by user kroeckx in the issue 2166 of the BTS,
   imported as issue 301 on the public gitlab. Solved by the octagon domain. */
void pub301 () {
  unsigned int len = Frama_C_interval(1, 1024);
  unsigned int n = Frama_C_interval(0, 63);
  if (n != 0) {
    if (len >= 64 || len + n >= 64) {
      n = 64 - n;
      len -= n; // [len] should stay between 0 and 1024.
    }
  }
}

#define MAX 20

void arrays () {
  int a[MAX], t[MAX];
  /*@ loop unroll MAX; */ // Unroll to ensure array initialization.
  for (int i = 0; i < MAX; i++) {
    a[i] = undet;
  }

  int x = Frama_C_interval(1, MAX);
  for (int j = x - 1; j >= 0; j--) {
    // Need the relation j <= x-1 to prove that index x-1-j >= 0.
    t[j] = a[x - 1 - j];
  }

  int y = Frama_C_interval(0, MAX-1);
  int index = (MAX-1) - y;
  for (int j = y; j < MAX; j++) {
    // Need the relation index + j == MAX-1 to prove index >= 0.
    t[index] = a[j];
    index --;
  }

  // Same with a floating-point variable f instead of x.
  float f = Frama_C_float_interval(0., (float)(MAX-1));
  index = (MAX-1) - (int)f;
  for (int j = (int)f; j < MAX; j++) {
    // Need the relation index + j == MAX-1 to prove index >= 0.
    t[index] = a[j];
    index --;
  }
}

void main () {
  demo ();
  integer_types ();
  arith ();
  join ();
  loop ();
  pointers ();
  saturate ();
  interprocedural ();
  dump ();
  pub301 ();
  arrays ();
}
