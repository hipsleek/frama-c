/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "time.h"
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

struct mystruct {
   int k ;
   int l ;
};
typedef struct mystruct mystruct;
/*@ predicate p1(int x, int y) = x + y > 0;
 */
int __gen_e_acsl_p1(int x, int y);

/*@ predicate p2(integer x, integer y) = x + y > 0;

*/
int __gen_e_acsl_p2(int x, int y);

int __gen_e_acsl_p2_3(int x, __e_acsl_mpz_struct * y);

int __gen_e_acsl_p2_5(int x, int y);

/*@ logic integer f1(integer x, integer y) = x + y;

*/
int __gen_e_acsl_f1_3(int x, int y);

void __gen_e_acsl_f1_5(__e_acsl_mpz_t *__retres_arg, int x,
                       __e_acsl_mpz_struct * y);

void __gen_e_acsl_f1_7(__e_acsl_mpz_t *__retres_arg, __e_acsl_mpz_struct * x,
                       __e_acsl_mpz_struct * y);

long __gen_e_acsl_f1(int x, int y);

/*@ logic char h_char(char c) = c;
 */
char __gen_e_acsl_h_char(char c);

/*@ logic short h_short(short s) = s;
 */
short __gen_e_acsl_h_short(short s);

/*@ logic int g_hidden(int x) = x;
 */
int __gen_e_acsl_g_hidden(int x);

/*@ logic int g(int x) = g_hidden(x);
 */
int __gen_e_acsl_g(int x);

/*@ logic mystruct t1(mystruct m) = m;
 */
void __gen_e_acsl_t1(mystruct *__retres_arg, mystruct m);

/*@ logic integer t2(mystruct m) = m.k + m.l;
 */
long __gen_e_acsl_t2(mystruct m);

/*@ predicate k_pred(integer x) = x > 0;

*/
int __gen_e_acsl_k_pred(int x);

/*@ requires k_pred(x); */
void __gen_e_acsl_k(int x);

void k(int x)
{
  return;
}

int glob = 5;
/*@ predicate never_called(int x) = x == x;
 */
/*@ logic double f2(double x) = (double)(1 / x);
 */
double __gen_e_acsl_f2(double x);

/*@ predicate p_here{L}(integer x) = x > 0;
 */
int __gen_e_acsl_p_here(int x);

/*@ logic integer f_here{L}(integer x) = x;
 */
int __gen_e_acsl_f_here(int x);

/*@ logic integer f_sum(integer x) = \sum(1, x, \lambda integer y; 1);
 */
int __gen_e_acsl_f_sum(int x);

/*@ logic real over(real a, real b) = a / b;

*/
void __gen_e_acsl_over(__e_acsl_mpq_t *__retres_arg, double a, double b);

int z = 8;
/*@ logic integer f3{L}(integer y) = \at(z + y,L);

*/
long __gen_e_acsl_f3(int y);

int main(void)
{
  int __retres;
  mystruct m;
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  int x = 1;
  int y = 2;
  {
    int __gen_e_acsl_p1_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_p1_2 = __gen_e_acsl_p1(x,y);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"y",0,y);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"x",0,x);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"p1(x, y)",0,
                                 __gen_e_acsl_p1_2);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "p1(x, y)";
    __gen_e_acsl_assert_data.file = "functions.c";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 56;
    __e_acsl_assert(__gen_e_acsl_p1_2,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert p1(x, y); */ ;
  {
    int __gen_e_acsl_p2_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __gen_e_acsl_p2_2 = __gen_e_acsl_p2(3,4);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"p2(3, 4)",0,
                                 __gen_e_acsl_p2_2);
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Assertion";
    __gen_e_acsl_assert_data_2.pred_txt = "p2(3, 4)";
    __gen_e_acsl_assert_data_2.file = "functions.c";
    __gen_e_acsl_assert_data_2.fct = "main";
    __gen_e_acsl_assert_data_2.line = 57;
    __e_acsl_assert(__gen_e_acsl_p2_2,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
  }
  /*@ assert p2(3, 4); */ ;
  {
    __e_acsl_mpz_t __gen_e_acsl_;
    int __gen_e_acsl_p2_4;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
      {.values = (void *)0};
    __gmpz_init_set_str(__gen_e_acsl_,"99999999999999999999999999999",10);
    __gen_e_acsl_p2_4 = __gen_e_acsl_p2_3(5,
                                          (__e_acsl_mpz_struct *)__gen_e_acsl_);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,
                                 "p2(5, 99999999999999999999999999999)",0,
                                 __gen_e_acsl_p2_4);
    __gen_e_acsl_assert_data_3.blocking = 1;
    __gen_e_acsl_assert_data_3.kind = "Assertion";
    __gen_e_acsl_assert_data_3.pred_txt = "p2(5, 99999999999999999999999999999)";
    __gen_e_acsl_assert_data_3.file = "functions.c";
    __gen_e_acsl_assert_data_3.fct = "main";
    __gen_e_acsl_assert_data_3.line = 58;
    __e_acsl_assert(__gen_e_acsl_p2_4,& __gen_e_acsl_assert_data_3);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
    __gmpz_clear(__gen_e_acsl_);
  }
  /*@ assert p2(5, 99999999999999999999999999999); */ ;
  {
    long __gen_e_acsl_f1_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
      {.values = (void *)0};
    __gen_e_acsl_f1_2 = __gen_e_acsl_f1(x,y);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,"y",0,y);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,"x",0,x);
    __e_acsl_assert_register_long(& __gen_e_acsl_assert_data_4,"f1(x, y)",0,
                                  __gen_e_acsl_f1_2);
    __gen_e_acsl_assert_data_4.blocking = 1;
    __gen_e_acsl_assert_data_4.kind = "Assertion";
    __gen_e_acsl_assert_data_4.pred_txt = "f1(x, y) == 3";
    __gen_e_acsl_assert_data_4.file = "functions.c";
    __gen_e_acsl_assert_data_4.fct = "main";
    __gen_e_acsl_assert_data_4.line = 60;
    __e_acsl_assert(__gen_e_acsl_f1_2 == 3L,& __gen_e_acsl_assert_data_4);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
  }
  /*@ assert f1(x, y) == 3; */ ;
  {
    int __gen_e_acsl_f1_4;
    int __gen_e_acsl_p2_6;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
      {.values = (void *)0};
    __gen_e_acsl_f1_4 = __gen_e_acsl_f1_3(3,4);
    __gen_e_acsl_p2_6 = __gen_e_acsl_p2_5(x,__gen_e_acsl_f1_4);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"f1(3, 4)",0,
                                 __gen_e_acsl_f1_4);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"x",0,x);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,
                                 "p2(x, f1(3, 4))",0,__gen_e_acsl_p2_6);
    __gen_e_acsl_assert_data_5.blocking = 1;
    __gen_e_acsl_assert_data_5.kind = "Assertion";
    __gen_e_acsl_assert_data_5.pred_txt = "p2(x, f1(3, 4))";
    __gen_e_acsl_assert_data_5.file = "functions.c";
    __gen_e_acsl_assert_data_5.fct = "main";
    __gen_e_acsl_assert_data_5.line = 61;
    __e_acsl_assert(__gen_e_acsl_p2_6,& __gen_e_acsl_assert_data_5);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
  }
  /*@ assert p2(x, f1(3, 4)); */ ;
  {
    __e_acsl_mpz_t __gen_e_acsl__3;
    __e_acsl_mpz_t __gen_e_acsl_f1_6;
    __e_acsl_mpz_t __gen_e_acsl__4;
    int __gen_e_acsl_gt_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_6 =
      {.values = (void *)0};
    __gmpz_init_set_str(__gen_e_acsl__3,"99999999999999999999999999999",10);
    __gen_e_acsl_f1_5(& __gen_e_acsl_f1_6,9,
                      (__e_acsl_mpz_struct *)__gen_e_acsl__3);
    __gmpz_init_set_si(__gen_e_acsl__4,0L);
    __gen_e_acsl_gt_2 = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_f1_6),
                                   (__e_acsl_mpz_struct const *)(__gen_e_acsl__4));
    __e_acsl_assert_register_mpz(& __gen_e_acsl_assert_data_6,
                                 "f1(9, 99999999999999999999999999999)",0,
                                 (__e_acsl_mpz_struct const *)(__gen_e_acsl_f1_6));
    __gen_e_acsl_assert_data_6.blocking = 1;
    __gen_e_acsl_assert_data_6.kind = "Assertion";
    __gen_e_acsl_assert_data_6.pred_txt = "f1(9, 99999999999999999999999999999) > 0";
    __gen_e_acsl_assert_data_6.file = "functions.c";
    __gen_e_acsl_assert_data_6.fct = "main";
    __gen_e_acsl_assert_data_6.line = 62;
    __e_acsl_assert(__gen_e_acsl_gt_2 > 0,& __gen_e_acsl_assert_data_6);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_6);
    __gmpz_clear(__gen_e_acsl__3);
    __gmpz_clear(__gen_e_acsl_f1_6);
    __gmpz_clear(__gen_e_acsl__4);
  }
  /*@ assert f1(9, 99999999999999999999999999999) > 0; */ ;
  {
    __e_acsl_mpz_t __gen_e_acsl__5;
    __e_acsl_mpz_t __gen_e_acsl_f1_8;
    __e_acsl_mpz_t __gen_e_acsl__6;
    int __gen_e_acsl_eq;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_7 =
      {.values = (void *)0};
    __gmpz_init_set_str(__gen_e_acsl__5,"99999999999999999999999999999",10);
    __gen_e_acsl_f1_7(& __gen_e_acsl_f1_8,
                      (__e_acsl_mpz_struct *)__gen_e_acsl__5,
                      (__e_acsl_mpz_struct *)__gen_e_acsl__5);
    __gmpz_init_set_str(__gen_e_acsl__6,"199999999999999999999999999998",10);
    __gen_e_acsl_eq = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_f1_8),
                                 (__e_acsl_mpz_struct const *)(__gen_e_acsl__6));
    __e_acsl_assert_register_mpz(& __gen_e_acsl_assert_data_7,
                                 "f1(99999999999999999999999999999, 99999999999999999999999999999)",
                                 0,
                                 (__e_acsl_mpz_struct const *)(__gen_e_acsl_f1_8));
    __gen_e_acsl_assert_data_7.blocking = 1;
    __gen_e_acsl_assert_data_7.kind = "Assertion";
    __gen_e_acsl_assert_data_7.pred_txt = "f1(99999999999999999999999999999, 99999999999999999999999999999) ==\n199999999999999999999999999998";
    __gen_e_acsl_assert_data_7.file = "functions.c";
    __gen_e_acsl_assert_data_7.fct = "main";
    __gen_e_acsl_assert_data_7.line = 63;
    __e_acsl_assert(__gen_e_acsl_eq == 0,& __gen_e_acsl_assert_data_7);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_7);
    __gmpz_clear(__gen_e_acsl__5);
    __gmpz_clear(__gen_e_acsl_f1_8);
    __gmpz_clear(__gen_e_acsl__6);
  }
  /*@
  assert
  f1(99999999999999999999999999999, 99999999999999999999999999999) ==
  199999999999999999999999999998; */
  ;
  {
    int __gen_e_acsl_g_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_8 =
      {.values = (void *)0};
    __gen_e_acsl_g_2 = __gen_e_acsl_g(x);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_8,"x",0,x);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_8,"g(x)",0,
                                 __gen_e_acsl_g_2);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_8,"x",0,x);
    __gen_e_acsl_assert_data_8.blocking = 1;
    __gen_e_acsl_assert_data_8.kind = "Assertion";
    __gen_e_acsl_assert_data_8.pred_txt = "g(x) == x";
    __gen_e_acsl_assert_data_8.file = "functions.c";
    __gen_e_acsl_assert_data_8.fct = "main";
    __gen_e_acsl_assert_data_8.line = 68;
    __e_acsl_assert(__gen_e_acsl_g_2 == x,& __gen_e_acsl_assert_data_8);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_8);
  }
  /*@ assert g(x) == x; */ ;
  char c = (char)'c';
  {
    char __gen_e_acsl_h_char_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_9 =
      {.values = (void *)0};
    __gen_e_acsl_h_char_2 = __gen_e_acsl_h_char(c);
    __e_acsl_assert_register_char(& __gen_e_acsl_assert_data_9,"c",0,c);
    __e_acsl_assert_register_char(& __gen_e_acsl_assert_data_9,"h_char(c)",0,
                                  __gen_e_acsl_h_char_2);
    __e_acsl_assert_register_char(& __gen_e_acsl_assert_data_9,"c",0,c);
    __gen_e_acsl_assert_data_9.blocking = 1;
    __gen_e_acsl_assert_data_9.kind = "Assertion";
    __gen_e_acsl_assert_data_9.pred_txt = "h_char(c) == c";
    __gen_e_acsl_assert_data_9.file = "functions.c";
    __gen_e_acsl_assert_data_9.fct = "main";
    __gen_e_acsl_assert_data_9.line = 71;
    __e_acsl_assert((int)__gen_e_acsl_h_char_2 == (int)c,
                    & __gen_e_acsl_assert_data_9);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_9);
  }
  /*@ assert h_char(c) == c; */ ;
  short s = (short)1;
  {
    short __gen_e_acsl_h_short_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_10 =
      {.values = (void *)0};
    __gen_e_acsl_h_short_2 = __gen_e_acsl_h_short(s);
    __e_acsl_assert_register_short(& __gen_e_acsl_assert_data_10,"s",0,s);
    __e_acsl_assert_register_short(& __gen_e_acsl_assert_data_10,
                                   "h_short(s)",0,__gen_e_acsl_h_short_2);
    __e_acsl_assert_register_short(& __gen_e_acsl_assert_data_10,"s",0,s);
    __gen_e_acsl_assert_data_10.blocking = 1;
    __gen_e_acsl_assert_data_10.kind = "Assertion";
    __gen_e_acsl_assert_data_10.pred_txt = "h_short(s) == s";
    __gen_e_acsl_assert_data_10.file = "functions.c";
    __gen_e_acsl_assert_data_10.fct = "main";
    __gen_e_acsl_assert_data_10.line = 73;
    __e_acsl_assert((int)__gen_e_acsl_h_short_2 == (int)s,
                    & __gen_e_acsl_assert_data_10);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_10);
  }
  /*@ assert h_short(s) == s; */ ;
  m.k = 8;
  m.l = 9;
  {
    mystruct __gen_e_acsl_r;
    mystruct __gen_e_acsl_t1_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_11 =
      {.values = (void *)0};
    __gen_e_acsl_t1(& __gen_e_acsl_t1_2,m);
    __gen_e_acsl_r = __gen_e_acsl_t1_2;
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_11,"r.k",0,
                                 __gen_e_acsl_r.k);
    __e_acsl_assert_register_struct(& __gen_e_acsl_assert_data_11,"m");
    __e_acsl_assert_register_struct(& __gen_e_acsl_assert_data_11,"t1(m)");
    __gen_e_acsl_assert_data_11.blocking = 1;
    __gen_e_acsl_assert_data_11.kind = "Assertion";
    __gen_e_acsl_assert_data_11.pred_txt = "\\let r = t1(m); r.k == 8";
    __gen_e_acsl_assert_data_11.file = "functions.c";
    __gen_e_acsl_assert_data_11.fct = "main";
    __gen_e_acsl_assert_data_11.line = 78;
    __e_acsl_assert(__gen_e_acsl_r.k == 8,& __gen_e_acsl_assert_data_11);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_11);
  }
  /*@ assert \let r = t1(m); r.k == 8; */ ;
  {
    mystruct __gen_e_acsl_t1_4;
    long __gen_e_acsl_t2_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_12 =
      {.values = (void *)0};
    __gen_e_acsl_t1(& __gen_e_acsl_t1_4,m);
    __gen_e_acsl_t2_2 = __gen_e_acsl_t2(__gen_e_acsl_t1_4);
    __e_acsl_assert_register_struct(& __gen_e_acsl_assert_data_12,"m");
    __e_acsl_assert_register_struct(& __gen_e_acsl_assert_data_12,"t1(m)");
    __e_acsl_assert_register_long(& __gen_e_acsl_assert_data_12,"t2(t1(m))",
                                  0,__gen_e_acsl_t2_2);
    __gen_e_acsl_assert_data_12.blocking = 1;
    __gen_e_acsl_assert_data_12.kind = "Assertion";
    __gen_e_acsl_assert_data_12.pred_txt = "t2(t1(m)) == 17";
    __gen_e_acsl_assert_data_12.file = "functions.c";
    __gen_e_acsl_assert_data_12.fct = "main";
    __gen_e_acsl_assert_data_12.line = 79;
    __e_acsl_assert(__gen_e_acsl_t2_2 == 17L,& __gen_e_acsl_assert_data_12);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_12);
  }
  /*@ assert t2(t1(m)) == 17; */ ;
  __gen_e_acsl_k(9);
  double d = 2.0;
  {
    double __gen_e_acsl_f2_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_13 =
      {.values = (void *)0};
    __gen_e_acsl_f2_2 = __gen_e_acsl_f2(d);
    __e_acsl_assert_register_double(& __gen_e_acsl_assert_data_13,"d",d);
    __e_acsl_assert_register_double(& __gen_e_acsl_assert_data_13,"f2(d)",
                                    __gen_e_acsl_f2_2);
    __gen_e_acsl_assert_data_13.blocking = 1;
    __gen_e_acsl_assert_data_13.kind = "Assertion";
    __gen_e_acsl_assert_data_13.pred_txt = "f2(d) > 0";
    __gen_e_acsl_assert_data_13.file = "functions.c";
    __gen_e_acsl_assert_data_13.fct = "main";
    __gen_e_acsl_assert_data_13.line = 84;
    __e_acsl_assert(__gen_e_acsl_f2_2 > 0.,& __gen_e_acsl_assert_data_13);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_13);
  }
  /*@ assert f2(d) > 0; */ ;
  {
    int __gen_e_acsl_f_sum_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_14 =
      {.values = (void *)0};
    __gen_e_acsl_f_sum_2 = __gen_e_acsl_f_sum(100);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_14,"f_sum(100)",
                                 0,__gen_e_acsl_f_sum_2);
    __gen_e_acsl_assert_data_14.blocking = 1;
    __gen_e_acsl_assert_data_14.kind = "Assertion";
    __gen_e_acsl_assert_data_14.pred_txt = "f_sum(100) == 100";
    __gen_e_acsl_assert_data_14.file = "functions.c";
    __gen_e_acsl_assert_data_14.fct = "main";
    __gen_e_acsl_assert_data_14.line = 86;
    __e_acsl_assert(__gen_e_acsl_f_sum_2 == 100,
                    & __gen_e_acsl_assert_data_14);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_14);
  }
  /*@ assert f_sum(100) == 100; */ ;
  {
    __e_acsl_mpq_t __gen_e_acsl_over_2;
    __e_acsl_mpq_t __gen_e_acsl__9;
    int __gen_e_acsl_eq_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_15 =
      {.values = (void *)0};
    __gen_e_acsl_over(& __gen_e_acsl_over_2,1.,2.);
    __gmpq_init(__gen_e_acsl__9);
    __gmpq_set_d(__gen_e_acsl__9,0.5);
    __gen_e_acsl_eq_2 = __gmpq_cmp((__e_acsl_mpq_struct const *)(__gen_e_acsl_over_2),
                                   (__e_acsl_mpq_struct const *)(__gen_e_acsl__9));
    __e_acsl_assert_register_mpq(& __gen_e_acsl_assert_data_15,
                                 "over(1., 2.)",
                                 (__e_acsl_mpq_struct const *)(__gen_e_acsl_over_2));
    __gen_e_acsl_assert_data_15.blocking = 1;
    __gen_e_acsl_assert_data_15.kind = "Assertion";
    __gen_e_acsl_assert_data_15.pred_txt = "over(1., 2.) == 0.5";
    __gen_e_acsl_assert_data_15.file = "functions.c";
    __gen_e_acsl_assert_data_15.fct = "main";
    __gen_e_acsl_assert_data_15.line = 88;
    __e_acsl_assert(__gen_e_acsl_eq_2 == 0,& __gen_e_acsl_assert_data_15);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_15);
    __gmpq_clear(__gen_e_acsl_over_2);
    __gmpq_clear(__gen_e_acsl__9);
  }
  /*@ assert over(1., 2.) == 0.5; */ ;
  {
    int __gen_e_acsl_p_here_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_16 =
      {.values = (void *)0};
    __gen_e_acsl_p_here_2 = __gen_e_acsl_p_here(27);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_16,"p_here(27)",
                                 0,__gen_e_acsl_p_here_2);
    __gen_e_acsl_assert_data_16.blocking = 1;
    __gen_e_acsl_assert_data_16.kind = "Assertion";
    __gen_e_acsl_assert_data_16.pred_txt = "p_here(27)";
    __gen_e_acsl_assert_data_16.file = "functions.c";
    __gen_e_acsl_assert_data_16.fct = "main";
    __gen_e_acsl_assert_data_16.line = 90;
    __e_acsl_assert(__gen_e_acsl_p_here_2,& __gen_e_acsl_assert_data_16);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_16);
  }
  /*@ assert p_here(27); */ ;
  {
    int __gen_e_acsl_f_here_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_17 =
      {.values = (void *)0};
    __gen_e_acsl_f_here_2 = __gen_e_acsl_f_here(27);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_17,"f_here(27)",
                                 0,__gen_e_acsl_f_here_2);
    __gen_e_acsl_assert_data_17.blocking = 1;
    __gen_e_acsl_assert_data_17.kind = "Assertion";
    __gen_e_acsl_assert_data_17.pred_txt = "f_here(27) == 27";
    __gen_e_acsl_assert_data_17.file = "functions.c";
    __gen_e_acsl_assert_data_17.fct = "main";
    __gen_e_acsl_assert_data_17.line = 91;
    __e_acsl_assert(__gen_e_acsl_f_here_2 == 27,
                    & __gen_e_acsl_assert_data_17);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_17);
  }
  /*@ assert f_here(27) == 27; */ ;
  {
    long __gen_e_acsl_f3_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_18 =
      {.values = (void *)0};
    __gen_e_acsl_f3_2 = __gen_e_acsl_f3(5);
    __e_acsl_assert_register_long(& __gen_e_acsl_assert_data_18,"f3(5)",0,
                                  __gen_e_acsl_f3_2);
    __gen_e_acsl_assert_data_18.blocking = 1;
    __gen_e_acsl_assert_data_18.kind = "Assertion";
    __gen_e_acsl_assert_data_18.pred_txt = "f3(5) == 13";
    __gen_e_acsl_assert_data_18.file = "functions.c";
    __gen_e_acsl_assert_data_18.fct = "main";
    __gen_e_acsl_assert_data_18.line = 93;
    __e_acsl_assert(__gen_e_acsl_f3_2 == 13L,& __gen_e_acsl_assert_data_18);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_18);
  }
  /*@ assert f3(5) == 13; */ ;
  __retres = 0;
  __e_acsl_memory_clean();
  return __retres;
}

/*@ requires k_pred(x); */
void __gen_e_acsl_k(int x)
{
  {
    int __gen_e_acsl_k_pred_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_k_pred_2 = __gen_e_acsl_k_pred(x);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"x",0,x);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"k_pred(x)",0,
                                 __gen_e_acsl_k_pred_2);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Precondition";
    __gen_e_acsl_assert_data.pred_txt = "k_pred(x)";
    __gen_e_acsl_assert_data.file = "functions.c";
    __gen_e_acsl_assert_data.fct = "k";
    __gen_e_acsl_assert_data.line = 28;
    __e_acsl_assert(__gen_e_acsl_k_pred_2,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  k(x);
  return;
}

/*@ assigns \result;
    assigns \result \from x, y; */
int __gen_e_acsl_p1(int x, int y)
{
  int __retres = x + (long)y > 0L;
  return __retres;
}

/*@ assigns \result;
    assigns \result \from x, y; */
int __gen_e_acsl_p2(int x, int y)
{
  int __retres = x + y > 0;
  return __retres;
}

/*@ assigns \result;
    assigns \result \from x, *((__e_acsl_mpz_struct *)y); */
int __gen_e_acsl_p2_3(int x, __e_acsl_mpz_struct * y)
{
  __e_acsl_mpz_t __gen_e_acsl_x;
  __e_acsl_mpz_t __gen_e_acsl_add;
  __e_acsl_mpz_t __gen_e_acsl__2;
  int __gen_e_acsl_gt;
  __gmpz_init_set_si(__gen_e_acsl_x,(long)x);
  __gmpz_init(__gen_e_acsl_add);
  __gmpz_add(__gen_e_acsl_add,(__e_acsl_mpz_struct const *)(__gen_e_acsl_x),
             (__e_acsl_mpz_struct const *)(y));
  __gmpz_init_set_si(__gen_e_acsl__2,0L);
  __gen_e_acsl_gt = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_add),
                               (__e_acsl_mpz_struct const *)(__gen_e_acsl__2));
  int __retres = __gen_e_acsl_gt > 0;
  __gmpz_clear(__gen_e_acsl_x);
  __gmpz_clear(__gen_e_acsl_add);
  __gmpz_clear(__gen_e_acsl__2);
  return __retres;
}

/*@ assigns \result;
    assigns \result \from x, y; */
int __gen_e_acsl_p2_5(int x, int y)
{
  int __retres = x + (long)y > 0L;
  return __retres;
}

/*@ assigns \result;
    assigns \result \from x, y; */
int __gen_e_acsl_f1_3(int x, int y)
{
  int __retres = x + y;
  return __retres;
}

/*@ assigns (*__retres_arg)[0];
    assigns (*__retres_arg)[0] \from x, *((__e_acsl_mpz_struct *)y);
 */
void __gen_e_acsl_f1_5(__e_acsl_mpz_t *__retres_arg, int x,
                       __e_acsl_mpz_struct * y)
{
  __e_acsl_mpz_t __gen_e_acsl_x_2;
  __e_acsl_mpz_t __gen_e_acsl_add_2;
  __gmpz_init_set_si(__gen_e_acsl_x_2,(long)x);
  __gmpz_init(__gen_e_acsl_add_2);
  __gmpz_add(__gen_e_acsl_add_2,
             (__e_acsl_mpz_struct const *)(__gen_e_acsl_x_2),
             (__e_acsl_mpz_struct const *)(y));
  __gmpz_init_set(*__retres_arg,
                  (__e_acsl_mpz_struct const *)(__gen_e_acsl_add_2));
  __gmpz_clear(__gen_e_acsl_x_2);
  __gmpz_clear(__gen_e_acsl_add_2);
  return;
}

/*@ assigns (*__retres_arg)[0];
    assigns (*__retres_arg)[0]
      \from *((__e_acsl_mpz_struct *)x), *((__e_acsl_mpz_struct *)y);
 */
void __gen_e_acsl_f1_7(__e_acsl_mpz_t *__retres_arg, __e_acsl_mpz_struct * x,
                       __e_acsl_mpz_struct * y)
{
  __e_acsl_mpz_t __gen_e_acsl_add_3;
  __gmpz_init(__gen_e_acsl_add_3);
  __gmpz_add(__gen_e_acsl_add_3,(__e_acsl_mpz_struct const *)(x),
             (__e_acsl_mpz_struct const *)(y));
  __gmpz_init_set(*__retres_arg,
                  (__e_acsl_mpz_struct const *)(__gen_e_acsl_add_3));
  __gmpz_clear(__gen_e_acsl_add_3);
  return;
}

/*@ assigns \result;
    assigns \result \from x, y; */
long __gen_e_acsl_f1(int x, int y)
{
  long __retres = x + (long)y;
  return __retres;
}

/*@ assigns \result;
    assigns \result \from c; */
char __gen_e_acsl_h_char(char c)
{
  return c;
}

/*@ assigns \result;
    assigns \result \from s; */
short __gen_e_acsl_h_short(short s)
{
  return s;
}

/*@ assigns \result;
    assigns \result \from x; */
int __gen_e_acsl_g_hidden(int x)
{
  return x;
}

/*@ assigns \result;
    assigns \result \from x; */
int __gen_e_acsl_g(int x)
{
  int __gen_e_acsl_g_hidden_2;
  __gen_e_acsl_g_hidden_2 = __gen_e_acsl_g_hidden(x);
  return __gen_e_acsl_g_hidden_2;
}

/*@ assigns __retres_arg;
    assigns __retres_arg \from m; */
void __gen_e_acsl_t1(mystruct *__retres_arg, mystruct m)
{
  *__retres_arg = m;
  return;
}

/*@ assigns \result;
    assigns \result \from m; */
long __gen_e_acsl_t2(mystruct m)
{
  long __retres = m.k + (long)m.l;
  return __retres;
}

/*@ assigns \result;
    assigns \result \from x; */
int __gen_e_acsl_k_pred(int x)
{
  int __retres = x > 0;
  return __retres;
}

/*@ assigns \result;
    assigns \result \from x; */
double __gen_e_acsl_f2(double x)
{
  __e_acsl_mpq_t __gen_e_acsl__7;
  __e_acsl_mpq_t __gen_e_acsl_x_3;
  __e_acsl_mpq_t __gen_e_acsl_div;
  double __gen_e_acsl__8;
  __gmpq_init(__gen_e_acsl__7);
  __gmpq_set_str(__gen_e_acsl__7,"1",10);
  __gmpq_init(__gen_e_acsl_x_3);
  __gmpq_set_d(__gen_e_acsl_x_3,x);
  __gmpq_init(__gen_e_acsl_div);
  __gmpq_div(__gen_e_acsl_div,(__e_acsl_mpq_struct const *)(__gen_e_acsl__7),
             (__e_acsl_mpq_struct const *)(__gen_e_acsl_x_3));
  __gen_e_acsl__8 = __gmpq_get_d((__e_acsl_mpq_struct const *)(__gen_e_acsl_div));
  __gmpq_clear(__gen_e_acsl__7);
  __gmpq_clear(__gen_e_acsl_x_3);
  __gmpq_clear(__gen_e_acsl_div);
  /*@ assert Eva: is_nan_or_infinite: \is_finite(__gen_e_acsl__8); */
  return __gen_e_acsl__8;
}

/*@ assigns \result;
    assigns \result \from x; */
int __gen_e_acsl_p_here(int x)
{
  int __retres = x > 0;
  return __retres;
}

/*@ assigns \result;
    assigns \result \from x; */
int __gen_e_acsl_f_here(int x)
{
  return x;
}

/*@ assigns \result;
    assigns \result \from x; */
int __gen_e_acsl_f_sum(int x)
{
  int __gen_e_acsl_y;
  int __gen_e_acsl_one;
  int __gen_e_acsl_cond;
  int __gen_e_acsl_lambda;
  int __gen_e_acsl_accumulator;
  __gen_e_acsl_one = 1;
  __gen_e_acsl_cond = 0;
  __gen_e_acsl_lambda = 0;
  __gen_e_acsl_accumulator = 0;
  __gen_e_acsl_y = 1;
  while (1) {
    __gen_e_acsl_cond = __gen_e_acsl_y > x;
    if (__gen_e_acsl_cond) break;
    else {
      __gen_e_acsl_lambda = 1;
      __gen_e_acsl_accumulator += __gen_e_acsl_lambda;
      __gen_e_acsl_y += __gen_e_acsl_one;
    }
  }
  return __gen_e_acsl_accumulator;
}

/*@ assigns (*__retres_arg)[0];
    assigns (*__retres_arg)[0] \from a, b; */
void __gen_e_acsl_over(__e_acsl_mpq_t *__retres_arg, double a, double b)
{
  __e_acsl_mpq_t __gen_e_acsl_a;
  __e_acsl_mpq_t __gen_e_acsl_b;
  __e_acsl_mpq_t __gen_e_acsl_div_2;
  __gmpq_init(__gen_e_acsl_a);
  __gmpq_set_d(__gen_e_acsl_a,a);
  __gmpq_init(__gen_e_acsl_b);
  __gmpq_set_d(__gen_e_acsl_b,b);
  __gmpq_init(__gen_e_acsl_div_2);
  __gmpq_div(__gen_e_acsl_div_2,
             (__e_acsl_mpq_struct const *)(__gen_e_acsl_a),
             (__e_acsl_mpq_struct const *)(__gen_e_acsl_b));
  __gmpq_init(*__retres_arg);
  __gmpq_set(*__retres_arg,(__e_acsl_mpq_struct const *)(__gen_e_acsl_div_2));
  __gmpq_clear(__gen_e_acsl_a);
  __gmpq_clear(__gen_e_acsl_b);
  __gmpq_clear(__gen_e_acsl_div_2);
  return;
}

/*@ assigns \result;
    assigns \result \from y; */
long __gen_e_acsl_f3(int y)
{
  long __retres = z + (long)y;
  return __retres;
}

