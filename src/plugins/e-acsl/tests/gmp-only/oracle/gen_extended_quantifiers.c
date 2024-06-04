/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "time.h"
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

int main(void)
{
  int __retres;
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  {
    __e_acsl_mpz_t __gen_e_acsl_;
    __e_acsl_mpz_t __gen_e_acsl__2;
    __e_acsl_mpz_t __gen_e_acsl_k;
    __e_acsl_mpz_t __gen_e_acsl_one;
    int __gen_e_acsl_cond;
    __e_acsl_mpz_t __gen_e_acsl_lambda;
    __e_acsl_mpz_t __gen_e_acsl_accumulator;
    __e_acsl_mpz_t __gen_e_acsl__4;
    int __gen_e_acsl_eq;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gmpz_init_set_si(__gen_e_acsl_,2L);
    __gmpz_init_set_si(__gen_e_acsl__2,10L);
    __gmpz_init_set_si(__gen_e_acsl_one,1L);
    __gen_e_acsl_cond = 0;
    __gmpz_init_set_si(__gen_e_acsl_lambda,0L);
    __gmpz_init_set_si(__gen_e_acsl_accumulator,0L);
    __gmpz_init_set(__gen_e_acsl_k,
                    (__e_acsl_mpz_struct const *)(__gen_e_acsl_));
    while (1) {
      __gen_e_acsl_cond = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_k),
                                     (__e_acsl_mpz_struct const *)(__gen_e_acsl__2));
      if (__gen_e_acsl_cond > 0) break;
      else {
        {
          __e_acsl_mpz_t __gen_e_acsl__3;
          __e_acsl_mpz_t __gen_e_acsl_mul;
          __gmpz_init_set_si(__gen_e_acsl__3,2L);
          __gmpz_init(__gen_e_acsl_mul);
          __gmpz_mul(__gen_e_acsl_mul,
                     (__e_acsl_mpz_struct const *)(__gen_e_acsl__3),
                     (__e_acsl_mpz_struct const *)(__gen_e_acsl_k));
          __gmpz_set(__gen_e_acsl_lambda,
                     (__e_acsl_mpz_struct const *)(__gen_e_acsl_mul));
          __gmpz_clear(__gen_e_acsl__3);
          __gmpz_clear(__gen_e_acsl_mul);
        }
        __gmpz_add(__gen_e_acsl_accumulator,
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_accumulator),
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_lambda));
        __gmpz_add(__gen_e_acsl_k,
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_k),
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_one));
      }
    }
    __gmpz_init_set_si(__gen_e_acsl__4,108L);
    __gen_e_acsl_eq = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_accumulator),
                                 (__e_acsl_mpz_struct const *)(__gen_e_acsl__4));
    __e_acsl_assert_register_mpz(& __gen_e_acsl_assert_data,
                                 "\\sum(2, 10, \\lambda integer k; 2 * k)",0,
                                 (__e_acsl_mpz_struct const *)(__gen_e_acsl_accumulator));
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "\\sum(2, 10, \\lambda integer k; 2 * k) == 108";
    __gen_e_acsl_assert_data.file = "extended_quantifiers.i";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 7;
    __e_acsl_assert(__gen_e_acsl_eq == 0,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    __gmpz_clear(__gen_e_acsl_);
    __gmpz_clear(__gen_e_acsl__2);
    __gmpz_clear(__gen_e_acsl_k);
    __gmpz_clear(__gen_e_acsl_one);
    __gmpz_clear(__gen_e_acsl_lambda);
    __gmpz_clear(__gen_e_acsl_accumulator);
    __gmpz_clear(__gen_e_acsl__4);
  }
  /*@ assert \sum(2, 10, \lambda integer k; 2 * k) == 108; */ ;
  {
    __e_acsl_mpz_t __gen_e_acsl__5;
    __e_acsl_mpz_t __gen_e_acsl__6;
    __e_acsl_mpz_t __gen_e_acsl_k_2;
    __e_acsl_mpz_t __gen_e_acsl_one_2;
    int __gen_e_acsl_cond_2;
    __e_acsl_mpz_t __gen_e_acsl_lambda_2;
    __e_acsl_mpz_t __gen_e_acsl_accumulator_2;
    int __gen_e_acsl_eq_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __gmpz_init_set_si(__gen_e_acsl__5,1L);
    __gmpz_init_set_si(__gen_e_acsl__6,10L);
    __gmpz_init_set_si(__gen_e_acsl_one_2,1L);
    __gen_e_acsl_cond_2 = 0;
    __gmpz_init_set_si(__gen_e_acsl_lambda_2,0L);
    __gmpz_init_set_si(__gen_e_acsl_accumulator_2,0L);
    __gmpz_init_set(__gen_e_acsl_k_2,
                    (__e_acsl_mpz_struct const *)(__gen_e_acsl__5));
    while (1) {
      __gen_e_acsl_cond_2 = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_k_2),
                                       (__e_acsl_mpz_struct const *)(__gen_e_acsl__6));
      if (__gen_e_acsl_cond_2 > 0) break;
      else {
        {
          __e_acsl_mpz_t __gen_e_acsl__7;
          __gmpz_init_set_str(__gen_e_acsl__7,"1",10);
          __gmpz_set(__gen_e_acsl_lambda_2,
                     (__e_acsl_mpz_struct const *)(__gen_e_acsl__7));
          __gmpz_clear(__gen_e_acsl__7);
        }
        __gmpz_add(__gen_e_acsl_accumulator_2,
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_accumulator_2),
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_lambda_2));
        __gmpz_add(__gen_e_acsl_k_2,
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_k_2),
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_one_2));
      }
    }
    __gen_e_acsl_eq_2 = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_accumulator_2),
                                   (__e_acsl_mpz_struct const *)(__gen_e_acsl__6));
    __e_acsl_assert_register_mpz(& __gen_e_acsl_assert_data_2,
                                 "\\sum(1, 10, \\lambda integer k; 1)",0,
                                 (__e_acsl_mpz_struct const *)(__gen_e_acsl_accumulator_2));
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Assertion";
    __gen_e_acsl_assert_data_2.pred_txt = "\\sum(1, 10, \\lambda integer k; 1) == 10";
    __gen_e_acsl_assert_data_2.file = "extended_quantifiers.i";
    __gen_e_acsl_assert_data_2.fct = "main";
    __gen_e_acsl_assert_data_2.line = 8;
    __e_acsl_assert(__gen_e_acsl_eq_2 == 0,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
    __gmpz_clear(__gen_e_acsl__5);
    __gmpz_clear(__gen_e_acsl__6);
    __gmpz_clear(__gen_e_acsl_k_2);
    __gmpz_clear(__gen_e_acsl_one_2);
    __gmpz_clear(__gen_e_acsl_lambda_2);
    __gmpz_clear(__gen_e_acsl_accumulator_2);
  }
  /*@ assert \sum(1, 10, \lambda integer k; 1) == 10; */ ;
  {
    __e_acsl_mpz_t __gen_e_acsl__8;
    __e_acsl_mpz_t __gen_e_acsl__9;
    __e_acsl_mpz_t __gen_e_acsl_k_3;
    __e_acsl_mpz_t __gen_e_acsl_one_3;
    int __gen_e_acsl_cond_3;
    __e_acsl_mpz_t __gen_e_acsl_lambda_3;
    __e_acsl_mpz_t __gen_e_acsl_accumulator_3;
    __e_acsl_mpz_t __gen_e_acsl__14;
    int __gen_e_acsl_eq_3;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
      {.values = (void *)0};
    __gmpz_init_set_si(__gen_e_acsl__8,2L);
    __gmpz_init_set_si(__gen_e_acsl__9,10L);
    __gmpz_init_set_si(__gen_e_acsl_one_3,1L);
    __gen_e_acsl_cond_3 = 0;
    __gmpz_init_set_si(__gen_e_acsl_lambda_3,0L);
    __gmpz_init_set_si(__gen_e_acsl_accumulator_3,0L);
    __gmpz_init_set(__gen_e_acsl_k_3,
                    (__e_acsl_mpz_struct const *)(__gen_e_acsl__8));
    while (1) {
      __gen_e_acsl_cond_3 = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_k_3),
                                       (__e_acsl_mpz_struct const *)(__gen_e_acsl__9));
      if (__gen_e_acsl_cond_3 > 0) break;
      else {
        {
          __e_acsl_mpz_t __gen_e_acsl__10;
          __e_acsl_mpz_t __gen_e_acsl_sub;
          __e_acsl_mpz_t __gen_e_acsl__11;
          int __gen_e_acsl_ge;
          __e_acsl_mpz_t __gen_e_acsl_if;
          __gmpz_init_set_si(__gen_e_acsl__10,2L);
          __gmpz_init(__gen_e_acsl_sub);
          __gmpz_sub(__gen_e_acsl_sub,
                     (__e_acsl_mpz_struct const *)(__gen_e_acsl_k_3),
                     (__e_acsl_mpz_struct const *)(__gen_e_acsl__10));
          __gmpz_init_set_si(__gen_e_acsl__11,0L);
          __gen_e_acsl_ge = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_sub),
                                       (__e_acsl_mpz_struct const *)(__gen_e_acsl__11));
          if (__gen_e_acsl_ge >= 0) {
            __e_acsl_mpz_t __gen_e_acsl__12;
            __gmpz_init_set_si(__gen_e_acsl__12,1L);
            __gmpz_init_set(__gen_e_acsl_if,
                            (__e_acsl_mpz_struct const *)(__gen_e_acsl__12));
            __gmpz_clear(__gen_e_acsl__12);
          }
          else {
            __e_acsl_mpz_t __gen_e_acsl__13;
            __gmpz_init_set_si(__gen_e_acsl__13,0L);
            __gmpz_init_set(__gen_e_acsl_if,
                            (__e_acsl_mpz_struct const *)(__gen_e_acsl__13));
            __gmpz_clear(__gen_e_acsl__13);
          }
          __gmpz_set(__gen_e_acsl_lambda_3,
                     (__e_acsl_mpz_struct const *)(__gen_e_acsl_if));
          __gmpz_clear(__gen_e_acsl__10);
          __gmpz_clear(__gen_e_acsl_sub);
          __gmpz_clear(__gen_e_acsl__11);
          __gmpz_clear(__gen_e_acsl_if);
        }
        __gmpz_add(__gen_e_acsl_accumulator_3,
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_accumulator_3),
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_lambda_3));
        __gmpz_add(__gen_e_acsl_k_3,
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_k_3),
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_one_3));
      }
    }
    __gmpz_init_set_si(__gen_e_acsl__14,9L);
    __gen_e_acsl_eq_3 = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_accumulator_3),
                                   (__e_acsl_mpz_struct const *)(__gen_e_acsl__14));
    __e_acsl_assert_register_mpz(& __gen_e_acsl_assert_data_3,
                                 "\\sum(2, 10, \\lambda integer k; k - 2 >= 0? 1: 0)",
                                 0,
                                 (__e_acsl_mpz_struct const *)(__gen_e_acsl_accumulator_3));
    __gen_e_acsl_assert_data_3.blocking = 1;
    __gen_e_acsl_assert_data_3.kind = "Assertion";
    __gen_e_acsl_assert_data_3.pred_txt = "\\numof(2, 10, \\lambda integer k; k - 2 >= 0) == 9";
    __gen_e_acsl_assert_data_3.file = "extended_quantifiers.i";
    __gen_e_acsl_assert_data_3.fct = "main";
    __gen_e_acsl_assert_data_3.line = 10;
    __e_acsl_assert(__gen_e_acsl_eq_3 == 0,& __gen_e_acsl_assert_data_3);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
    __gmpz_clear(__gen_e_acsl__8);
    __gmpz_clear(__gen_e_acsl__9);
    __gmpz_clear(__gen_e_acsl_k_3);
    __gmpz_clear(__gen_e_acsl_one_3);
    __gmpz_clear(__gen_e_acsl_lambda_3);
    __gmpz_clear(__gen_e_acsl_accumulator_3);
    __gmpz_clear(__gen_e_acsl__14);
  }
  /*@ assert \numof(2, 10, \lambda integer k; k - 2 >= 0) == 9; */ ;
  {
    __e_acsl_mpz_t __gen_e_acsl__15;
    __e_acsl_mpz_t __gen_e_acsl__16;
    __e_acsl_mpz_t __gen_e_acsl_k_4;
    __e_acsl_mpz_t __gen_e_acsl_one_4;
    int __gen_e_acsl_cond_4;
    __e_acsl_mpz_t __gen_e_acsl_lambda_4;
    __e_acsl_mpz_t __gen_e_acsl_accumulator_4;
    __e_acsl_mpz_t __gen_e_acsl__17;
    int __gen_e_acsl_eq_4;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
      {.values = (void *)0};
    __gmpz_init_set_si(__gen_e_acsl__15,1L);
    __gmpz_init_set_si(__gen_e_acsl__16,10L);
    __gmpz_init_set_si(__gen_e_acsl_one_4,1L);
    __gen_e_acsl_cond_4 = 0;
    __gmpz_init_set_si(__gen_e_acsl_lambda_4,0L);
    __gmpz_init_set_si(__gen_e_acsl_accumulator_4,1L);
    __gmpz_init_set(__gen_e_acsl_k_4,
                    (__e_acsl_mpz_struct const *)(__gen_e_acsl__15));
    while (1) {
      __gen_e_acsl_cond_4 = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_k_4),
                                       (__e_acsl_mpz_struct const *)(__gen_e_acsl__16));
      if (__gen_e_acsl_cond_4 > 0) break;
      else {
        __gmpz_set(__gen_e_acsl_lambda_4,
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_k_4));
        __gmpz_mul(__gen_e_acsl_accumulator_4,
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_accumulator_4),
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_lambda_4));
        __gmpz_add(__gen_e_acsl_k_4,
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_k_4),
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_one_4));
      }
    }
    __gmpz_init_set_ui(__gen_e_acsl__17,3628800UL);
    __gen_e_acsl_eq_4 = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_accumulator_4),
                                   (__e_acsl_mpz_struct const *)(__gen_e_acsl__17));
    __e_acsl_assert_register_mpz(& __gen_e_acsl_assert_data_4,
                                 "\\product(1, 10, \\lambda integer k; k)",0,
                                 (__e_acsl_mpz_struct const *)(__gen_e_acsl_accumulator_4));
    __gen_e_acsl_assert_data_4.blocking = 1;
    __gen_e_acsl_assert_data_4.kind = "Assertion";
    __gen_e_acsl_assert_data_4.pred_txt = "\\product(1, 10, \\lambda integer k; k) == 3628800";
    __gen_e_acsl_assert_data_4.file = "extended_quantifiers.i";
    __gen_e_acsl_assert_data_4.fct = "main";
    __gen_e_acsl_assert_data_4.line = 12;
    __e_acsl_assert(__gen_e_acsl_eq_4 == 0,& __gen_e_acsl_assert_data_4);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
    __gmpz_clear(__gen_e_acsl__15);
    __gmpz_clear(__gen_e_acsl__16);
    __gmpz_clear(__gen_e_acsl_k_4);
    __gmpz_clear(__gen_e_acsl_one_4);
    __gmpz_clear(__gen_e_acsl_lambda_4);
    __gmpz_clear(__gen_e_acsl_accumulator_4);
    __gmpz_clear(__gen_e_acsl__17);
  }
  /*@ assert \product(1, 10, \lambda integer k; k) == 3628800; */ ;
  {
    __e_acsl_mpz_t __gen_e_acsl__18;
    __e_acsl_mpz_t __gen_e_acsl_neg;
    __e_acsl_mpz_t __gen_e_acsl_k_5;
    __e_acsl_mpz_t __gen_e_acsl_one_5;
    int __gen_e_acsl_cond_5;
    __e_acsl_mpz_t __gen_e_acsl_lambda_5;
    __e_acsl_mpz_t __gen_e_acsl_accumulator_5;
    __e_acsl_mpz_t __gen_e_acsl__19;
    int __gen_e_acsl_eq_5;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
      {.values = (void *)0};
    __gmpz_init_set_si(__gen_e_acsl__18,10L);
    __gmpz_init(__gen_e_acsl_neg);
    __gmpz_neg(__gen_e_acsl_neg,
               (__e_acsl_mpz_struct const *)(__gen_e_acsl__18));
    __gmpz_init_set_si(__gen_e_acsl_one_5,1L);
    __gen_e_acsl_cond_5 = 0;
    __gmpz_init_set_si(__gen_e_acsl_lambda_5,0L);
    __gmpz_init_set_si(__gen_e_acsl_accumulator_5,1L);
    __gmpz_init_set(__gen_e_acsl_k_5,
                    (__e_acsl_mpz_struct const *)(__gen_e_acsl_neg));
    while (1) {
      __gen_e_acsl_cond_5 = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_k_5),
                                       (__e_acsl_mpz_struct const *)(__gen_e_acsl__18));
      if (__gen_e_acsl_cond_5 > 0) break;
      else {
        __gmpz_set(__gen_e_acsl_lambda_5,
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_k_5));
        __gmpz_mul(__gen_e_acsl_accumulator_5,
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_accumulator_5),
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_lambda_5));
        __gmpz_add(__gen_e_acsl_k_5,
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_k_5),
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_one_5));
      }
    }
    __gmpz_init_set_si(__gen_e_acsl__19,0L);
    __gen_e_acsl_eq_5 = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_accumulator_5),
                                   (__e_acsl_mpz_struct const *)(__gen_e_acsl__19));
    __e_acsl_assert_register_mpz(& __gen_e_acsl_assert_data_5,
                                 "\\product(-10, 10, \\lambda integer k; k)",
                                 0,
                                 (__e_acsl_mpz_struct const *)(__gen_e_acsl_accumulator_5));
    __gen_e_acsl_assert_data_5.blocking = 1;
    __gen_e_acsl_assert_data_5.kind = "Assertion";
    __gen_e_acsl_assert_data_5.pred_txt = "\\product(-10, 10, \\lambda integer k; k) == 0";
    __gen_e_acsl_assert_data_5.file = "extended_quantifiers.i";
    __gen_e_acsl_assert_data_5.fct = "main";
    __gen_e_acsl_assert_data_5.line = 13;
    __e_acsl_assert(__gen_e_acsl_eq_5 == 0,& __gen_e_acsl_assert_data_5);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
    __gmpz_clear(__gen_e_acsl__18);
    __gmpz_clear(__gen_e_acsl_neg);
    __gmpz_clear(__gen_e_acsl_k_5);
    __gmpz_clear(__gen_e_acsl_one_5);
    __gmpz_clear(__gen_e_acsl_lambda_5);
    __gmpz_clear(__gen_e_acsl_accumulator_5);
    __gmpz_clear(__gen_e_acsl__19);
  }
  /*@ assert \product(-10, 10, \lambda integer k; k) == 0; */ ;
  __retres = 0;
  __e_acsl_memory_clean();
  return __retres;
}

