/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "time.h"
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

/*@ logic integer identity(integer n) = n <= 0? n: identity(n - 1) + 1;

*/
void __gen_e_acsl_identity(__e_acsl_mpz_t *__retres_arg,
                           __e_acsl_mpz_struct * n);

int main(void)
{
  int __retres;
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  int i = 1;
  {
    __e_acsl_mpz_t __gen_e_acsl_i;
    __e_acsl_mpz_t __gen_e_acsl_identity_4;
    int __gen_e_acsl_eq;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gmpz_init_set_si(__gen_e_acsl_i,(long)i);
    __gen_e_acsl_identity(& __gen_e_acsl_identity_4,
                          (__e_acsl_mpz_struct *)__gen_e_acsl_i);
    __gen_e_acsl_eq = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_identity_4),
                                 (__e_acsl_mpz_struct const *)(__gen_e_acsl_i));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"i",0,i);
    __e_acsl_assert_register_mpz(& __gen_e_acsl_assert_data,"identity(i)",0,
                                 (__e_acsl_mpz_struct const *)(__gen_e_acsl_identity_4));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"i",0,i);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "identity(i) == i";
    __gen_e_acsl_assert_data.file = "rec_int.c";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 9;
    __e_acsl_assert(__gen_e_acsl_eq == 0,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    __gmpz_clear(__gen_e_acsl_i);
    __gmpz_clear(__gen_e_acsl_identity_4);
  }
  /*@ assert identity(i) == i; */ ;
  __retres = 0;
  __e_acsl_memory_clean();
  return __retres;
}

/*@ assigns (*__retres_arg)[0];
    assigns (*__retres_arg)[0] \from *((__e_acsl_mpz_struct *)n);
 */
void __gen_e_acsl_identity(__e_acsl_mpz_t *__retres_arg,
                           __e_acsl_mpz_struct * n)
{
  __e_acsl_mpz_t __gen_e_acsl_;
  int __gen_e_acsl_le;
  __e_acsl_mpz_t __gen_e_acsl_if;
  __gmpz_init_set_si(__gen_e_acsl_,0L);
  __gen_e_acsl_le = __gmpz_cmp((__e_acsl_mpz_struct const *)(n),
                               (__e_acsl_mpz_struct const *)(__gen_e_acsl_));
  if (__gen_e_acsl_le <= 0) __gmpz_init_set(__gen_e_acsl_if,
                                            (__e_acsl_mpz_struct const *)(n));
  else {
    __e_acsl_mpz_t __gen_e_acsl__2;
    __e_acsl_mpz_t __gen_e_acsl_sub;
    __e_acsl_mpz_t __gen_e_acsl_identity_3;
    __e_acsl_mpz_t __gen_e_acsl_add;
    __gmpz_init_set_si(__gen_e_acsl__2,1L);
    __gmpz_init(__gen_e_acsl_sub);
    __gmpz_sub(__gen_e_acsl_sub,(__e_acsl_mpz_struct const *)(n),
               (__e_acsl_mpz_struct const *)(__gen_e_acsl__2));
    __gen_e_acsl_identity(& __gen_e_acsl_identity_3,
                          (__e_acsl_mpz_struct *)__gen_e_acsl_sub);
    __gmpz_init(__gen_e_acsl_add);
    __gmpz_add(__gen_e_acsl_add,
               (__e_acsl_mpz_struct const *)(__gen_e_acsl_identity_3),
               (__e_acsl_mpz_struct const *)(__gen_e_acsl__2));
    __gmpz_init_set(__gen_e_acsl_if,
                    (__e_acsl_mpz_struct const *)(__gen_e_acsl_add));
    __gmpz_clear(__gen_e_acsl__2);
    __gmpz_clear(__gen_e_acsl_sub);
    __gmpz_clear(__gen_e_acsl_identity_3);
    __gmpz_clear(__gen_e_acsl_add);
  }
  __gmpz_init_set(*__retres_arg,
                  (__e_acsl_mpz_struct const *)(__gen_e_acsl_if));
  __gmpz_clear(__gen_e_acsl_);
  __gmpz_clear(__gen_e_acsl_if);
  return;
}

