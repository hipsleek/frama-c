/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "time.h"
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

int f(void)
{
  int __retres;
  int a = 10;
  __e_acsl_store_block((void *)(& a),4UL);
  __e_acsl_full_init((void *)(& a));
  goto lbl_1;
  lbl_2:
  {
    int __gen_e_acsl_valid;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_valid = __e_acsl_valid((void *)(& a),sizeof(int),
                                        (void *)(& a),(void *)0);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"&a",
                                 (void *)(& a));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,"sizeof(int)",
                                   0,sizeof(int));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"\\valid(&a)",0,
                                 __gen_e_acsl_valid);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "\\valid(&a)";
    __gen_e_acsl_assert_data.file = "issue-eacsl-105.c";
    __gen_e_acsl_assert_data.fct = "f";
    __gen_e_acsl_assert_data.line = 11;
    __e_acsl_assert(__gen_e_acsl_valid,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert \valid(&a); */ ;
  __retres = 0;
  goto return_label;
  lbl_1: goto lbl_2;
  return_label: {
                  __e_acsl_delete_block((void *)(& a));
                  return __retres;
                }
}

int main(void)
{
  int __retres;
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  f();
  __retres = 0;
  __e_acsl_memory_clean();
  return __retres;
}

