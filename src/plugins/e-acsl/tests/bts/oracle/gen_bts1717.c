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
  int *p;
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  __e_acsl_store_block((void *)(& p),8UL);
  int a = 10;
  __e_acsl_store_block((void *)(& a),4UL);
  __e_acsl_full_init((void *)(& a));
  goto lbl_1;
  lbl_2:
  {
    int __gen_e_acsl_initialized;
    int __gen_e_acsl_and;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_initialized = __e_acsl_initialized((void *)(& p),
                                                    sizeof(int *));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"&p",
                                 (void *)(& p));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                   "sizeof(int *)",0,sizeof(int *));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,
                                 "\\initialized(&p)",0,
                                 __gen_e_acsl_initialized);
    if (__gen_e_acsl_initialized) {
      int __gen_e_acsl_valid;
      __gen_e_acsl_valid = __e_acsl_valid((void *)p,sizeof(int),(void *)p,
                                          (void *)(& p));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"p",(void *)p);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"\\valid(p)",0,
                                   __gen_e_acsl_valid);
      __gen_e_acsl_and = __gen_e_acsl_valid;
    }
    else __gen_e_acsl_and = 0;
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "\\valid(p)";
    __gen_e_acsl_assert_data.file = "bts1717.i";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 10;
    __e_acsl_assert(__gen_e_acsl_and,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert \valid(p); */ ;
  __retres = 0;
  goto return_label;
  lbl_1: __e_acsl_full_init((void *)(& p));
         p = & a;
  goto lbl_2;
  return_label:
  {
    __e_acsl_delete_block((void *)(& p));
    __e_acsl_delete_block((void *)(& a));
    __e_acsl_memory_clean();
    return __retres;
  }
}


