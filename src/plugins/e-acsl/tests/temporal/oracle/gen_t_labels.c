/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "time.h"
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

void foo(int *a, int *b)
{
  __e_acsl_store_block((void *)(& b),8UL);
  __e_acsl_store_block((void *)(& a),8UL);
  __e_acsl_temporal_pull_parameter((void *)(& a),0U,8UL);
  __e_acsl_temporal_pull_parameter((void *)(& b),1U,8UL);
  int t = *a;
  __e_acsl_initialize((void *)a,sizeof(int));
  *a = *b;
  __e_acsl_initialize((void *)b,sizeof(int));
  *b = t;
  __e_acsl_delete_block((void *)(& b));
  __e_acsl_delete_block((void *)(& a));
  return;
}

int *Q;
int *bar(void)
{
  RET: ;
  return Q;
}

int main(int argc, char const **argv)
{
  int __retres;
  __e_acsl_memory_init(& argc,(char ***)(& argv),8UL);
  int a = 11;
  __e_acsl_store_block((void *)(& a),4UL);
  __e_acsl_full_init((void *)(& a));
  int b = 12;
  __e_acsl_store_block((void *)(& b),4UL);
  __e_acsl_full_init((void *)(& b));
  int *p = & a;
  __e_acsl_temporal_store_nblock((void *)(& p),(void *)(& a));
  __e_acsl_store_block((void *)(& p),8UL);
  __e_acsl_full_init((void *)(& p));
  int *q = & b;
  __e_acsl_temporal_store_nblock((void *)(& q),(void *)(& b));
  __e_acsl_store_block((void *)(& q),8UL);
  __e_acsl_full_init((void *)(& q));
  LAB:
  __e_acsl_temporal_reset_parameters();
  __e_acsl_temporal_reset_return();
  __e_acsl_temporal_save_nreferent_parameter((void *)(& p),0U);
  __e_acsl_temporal_save_nreferent_parameter((void *)(& q),1U);
  foo(p,q);
  {
    int __gen_e_acsl_initialized;
    int __gen_e_acsl_and;
    int __gen_e_acsl_and_3;
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
    if (__gen_e_acsl_and) {
      int __gen_e_acsl_initialized_2;
      int __gen_e_acsl_and_2;
      __gen_e_acsl_initialized_2 = __e_acsl_initialized((void *)(& q),
                                                        sizeof(int *));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"&q",
                                   (void *)(& q));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                     "sizeof(int *)",0,sizeof(int *));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,
                                   "\\initialized(&q)",0,
                                   __gen_e_acsl_initialized_2);
      if (__gen_e_acsl_initialized_2) {
        int __gen_e_acsl_valid_2;
        __gen_e_acsl_valid_2 = __e_acsl_valid((void *)q,sizeof(int),
                                              (void *)q,(void *)(& q));
        __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"q",
                                     (void *)q);
        __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                       "sizeof(int)",0,sizeof(int));
        __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"\\valid(q)",
                                     0,__gen_e_acsl_valid_2);
        __gen_e_acsl_and_2 = __gen_e_acsl_valid_2;
      }
      else __gen_e_acsl_and_2 = 0;
      __gen_e_acsl_and_3 = __gen_e_acsl_and_2;
    }
    else __gen_e_acsl_and_3 = 0;
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "\\valid(p) && \\valid(q)";
    __gen_e_acsl_assert_data.file = "t_labels.c";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 26;
    __e_acsl_assert(__gen_e_acsl_and_3,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert \valid(p) && \valid(q); */ ;
  LAB2:
  __e_acsl_full_init((void *)(& q));
  __e_acsl_temporal_store_nreferent((void *)(& q),(void *)(& p));
  q = p;
  {
    int __gen_e_acsl_initialized_3;
    int __gen_e_acsl_and_4;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __gen_e_acsl_initialized_3 = __e_acsl_initialized((void *)(& p),
                                                      sizeof(int *));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"&p",
                                 (void *)(& p));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                   "sizeof(int *)",0,sizeof(int *));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                 "\\initialized(&p)",0,
                                 __gen_e_acsl_initialized_3);
    if (__gen_e_acsl_initialized_3) {
      int __gen_e_acsl_valid_3;
      __gen_e_acsl_valid_3 = __e_acsl_valid((void *)p,sizeof(int),(void *)p,
                                            (void *)(& p));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"p",
                                   (void *)p);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"\\valid(p)",
                                   0,__gen_e_acsl_valid_3);
      __gen_e_acsl_and_4 = __gen_e_acsl_valid_3;
    }
    else __gen_e_acsl_and_4 = 0;
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Assertion";
    __gen_e_acsl_assert_data_2.pred_txt = "\\valid(p)";
    __gen_e_acsl_assert_data_2.file = "t_labels.c";
    __gen_e_acsl_assert_data_2.fct = "main";
    __gen_e_acsl_assert_data_2.line = 30;
    __e_acsl_assert(__gen_e_acsl_and_4,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
  }
  /*@ assert \valid(p); */ ;
  __retres = 0;
  __e_acsl_delete_block((void *)(& q));
  __e_acsl_delete_block((void *)(& p));
  __e_acsl_delete_block((void *)(& b));
  __e_acsl_delete_block((void *)(& a));
  __e_acsl_memory_clean();
  return __retres;
}

