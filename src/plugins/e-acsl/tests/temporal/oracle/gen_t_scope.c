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
  int *p = (int *)0;
  __e_acsl_temporal_store_nblock((void *)(& p),(void *)0);
  __e_acsl_store_block((void *)(& p),8UL);
  __e_acsl_full_init((void *)(& p));
  int *q = (int *)0;
  __e_acsl_temporal_store_nblock((void *)(& q),(void *)0);
  __e_acsl_store_block((void *)(& q),8UL);
  __e_acsl_full_init((void *)(& q));
  {
    int i = 9;
    __e_acsl_store_block((void *)(& i),4UL);
    __e_acsl_full_init((void *)(& i));
    __e_acsl_full_init((void *)(& p));
    __e_acsl_temporal_store_nblock((void *)(& p),(void *)(& i));
    p = & i;
    __e_acsl_full_init((void *)(& q));
    __e_acsl_temporal_store_nreferent((void *)(& q),(void *)(& p));
    q = p;
    __e_acsl_delete_block((void *)(& i));
  }
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
      /*@ assert Eva: dangling_pointer: !\dangling(&p); */
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
    __gen_e_acsl_assert_data.pred_txt = "!\\valid(p)";
    __gen_e_acsl_assert_data.file = "t_scope.c";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 15;
    __e_acsl_assert(! __gen_e_acsl_and,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert !\valid(p); */ ;
  {
    int __gen_e_acsl_initialized_2;
    int __gen_e_acsl_and_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __gen_e_acsl_initialized_2 = __e_acsl_initialized((void *)(& q),
                                                      sizeof(int *));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"&q",
                                 (void *)(& q));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                   "sizeof(int *)",0,sizeof(int *));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                 "\\initialized(&q)",0,
                                 __gen_e_acsl_initialized_2);
    if (__gen_e_acsl_initialized_2) {
      int __gen_e_acsl_valid_2;
      __gen_e_acsl_valid_2 = __e_acsl_valid((void *)q,sizeof(int),(void *)q,
                                            (void *)(& q));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"q",
                                   (void *)q);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"\\valid(q)",
                                   0,__gen_e_acsl_valid_2);
      __gen_e_acsl_and_2 = __gen_e_acsl_valid_2;
    }
    else __gen_e_acsl_and_2 = 0;
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Assertion";
    __gen_e_acsl_assert_data_2.pred_txt = "!\\valid(q)";
    __gen_e_acsl_assert_data_2.file = "t_scope.c";
    __gen_e_acsl_assert_data_2.fct = "main";
    __gen_e_acsl_assert_data_2.line = 16;
    __e_acsl_assert(! __gen_e_acsl_and_2,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
  }
  /*@ assert !\valid(q); */ ;
  {
    int j = 8;
    __e_acsl_store_block((void *)(& j),4UL);
    __e_acsl_full_init((void *)(& j));
    __e_acsl_full_init((void *)(& p));
    __e_acsl_temporal_store_nblock((void *)(& p),(void *)(& j));
    p = & j;
    {
      int __gen_e_acsl_initialized_3;
      int __gen_e_acsl_and_3;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
        {.values = (void *)0};
      __gen_e_acsl_initialized_3 = __e_acsl_initialized((void *)(& p),
                                                        sizeof(int *));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,"&p",
                                   (void *)(& p));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_3,
                                     "sizeof(int *)",0,sizeof(int *));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,
                                   "\\initialized(&p)",0,
                                   __gen_e_acsl_initialized_3);
      if (__gen_e_acsl_initialized_3) {
        int __gen_e_acsl_valid_3;
        __gen_e_acsl_valid_3 = __e_acsl_valid((void *)p,sizeof(int),
                                              (void *)p,(void *)(& p));
        __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,"p",
                                     (void *)p);
        __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_3,
                                       "sizeof(int)",0,sizeof(int));
        __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,
                                     "\\valid(p)",0,__gen_e_acsl_valid_3);
        __gen_e_acsl_and_3 = __gen_e_acsl_valid_3;
      }
      else __gen_e_acsl_and_3 = 0;
      __gen_e_acsl_assert_data_3.blocking = 1;
      __gen_e_acsl_assert_data_3.kind = "Assertion";
      __gen_e_acsl_assert_data_3.pred_txt = "\\valid(p)";
      __gen_e_acsl_assert_data_3.file = "t_scope.c";
      __gen_e_acsl_assert_data_3.fct = "main";
      __gen_e_acsl_assert_data_3.line = 21;
      __e_acsl_assert(__gen_e_acsl_and_3,& __gen_e_acsl_assert_data_3);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
    }
    /*@ assert \valid(p); */ ;
    __e_acsl_initialize((void *)p,sizeof(int));
    *p = 1;
    {
      int __gen_e_acsl_initialized_4;
      int __gen_e_acsl_and_4;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
        {.values = (void *)0};
      __gen_e_acsl_initialized_4 = __e_acsl_initialized((void *)(& q),
                                                        sizeof(int *));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"&q",
                                   (void *)(& q));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,
                                     "sizeof(int *)",0,sizeof(int *));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,
                                   "\\initialized(&q)",0,
                                   __gen_e_acsl_initialized_4);
      if (__gen_e_acsl_initialized_4) {
        int __gen_e_acsl_valid_4;
        __gen_e_acsl_valid_4 = __e_acsl_valid((void *)q,sizeof(int),
                                              (void *)q,(void *)(& q));
        __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"q",
                                     (void *)q);
        __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,
                                       "sizeof(int)",0,sizeof(int));
        __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,
                                     "\\valid(q)",0,__gen_e_acsl_valid_4);
        __gen_e_acsl_and_4 = __gen_e_acsl_valid_4;
      }
      else __gen_e_acsl_and_4 = 0;
      __gen_e_acsl_assert_data_4.blocking = 1;
      __gen_e_acsl_assert_data_4.kind = "Assertion";
      __gen_e_acsl_assert_data_4.pred_txt = "!\\valid(q)";
      __gen_e_acsl_assert_data_4.file = "t_scope.c";
      __gen_e_acsl_assert_data_4.fct = "main";
      __gen_e_acsl_assert_data_4.line = 24;
      __e_acsl_assert(! __gen_e_acsl_and_4,& __gen_e_acsl_assert_data_4);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
    }
    /*@ assert !\valid(q); */ ;
    {
      int __gen_e_acsl_valid_5;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
        {.values = (void *)0};
      __gen_e_acsl_valid_5 = __e_acsl_valid((void *)(& j),sizeof(int),
                                            (void *)(& j),(void *)0);
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_5,"&j",
                                   (void *)(& j));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_5,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,
                                   "\\valid(&j)",0,__gen_e_acsl_valid_5);
      __gen_e_acsl_assert_data_5.blocking = 1;
      __gen_e_acsl_assert_data_5.kind = "Assertion";
      __gen_e_acsl_assert_data_5.pred_txt = "\\valid(&j)";
      __gen_e_acsl_assert_data_5.file = "t_scope.c";
      __gen_e_acsl_assert_data_5.fct = "main";
      __gen_e_acsl_assert_data_5.line = 25;
      __e_acsl_assert(__gen_e_acsl_valid_5,& __gen_e_acsl_assert_data_5);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
    }
    /*@ assert \valid(&j); */ ;
    __e_acsl_delete_block((void *)(& j));
  }
  int len = 3;
  __e_acsl_full_init((void *)(& p));
  __e_acsl_temporal_store_nblock((void *)(& p),(void *)0);
  p = (int *)0;
  __e_acsl_full_init((void *)(& q));
  __e_acsl_temporal_store_nblock((void *)(& q),(void *)0);
  q = (int *)0;
  while (len) {
    int a;
    __e_acsl_store_block((void *)(& a),4UL);
    {
      int __gen_e_acsl_initialized_5;
      int __gen_e_acsl_and_5;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_6 =
        {.values = (void *)0};
      __gen_e_acsl_initialized_5 = __e_acsl_initialized((void *)(& p),
                                                        sizeof(int *));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_6,"&p",
                                   (void *)(& p));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_6,
                                     "sizeof(int *)",0,sizeof(int *));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_6,
                                   "\\initialized(&p)",0,
                                   __gen_e_acsl_initialized_5);
      if (__gen_e_acsl_initialized_5) {
        int __gen_e_acsl_valid_6;
        __gen_e_acsl_valid_6 = __e_acsl_valid((void *)p,sizeof(int),
                                              (void *)p,(void *)(& p));
        __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_6,"p",
                                     (void *)p);
        __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_6,
                                       "sizeof(int)",0,sizeof(int));
        __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_6,
                                     "\\valid(p)",0,__gen_e_acsl_valid_6);
        __gen_e_acsl_and_5 = __gen_e_acsl_valid_6;
      }
      else __gen_e_acsl_and_5 = 0;
      __gen_e_acsl_assert_data_6.blocking = 1;
      __gen_e_acsl_assert_data_6.kind = "Assertion";
      __gen_e_acsl_assert_data_6.pred_txt = "!\\valid(p)";
      __gen_e_acsl_assert_data_6.file = "t_scope.c";
      __gen_e_acsl_assert_data_6.fct = "main";
      __gen_e_acsl_assert_data_6.line = 33;
      __e_acsl_assert(! __gen_e_acsl_and_5,& __gen_e_acsl_assert_data_6);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_6);
    }
    /*@ assert !\valid(p); */ ;
    __e_acsl_full_init((void *)(& q));
    __e_acsl_temporal_store_nblock((void *)(& q),(void *)(& a));
    q = & a;
    __e_acsl_full_init((void *)(& p));
    __e_acsl_temporal_store_nreferent((void *)(& p),(void *)(& q));
    p = q;
    {
      int __gen_e_acsl_initialized_6;
      int __gen_e_acsl_and_6;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_7 =
        {.values = (void *)0};
      __gen_e_acsl_initialized_6 = __e_acsl_initialized((void *)(& p),
                                                        sizeof(int *));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_7,"&p",
                                   (void *)(& p));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_7,
                                     "sizeof(int *)",0,sizeof(int *));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_7,
                                   "\\initialized(&p)",0,
                                   __gen_e_acsl_initialized_6);
      if (__gen_e_acsl_initialized_6) {
        int __gen_e_acsl_valid_7;
        __gen_e_acsl_valid_7 = __e_acsl_valid((void *)p,sizeof(int),
                                              (void *)p,(void *)(& p));
        __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_7,"p",
                                     (void *)p);
        __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_7,
                                       "sizeof(int)",0,sizeof(int));
        __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_7,
                                     "\\valid(p)",0,__gen_e_acsl_valid_7);
        __gen_e_acsl_and_6 = __gen_e_acsl_valid_7;
      }
      else __gen_e_acsl_and_6 = 0;
      __gen_e_acsl_assert_data_7.blocking = 1;
      __gen_e_acsl_assert_data_7.kind = "Assertion";
      __gen_e_acsl_assert_data_7.pred_txt = "\\valid(p)";
      __gen_e_acsl_assert_data_7.file = "t_scope.c";
      __gen_e_acsl_assert_data_7.fct = "main";
      __gen_e_acsl_assert_data_7.line = 36;
      __e_acsl_assert(__gen_e_acsl_and_6,& __gen_e_acsl_assert_data_7);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_7);
    }
    /*@ assert \valid(p); */ ;
    len --;
    __e_acsl_delete_block((void *)(& a));
  }
  __retres = 0;
  __e_acsl_delete_block((void *)(& q));
  __e_acsl_delete_block((void *)(& p));
  __e_acsl_memory_clean();
  return __retres;
}


