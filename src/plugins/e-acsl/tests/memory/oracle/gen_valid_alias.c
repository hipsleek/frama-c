/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "time.h"
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

int main(void)
{
  int __retres;
  int *a;
  int *b;
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  __e_acsl_store_block((void *)(& b),8UL);
  __e_acsl_store_block((void *)(& a),8UL);
  int n = 0;
  {
    int __gen_e_acsl_initialized;
    int __gen_e_acsl_and;
    int __gen_e_acsl_and_3;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_initialized = __e_acsl_initialized((void *)(& a),
                                                    sizeof(int *));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"&a",
                                 (void *)(& a));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                   "sizeof(int *)",0,sizeof(int *));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,
                                 "\\initialized(&a)",0,
                                 __gen_e_acsl_initialized);
    if (__gen_e_acsl_initialized) {
      int __gen_e_acsl_valid;
      __gen_e_acsl_valid = __e_acsl_valid((void *)a,sizeof(int),(void *)a,
                                          (void *)(& a));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"a",(void *)a);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"\\valid(a)",0,
                                   __gen_e_acsl_valid);
      __gen_e_acsl_and = __gen_e_acsl_valid;
    }
    else __gen_e_acsl_and = 0;
    if (! __gen_e_acsl_and) {
      int __gen_e_acsl_initialized_2;
      int __gen_e_acsl_and_2;
      __gen_e_acsl_initialized_2 = __e_acsl_initialized((void *)(& b),
                                                        sizeof(int *));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"&b",
                                   (void *)(& b));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                     "sizeof(int *)",0,sizeof(int *));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,
                                   "\\initialized(&b)",0,
                                   __gen_e_acsl_initialized_2);
      if (__gen_e_acsl_initialized_2) {
        int __gen_e_acsl_valid_2;
        __gen_e_acsl_valid_2 = __e_acsl_valid((void *)b,sizeof(int),
                                              (void *)b,(void *)(& b));
        __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"b",
                                     (void *)b);
        __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                       "sizeof(int)",0,sizeof(int));
        __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"\\valid(b)",
                                     0,__gen_e_acsl_valid_2);
        __gen_e_acsl_and_2 = __gen_e_acsl_valid_2;
      }
      else __gen_e_acsl_and_2 = 0;
      __gen_e_acsl_and_3 = ! __gen_e_acsl_and_2;
    }
    else __gen_e_acsl_and_3 = 0;
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "!\\valid(a) && !\\valid(b)";
    __gen_e_acsl_assert_data.file = "valid_alias.c";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 9;
    __e_acsl_assert(__gen_e_acsl_and_3,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert !\valid(a) && !\valid(b); */ ;
  __e_acsl_full_init((void *)(& a));
  a = (int *)malloc(sizeof(int));
  __e_acsl_initialize((void *)a,sizeof(int));
  *a = n;
  __e_acsl_full_init((void *)(& b));
  b = a;
  {
    int __gen_e_acsl_initialized_3;
    int __gen_e_acsl_and_4;
    int __gen_e_acsl_and_6;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __gen_e_acsl_initialized_3 = __e_acsl_initialized((void *)(& a),
                                                      sizeof(int *));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"&a",
                                 (void *)(& a));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                   "sizeof(int *)",0,sizeof(int *));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                 "\\initialized(&a)",0,
                                 __gen_e_acsl_initialized_3);
    if (__gen_e_acsl_initialized_3) {
      int __gen_e_acsl_valid_3;
      __gen_e_acsl_valid_3 = __e_acsl_valid((void *)a,sizeof(int),(void *)a,
                                            (void *)(& a));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"a",
                                   (void *)a);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"\\valid(a)",
                                   0,__gen_e_acsl_valid_3);
      __gen_e_acsl_and_4 = __gen_e_acsl_valid_3;
    }
    else __gen_e_acsl_and_4 = 0;
    if (__gen_e_acsl_and_4) {
      int __gen_e_acsl_initialized_4;
      int __gen_e_acsl_and_5;
      __gen_e_acsl_initialized_4 = __e_acsl_initialized((void *)(& b),
                                                        sizeof(int *));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"&b",
                                   (void *)(& b));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                     "sizeof(int *)",0,sizeof(int *));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                   "\\initialized(&b)",0,
                                   __gen_e_acsl_initialized_4);
      if (__gen_e_acsl_initialized_4) {
        int __gen_e_acsl_valid_4;
        __gen_e_acsl_valid_4 = __e_acsl_valid((void *)b,sizeof(int),
                                              (void *)b,(void *)(& b));
        __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"b",
                                     (void *)b);
        __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                       "sizeof(int)",0,sizeof(int));
        __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                     "\\valid(b)",0,__gen_e_acsl_valid_4);
        __gen_e_acsl_and_5 = __gen_e_acsl_valid_4;
      }
      else __gen_e_acsl_and_5 = 0;
      __gen_e_acsl_and_6 = __gen_e_acsl_and_5;
    }
    else __gen_e_acsl_and_6 = 0;
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Assertion";
    __gen_e_acsl_assert_data_2.pred_txt = "\\valid(a) && \\valid(b)";
    __gen_e_acsl_assert_data_2.file = "valid_alias.c";
    __gen_e_acsl_assert_data_2.fct = "main";
    __gen_e_acsl_assert_data_2.line = 13;
    __e_acsl_assert(__gen_e_acsl_and_6,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
  }
  /*@ assert \valid(a) && \valid(b); */ ;
  {
    int __gen_e_acsl_initialized_5;
    int __gen_e_acsl_and_7;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
      {.values = (void *)0};
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
      {.values = (void *)0};
    __gen_e_acsl_initialized_5 = __e_acsl_initialized((void *)(& b),
                                                      sizeof(int *));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"&b",
                                 (void *)(& b));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,
                                   "sizeof(int *)",0,sizeof(int *));
    if (__gen_e_acsl_initialized_5) {
      int __gen_e_acsl_valid_read;
      __gen_e_acsl_valid_read = __e_acsl_valid_read((void *)b,sizeof(int),
                                                    (void *)b,(void *)(& b));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"b",
                                   (void *)b);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,
                                     "sizeof(int)",0,sizeof(int));
      __gen_e_acsl_and_7 = __gen_e_acsl_valid_read;
    }
    else __gen_e_acsl_and_7 = 0;
    __gen_e_acsl_assert_data_4.blocking = 1;
    __gen_e_acsl_assert_data_4.kind = "RTE";
    __gen_e_acsl_assert_data_4.pred_txt = "\\valid_read(b)";
    __gen_e_acsl_assert_data_4.file = "valid_alias.c";
    __gen_e_acsl_assert_data_4.fct = "main";
    __gen_e_acsl_assert_data_4.line = 14;
    __gen_e_acsl_assert_data_4.name = "mem_access";
    __e_acsl_assert(__gen_e_acsl_and_7,& __gen_e_acsl_assert_data_4);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"*b",0,*b);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"n",0,n);
    __gen_e_acsl_assert_data_3.blocking = 1;
    __gen_e_acsl_assert_data_3.kind = "Assertion";
    __gen_e_acsl_assert_data_3.pred_txt = "*b == n";
    __gen_e_acsl_assert_data_3.file = "valid_alias.c";
    __gen_e_acsl_assert_data_3.fct = "main";
    __gen_e_acsl_assert_data_3.line = 14;
    __e_acsl_assert(*b == n,& __gen_e_acsl_assert_data_3);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
  }
  /*@ assert *b == n; */ ;
  free((void *)b);
  {
    int __gen_e_acsl_initialized_6;
    int __gen_e_acsl_and_8;
    int __gen_e_acsl_and_10;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
      {.values = (void *)0};
    __gen_e_acsl_initialized_6 = __e_acsl_initialized((void *)(& a),
                                                      sizeof(int *));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_5,"&a",
                                 (void *)(& a));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_5,
                                   "sizeof(int *)",0,sizeof(int *));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,
                                 "\\initialized(&a)",0,
                                 __gen_e_acsl_initialized_6);
    if (__gen_e_acsl_initialized_6) {
      int __gen_e_acsl_valid_5;
      /*@ assert Eva: dangling_pointer: !\dangling(&a); */
      __gen_e_acsl_valid_5 = __e_acsl_valid((void *)a,sizeof(int),(void *)a,
                                            (void *)(& a));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_5,"a",
                                   (void *)a);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_5,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"\\valid(a)",
                                   0,__gen_e_acsl_valid_5);
      __gen_e_acsl_and_8 = __gen_e_acsl_valid_5;
    }
    else __gen_e_acsl_and_8 = 0;
    if (! __gen_e_acsl_and_8) {
      int __gen_e_acsl_initialized_7;
      int __gen_e_acsl_and_9;
      __gen_e_acsl_initialized_7 = __e_acsl_initialized((void *)(& b),
                                                        sizeof(int *));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_5,"&b",
                                   (void *)(& b));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_5,
                                     "sizeof(int *)",0,sizeof(int *));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,
                                   "\\initialized(&b)",0,
                                   __gen_e_acsl_initialized_7);
      if (__gen_e_acsl_initialized_7) {
        int __gen_e_acsl_valid_6;
        __gen_e_acsl_valid_6 = __e_acsl_valid((void *)b,sizeof(int),
                                              (void *)b,(void *)(& b));
        __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_5,"b",
                                     (void *)b);
        __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_5,
                                       "sizeof(int)",0,sizeof(int));
        __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,
                                     "\\valid(b)",0,__gen_e_acsl_valid_6);
        __gen_e_acsl_and_9 = __gen_e_acsl_valid_6;
      }
      else __gen_e_acsl_and_9 = 0;
      __gen_e_acsl_and_10 = ! __gen_e_acsl_and_9;
    }
    else __gen_e_acsl_and_10 = 0;
    __gen_e_acsl_assert_data_5.blocking = 1;
    __gen_e_acsl_assert_data_5.kind = "Assertion";
    __gen_e_acsl_assert_data_5.pred_txt = "!\\valid(a) && !\\valid(b)";
    __gen_e_acsl_assert_data_5.file = "valid_alias.c";
    __gen_e_acsl_assert_data_5.fct = "main";
    __gen_e_acsl_assert_data_5.line = 16;
    __e_acsl_assert(__gen_e_acsl_and_10,& __gen_e_acsl_assert_data_5);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
  }
  /*@ assert !\valid(a) && !\valid(b); */ ;
  __retres = 0;
  __e_acsl_delete_block((void *)(& b));
  __e_acsl_delete_block((void *)(& a));
  __e_acsl_memory_clean();
  return __retres;
}

