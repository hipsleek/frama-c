/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "time.h"
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

int G = 0;
int *P;
void __e_acsl_globals_init(void)
{
  static char __e_acsl_already_run = 0;
  if (! __e_acsl_already_run) {
    __e_acsl_already_run = 1;
    __e_acsl_store_block((void *)(& P),8UL);
    __e_acsl_full_init((void *)(& P));
    __e_acsl_store_block((void *)(& G),4UL);
    __e_acsl_full_init((void *)(& G));
  }
  return;
}

void __e_acsl_globals_clean(void)
{
  __e_acsl_delete_block((void *)(& P));
  __e_acsl_delete_block((void *)(& G));
  return;
}

int main(void)
{
  int __retres;
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  __e_acsl_globals_init();
  P = & G;
  int *q = P;
  __e_acsl_store_block((void *)(& q),8UL);
  __e_acsl_full_init((void *)(& q));
  {
    int __gen_e_acsl_valid_read;
    int __gen_e_acsl_valid;
    __e_acsl_initialize((void *)P,sizeof(int));
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_valid_read = __e_acsl_valid_read((void *)P,sizeof(int),
                                                  (void *)P,(void *)(& P));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"P",(void *)P);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,"sizeof(int)",
                                   0,sizeof(int));
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "RTE";
    __gen_e_acsl_assert_data.pred_txt = "\\valid_read(P)";
    __gen_e_acsl_assert_data.file = "ghost.i";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 13;
    __gen_e_acsl_assert_data.name = "mem_access";
    __e_acsl_assert(__gen_e_acsl_valid_read,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __gen_e_acsl_valid = __e_acsl_valid((void *)P,sizeof(int),(void *)P,
                                        (void *)(& P));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"P",(void *)P);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                   "sizeof(int)",0,sizeof(int));
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "RTE";
    __gen_e_acsl_assert_data_2.pred_txt = "\\valid(P)";
    __gen_e_acsl_assert_data_2.file = "ghost.i";
    __gen_e_acsl_assert_data_2.fct = "main";
    __gen_e_acsl_assert_data_2.line = 13;
    __gen_e_acsl_assert_data_2.name = "mem_access";
    __e_acsl_assert(__gen_e_acsl_valid,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
  }
  (*P) ++;
  {
    int __gen_e_acsl_initialized;
    int __gen_e_acsl_and;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
      {.values = (void *)0};
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
      {.values = (void *)0};
    __gen_e_acsl_initialized = __e_acsl_initialized((void *)(& q),
                                                    sizeof(int *));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"&q",
                                 (void *)(& q));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,
                                   "sizeof(int *)",0,sizeof(int *));
    if (__gen_e_acsl_initialized) {
      int __gen_e_acsl_valid_read_2;
      __gen_e_acsl_valid_read_2 = __e_acsl_valid_read((void *)q,sizeof(int),
                                                      (void *)q,
                                                      (void *)(& q));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"q",
                                   (void *)q);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,
                                     "sizeof(int)",0,sizeof(int));
      __gen_e_acsl_and = __gen_e_acsl_valid_read_2;
    }
    else __gen_e_acsl_and = 0;
    __gen_e_acsl_assert_data_4.blocking = 1;
    __gen_e_acsl_assert_data_4.kind = "RTE";
    __gen_e_acsl_assert_data_4.pred_txt = "\\valid_read(q)";
    __gen_e_acsl_assert_data_4.file = "ghost.i";
    __gen_e_acsl_assert_data_4.fct = "main";
    __gen_e_acsl_assert_data_4.line = 14;
    __gen_e_acsl_assert_data_4.name = "mem_access";
    __e_acsl_assert(__gen_e_acsl_and,& __gen_e_acsl_assert_data_4);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"*q",0,*q);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"G",0,G);
    __gen_e_acsl_assert_data_3.blocking = 1;
    __gen_e_acsl_assert_data_3.kind = "Assertion";
    __gen_e_acsl_assert_data_3.pred_txt = "*q == G";
    __gen_e_acsl_assert_data_3.file = "ghost.i";
    __gen_e_acsl_assert_data_3.fct = "main";
    __gen_e_acsl_assert_data_3.line = 14;
    __e_acsl_assert(*q == G,& __gen_e_acsl_assert_data_3);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
  }
  /*@ assert *q == G; */ ;
  int x = 1;
  if (x) x ++;
  else {
    G ++;
    G ++;
  }
  __retres = 0;
  __e_acsl_delete_block((void *)(& q));
  __e_acsl_globals_clean();
  __e_acsl_memory_clean();
  return __retres;
}


