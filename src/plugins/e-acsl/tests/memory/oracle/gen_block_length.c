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

int A[4] = {1, 2, 3, 4};
int *PA;
void __e_acsl_globals_init(void)
{
  static char __e_acsl_already_run = 0;
  if (! __e_acsl_already_run) {
    __e_acsl_already_run = 1;
    __e_acsl_store_block((void *)(& PA),8UL);
    __e_acsl_full_init((void *)(& PA));
    __e_acsl_store_block((void *)(A),16UL);
    __e_acsl_full_init((void *)(& A));
  }
  return;
}

void __e_acsl_globals_clean(void)
{
  __e_acsl_delete_block((void *)(& PA));
  __e_acsl_delete_block((void *)(A));
  return;
}

int main(void)
{
  int __retres;
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  __e_acsl_globals_init();
  PA = (int *)(& A);
  {
    unsigned long __gen_e_acsl_block_length;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_block_length = __e_acsl_block_length((void *)(A));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"(int *)A",
                                 (void *)(A));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                   "\\block_length((int *)A)",0,
                                   __gen_e_acsl_block_length);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"sizeof(A)",0,16);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "\\block_length((int *)A) == sizeof(A)";
    __gen_e_acsl_assert_data.file = "block_length.c";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 13;
    __e_acsl_assert(__gen_e_acsl_block_length == 16UL,
                    & __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert \block_length((int *)A) == sizeof(A); */ ;
  {
    unsigned long __gen_e_acsl_block_length_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __gen_e_acsl_block_length_2 = __e_acsl_block_length((void *)(& A[3]));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"&A[3]",
                                 (void *)(& A[3]));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                   "\\block_length(&A[3])",0,
                                   __gen_e_acsl_block_length_2);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"sizeof(A)",0,
                                 16);
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Assertion";
    __gen_e_acsl_assert_data_2.pred_txt = "\\block_length(&A[3]) == sizeof(A)";
    __gen_e_acsl_assert_data_2.file = "block_length.c";
    __gen_e_acsl_assert_data_2.fct = "main";
    __gen_e_acsl_assert_data_2.line = 14;
    __e_acsl_assert(__gen_e_acsl_block_length_2 == 16UL,
                    & __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
  }
  /*@ assert \block_length(&A[3]) == sizeof(A); */ ;
  {
    unsigned long __gen_e_acsl_block_length_3;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
      {.values = (void *)0};
    __gen_e_acsl_block_length_3 = __e_acsl_block_length((void *)PA);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,"PA",
                                 (void *)PA);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_3,
                                   "\\block_length(PA)",0,
                                   __gen_e_acsl_block_length_3);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"sizeof(A)",0,
                                 16);
    __gen_e_acsl_assert_data_3.blocking = 1;
    __gen_e_acsl_assert_data_3.kind = "Assertion";
    __gen_e_acsl_assert_data_3.pred_txt = "\\block_length(PA) == sizeof(A)";
    __gen_e_acsl_assert_data_3.file = "block_length.c";
    __gen_e_acsl_assert_data_3.fct = "main";
    __gen_e_acsl_assert_data_3.line = 15;
    __e_acsl_assert(__gen_e_acsl_block_length_3 == 16UL,
                    & __gen_e_acsl_assert_data_3);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
  }
  /*@ assert \block_length(PA) == sizeof(A); */ ;
  PA ++;
  {
    unsigned long __gen_e_acsl_block_length_4;
    unsigned long __gen_e_acsl_block_length_5;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
      {.values = (void *)0};
    __gen_e_acsl_block_length_4 = __e_acsl_block_length((void *)(PA + 1));
    __gen_e_acsl_block_length_5 = __e_acsl_block_length((void *)(& A[1]));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"PA",
                                 (void *)PA);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,
                                   "\\block_length(PA + 1)",0,
                                   __gen_e_acsl_block_length_4);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"&A[1]",
                                 (void *)(& A[1]));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,
                                   "\\block_length(&A[1])",0,
                                   __gen_e_acsl_block_length_5);
    __gen_e_acsl_assert_data_4.blocking = 1;
    __gen_e_acsl_assert_data_4.kind = "Assertion";
    __gen_e_acsl_assert_data_4.pred_txt = "\\block_length(PA + 1) == \\block_length(&A[1])";
    __gen_e_acsl_assert_data_4.file = "block_length.c";
    __gen_e_acsl_assert_data_4.fct = "main";
    __gen_e_acsl_assert_data_4.line = 17;
    __e_acsl_assert(__gen_e_acsl_block_length_4 == __gen_e_acsl_block_length_5,
                    & __gen_e_acsl_assert_data_4);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
  }
  /*@ assert \block_length(PA + 1) == \block_length(&A[1]); */ ;
  int a[4] = {1, 2, 3, 4};
  __e_acsl_store_block((void *)(a),16UL);
  __e_acsl_full_init((void *)(& a));
  int *pa = (int *)(& a);
  __e_acsl_store_block((void *)(& pa),8UL);
  __e_acsl_full_init((void *)(& pa));
  {
    unsigned long __gen_e_acsl_block_length_6;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
      {.values = (void *)0};
    __gen_e_acsl_block_length_6 = __e_acsl_block_length((void *)(a));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_5,"(int *)a",
                                 (void *)(a));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_5,
                                   "\\block_length((int *)a)",0,
                                   __gen_e_acsl_block_length_6);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"sizeof(a)",0,
                                 16);
    __gen_e_acsl_assert_data_5.blocking = 1;
    __gen_e_acsl_assert_data_5.kind = "Assertion";
    __gen_e_acsl_assert_data_5.pred_txt = "\\block_length((int *)a) == sizeof(a)";
    __gen_e_acsl_assert_data_5.file = "block_length.c";
    __gen_e_acsl_assert_data_5.fct = "main";
    __gen_e_acsl_assert_data_5.line = 22;
    __e_acsl_assert(__gen_e_acsl_block_length_6 == 16UL,
                    & __gen_e_acsl_assert_data_5);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
  }
  /*@ assert \block_length((int *)a) == sizeof(a); */ ;
  {
    unsigned long __gen_e_acsl_block_length_7;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_6 =
      {.values = (void *)0};
    __gen_e_acsl_block_length_7 = __e_acsl_block_length((void *)(& a[3]));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_6,"&a[3]",
                                 (void *)(& a[3]));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_6,
                                   "\\block_length(&a[3])",0,
                                   __gen_e_acsl_block_length_7);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_6,"sizeof(a)",0,
                                 16);
    __gen_e_acsl_assert_data_6.blocking = 1;
    __gen_e_acsl_assert_data_6.kind = "Assertion";
    __gen_e_acsl_assert_data_6.pred_txt = "\\block_length(&a[3]) == sizeof(a)";
    __gen_e_acsl_assert_data_6.file = "block_length.c";
    __gen_e_acsl_assert_data_6.fct = "main";
    __gen_e_acsl_assert_data_6.line = 23;
    __e_acsl_assert(__gen_e_acsl_block_length_7 == 16UL,
                    & __gen_e_acsl_assert_data_6);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_6);
  }
  /*@ assert \block_length(&a[3]) == sizeof(a); */ ;
  {
    unsigned long __gen_e_acsl_block_length_8;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_7 =
      {.values = (void *)0};
    __gen_e_acsl_block_length_8 = __e_acsl_block_length((void *)pa);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_7,"pa",
                                 (void *)pa);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_7,
                                   "\\block_length(pa)",0,
                                   __gen_e_acsl_block_length_8);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_7,"sizeof(a)",0,
                                 16);
    __gen_e_acsl_assert_data_7.blocking = 1;
    __gen_e_acsl_assert_data_7.kind = "Assertion";
    __gen_e_acsl_assert_data_7.pred_txt = "\\block_length(pa) == sizeof(a)";
    __gen_e_acsl_assert_data_7.file = "block_length.c";
    __gen_e_acsl_assert_data_7.fct = "main";
    __gen_e_acsl_assert_data_7.line = 24;
    __e_acsl_assert(__gen_e_acsl_block_length_8 == 16UL,
                    & __gen_e_acsl_assert_data_7);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_7);
  }
  /*@ assert \block_length(pa) == sizeof(a); */ ;
  __e_acsl_full_init((void *)(& pa));
  pa ++;
  {
    unsigned long __gen_e_acsl_block_length_9;
    unsigned long __gen_e_acsl_block_length_10;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_8 =
      {.values = (void *)0};
    __gen_e_acsl_block_length_9 = __e_acsl_block_length((void *)(pa + 1));
    __gen_e_acsl_block_length_10 = __e_acsl_block_length((void *)(& a[1]));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_8,"pa",
                                 (void *)pa);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_8,
                                   "\\block_length(pa + 1)",0,
                                   __gen_e_acsl_block_length_9);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_8,"&a[1]",
                                 (void *)(& a[1]));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_8,
                                   "\\block_length(&a[1])",0,
                                   __gen_e_acsl_block_length_10);
    __gen_e_acsl_assert_data_8.blocking = 1;
    __gen_e_acsl_assert_data_8.kind = "Assertion";
    __gen_e_acsl_assert_data_8.pred_txt = "\\block_length(pa + 1) == \\block_length(&a[1])";
    __gen_e_acsl_assert_data_8.file = "block_length.c";
    __gen_e_acsl_assert_data_8.fct = "main";
    __gen_e_acsl_assert_data_8.line = 26;
    __e_acsl_assert(__gen_e_acsl_block_length_9 == __gen_e_acsl_block_length_10,
                    & __gen_e_acsl_assert_data_8);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_8);
  }
  /*@ assert \block_length(pa + 1) == \block_length(&a[1]); */ ;
  long l = (long)4;
  __e_acsl_store_block((void *)(& l),8UL);
  __e_acsl_full_init((void *)(& l));
  char *pl = (char *)(& l);
  __e_acsl_store_block((void *)(& pl),8UL);
  __e_acsl_full_init((void *)(& pl));
  {
    unsigned long __gen_e_acsl_block_length_11;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_9 =
      {.values = (void *)0};
    __gen_e_acsl_block_length_11 = __e_acsl_block_length((void *)(& l));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_9,"&l",
                                 (void *)(& l));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_9,
                                   "\\block_length(&l)",0,
                                   __gen_e_acsl_block_length_11);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_9,"sizeof(long)",
                                 0,8);
    __gen_e_acsl_assert_data_9.blocking = 1;
    __gen_e_acsl_assert_data_9.kind = "Assertion";
    __gen_e_acsl_assert_data_9.pred_txt = "\\block_length(&l) == sizeof(long)";
    __gen_e_acsl_assert_data_9.file = "block_length.c";
    __gen_e_acsl_assert_data_9.fct = "main";
    __gen_e_acsl_assert_data_9.line = 32;
    __e_acsl_assert(__gen_e_acsl_block_length_11 == 8UL,
                    & __gen_e_acsl_assert_data_9);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_9);
  }
  /*@ assert \block_length(&l) == sizeof(long); */ ;
  {
    unsigned long __gen_e_acsl_block_length_12;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_10 =
      {.values = (void *)0};
    __gen_e_acsl_block_length_12 = __e_acsl_block_length((void *)pl);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_10,"pl",
                                 (void *)pl);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_10,
                                   "\\block_length(pl)",0,
                                   __gen_e_acsl_block_length_12);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_10,
                                 "sizeof(long)",0,8);
    __gen_e_acsl_assert_data_10.blocking = 1;
    __gen_e_acsl_assert_data_10.kind = "Assertion";
    __gen_e_acsl_assert_data_10.pred_txt = "\\block_length(pl) == sizeof(long)";
    __gen_e_acsl_assert_data_10.file = "block_length.c";
    __gen_e_acsl_assert_data_10.fct = "main";
    __gen_e_acsl_assert_data_10.line = 33;
    __e_acsl_assert(__gen_e_acsl_block_length_12 == 8UL,
                    & __gen_e_acsl_assert_data_10);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_10);
  }
  /*@ assert \block_length(pl) == sizeof(long); */ ;
  {
    unsigned long __gen_e_acsl_block_length_13;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_11 =
      {.values = (void *)0};
    __gen_e_acsl_block_length_13 = __e_acsl_block_length((void *)(pl + 7));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_11,"pl",
                                 (void *)pl);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_11,
                                   "\\block_length(pl + 7)",0,
                                   __gen_e_acsl_block_length_13);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_11,
                                 "sizeof(long)",0,8);
    __gen_e_acsl_assert_data_11.blocking = 1;
    __gen_e_acsl_assert_data_11.kind = "Assertion";
    __gen_e_acsl_assert_data_11.pred_txt = "\\block_length(pl + 7) == sizeof(long)";
    __gen_e_acsl_assert_data_11.file = "block_length.c";
    __gen_e_acsl_assert_data_11.fct = "main";
    __gen_e_acsl_assert_data_11.line = 34;
    __e_acsl_assert(__gen_e_acsl_block_length_13 == 8UL,
                    & __gen_e_acsl_assert_data_11);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_11);
  }
  /*@ assert \block_length(pl + 7) == sizeof(long); */ ;
  int *pi = (int *)(& l);
  __e_acsl_store_block((void *)(& pi),8UL);
  __e_acsl_full_init((void *)(& pi));
  {
    unsigned long __gen_e_acsl_block_length_14;
    unsigned long __gen_e_acsl_block_length_15;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_12 =
      {.values = (void *)0};
    __gen_e_acsl_block_length_14 = __e_acsl_block_length((void *)pi);
    __gen_e_acsl_block_length_15 = __e_acsl_block_length((void *)(& l));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_12,"pi",
                                 (void *)pi);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_12,
                                   "\\block_length(pi)",0,
                                   __gen_e_acsl_block_length_14);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_12,"&l",
                                 (void *)(& l));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_12,
                                   "\\block_length(&l)",0,
                                   __gen_e_acsl_block_length_15);
    __gen_e_acsl_assert_data_12.blocking = 1;
    __gen_e_acsl_assert_data_12.kind = "Assertion";
    __gen_e_acsl_assert_data_12.pred_txt = "\\block_length(pi) == \\block_length(&l)";
    __gen_e_acsl_assert_data_12.file = "block_length.c";
    __gen_e_acsl_assert_data_12.fct = "main";
    __gen_e_acsl_assert_data_12.line = 36;
    __e_acsl_assert(__gen_e_acsl_block_length_14 == __gen_e_acsl_block_length_15,
                    & __gen_e_acsl_assert_data_12);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_12);
  }
  /*@ assert \block_length(pi) == \block_length(&l); */ ;
  __e_acsl_full_init((void *)(& pi));
  pi ++;
  {
    unsigned long __gen_e_acsl_block_length_16;
    unsigned long __gen_e_acsl_block_length_17;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_13 =
      {.values = (void *)0};
    __gen_e_acsl_block_length_16 = __e_acsl_block_length((void *)pi);
    __gen_e_acsl_block_length_17 = __e_acsl_block_length((void *)(& l));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_13,"pi",
                                 (void *)pi);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_13,
                                   "\\block_length(pi)",0,
                                   __gen_e_acsl_block_length_16);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_13,"&l",
                                 (void *)(& l));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_13,
                                   "\\block_length(&l)",0,
                                   __gen_e_acsl_block_length_17);
    __gen_e_acsl_assert_data_13.blocking = 1;
    __gen_e_acsl_assert_data_13.kind = "Assertion";
    __gen_e_acsl_assert_data_13.pred_txt = "\\block_length(pi) == \\block_length(&l)";
    __gen_e_acsl_assert_data_13.file = "block_length.c";
    __gen_e_acsl_assert_data_13.fct = "main";
    __gen_e_acsl_assert_data_13.line = 38;
    __e_acsl_assert(__gen_e_acsl_block_length_16 == __gen_e_acsl_block_length_17,
                    & __gen_e_acsl_assert_data_13);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_13);
  }
  /*@ assert \block_length(pi) == \block_length(&l); */ ;
  size_t size = (size_t)12;
  char *p = malloc(size);
  __e_acsl_store_block((void *)(& p),8UL);
  __e_acsl_full_init((void *)(& p));
  {
    unsigned long __gen_e_acsl_block_length_18;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_14 =
      {.values = (void *)0};
    __gen_e_acsl_block_length_18 = __e_acsl_block_length((void *)p);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_14,"p",(void *)p);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_14,
                                   "\\block_length(p)",0,
                                   __gen_e_acsl_block_length_18);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_14,"size",0,
                                   size);
    __gen_e_acsl_assert_data_14.blocking = 1;
    __gen_e_acsl_assert_data_14.kind = "Assertion";
    __gen_e_acsl_assert_data_14.pred_txt = "\\block_length(p) == size";
    __gen_e_acsl_assert_data_14.file = "block_length.c";
    __gen_e_acsl_assert_data_14.fct = "main";
    __gen_e_acsl_assert_data_14.line = 43;
    __e_acsl_assert(__gen_e_acsl_block_length_18 == size,
                    & __gen_e_acsl_assert_data_14);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_14);
  }
  /*@ assert \block_length(p) == size; */ ;
  {
    unsigned long __gen_e_acsl_block_length_19;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_15 =
      {.values = (void *)0};
    __gen_e_acsl_block_length_19 = __e_acsl_block_length((void *)(p + 11));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_15,"p",(void *)p);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_15,
                                   "\\block_length(p + 11)",0,
                                   __gen_e_acsl_block_length_19);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_15,"size",0,
                                   size);
    __gen_e_acsl_assert_data_15.blocking = 1;
    __gen_e_acsl_assert_data_15.kind = "Assertion";
    __gen_e_acsl_assert_data_15.pred_txt = "\\block_length(p + 11) == size";
    __gen_e_acsl_assert_data_15.file = "block_length.c";
    __gen_e_acsl_assert_data_15.fct = "main";
    __gen_e_acsl_assert_data_15.line = 44;
    __e_acsl_assert(__gen_e_acsl_block_length_19 == size,
                    & __gen_e_acsl_assert_data_15);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_15);
  }
  /*@ assert \block_length(p + 11) == size; */ ;
  __e_acsl_full_init((void *)(& p));
  p += 5;
  {
    unsigned long __gen_e_acsl_block_length_20;
    unsigned long __gen_e_acsl_block_length_21;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_16 =
      {.values = (void *)0};
    __gen_e_acsl_block_length_20 = __e_acsl_block_length((void *)(p + 5));
    __gen_e_acsl_block_length_21 = __e_acsl_block_length((void *)(p - 5));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_16,"p",(void *)p);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_16,
                                   "\\block_length(p + 5)",0,
                                   __gen_e_acsl_block_length_20);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_16,"p",(void *)p);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_16,
                                   "\\block_length(p - 5)",0,
                                   __gen_e_acsl_block_length_21);
    __gen_e_acsl_assert_data_16.blocking = 1;
    __gen_e_acsl_assert_data_16.kind = "Assertion";
    __gen_e_acsl_assert_data_16.pred_txt = "\\block_length(p + 5) == \\block_length(p - 5)";
    __gen_e_acsl_assert_data_16.file = "block_length.c";
    __gen_e_acsl_assert_data_16.fct = "main";
    __gen_e_acsl_assert_data_16.line = 46;
    __e_acsl_assert(__gen_e_acsl_block_length_20 == __gen_e_acsl_block_length_21,
                    & __gen_e_acsl_assert_data_16);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_16);
  }
  /*@ assert \block_length(p + 5) == \block_length(p - 5); */ ;
  size = (unsigned long)30 * sizeof(long);
  long *q = malloc(size);
  __e_acsl_store_block((void *)(& q),8UL);
  __e_acsl_full_init((void *)(& q));
  {
    unsigned long __gen_e_acsl_block_length_22;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_17 =
      {.values = (void *)0};
    __gen_e_acsl_block_length_22 = __e_acsl_block_length((void *)q);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_17,"q",(void *)q);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_17,
                                   "\\block_length(q)",0,
                                   __gen_e_acsl_block_length_22);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_17,"size",0,
                                   size);
    __gen_e_acsl_assert_data_17.blocking = 1;
    __gen_e_acsl_assert_data_17.kind = "Assertion";
    __gen_e_acsl_assert_data_17.pred_txt = "\\block_length(q) == size";
    __gen_e_acsl_assert_data_17.file = "block_length.c";
    __gen_e_acsl_assert_data_17.fct = "main";
    __gen_e_acsl_assert_data_17.line = 52;
    __e_acsl_assert(__gen_e_acsl_block_length_22 == size,
                    & __gen_e_acsl_assert_data_17);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_17);
  }
  /*@ assert \block_length(q) == size; */ ;
  __e_acsl_full_init((void *)(& q));
  q += 4;
  {
    unsigned long __gen_e_acsl_block_length_23;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_18 =
      {.values = (void *)0};
    __gen_e_acsl_block_length_23 = __e_acsl_block_length((void *)q);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_18,"q",(void *)q);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_18,
                                   "\\block_length(q)",0,
                                   __gen_e_acsl_block_length_23);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_18,"size",0,
                                   size);
    __gen_e_acsl_assert_data_18.blocking = 1;
    __gen_e_acsl_assert_data_18.kind = "Assertion";
    __gen_e_acsl_assert_data_18.pred_txt = "\\block_length(q) == size";
    __gen_e_acsl_assert_data_18.file = "block_length.c";
    __gen_e_acsl_assert_data_18.fct = "main";
    __gen_e_acsl_assert_data_18.line = 54;
    __e_acsl_assert(__gen_e_acsl_block_length_23 == size,
                    & __gen_e_acsl_assert_data_18);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_18);
  }
  /*@ assert \block_length(q) == size; */ ;
  __retres = 0;
  __e_acsl_delete_block((void *)(& q));
  __e_acsl_delete_block((void *)(& p));
  __e_acsl_delete_block((void *)(& pi));
  __e_acsl_delete_block((void *)(& pl));
  __e_acsl_delete_block((void *)(& l));
  __e_acsl_delete_block((void *)(& pa));
  __e_acsl_delete_block((void *)(a));
  __e_acsl_globals_clean();
  __e_acsl_memory_clean();
  return __retres;
}


