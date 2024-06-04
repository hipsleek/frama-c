/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "time.h"
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

int find_last_of(int const *a, int len, int value)
{
  int __retres;
  __e_acsl_store_block((void *)(& a),8UL);
  size_t o = (size_t)len;
  {
    int __gen_e_acsl_forall;
    __e_acsl_mpz_t __gen_e_acsl_i;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_forall = 1;
    __gmpz_init(__gen_e_acsl_i);
    {
      __e_acsl_mpz_t __gen_e_acsl_len;
      __gmpz_init_set_si(__gen_e_acsl_len,(long)len);
      __gmpz_set(__gen_e_acsl_i,
                 (__e_acsl_mpz_struct const *)(__gen_e_acsl_len));
      __gmpz_clear(__gen_e_acsl_len);
    }
    while (1) {
      {
        __e_acsl_mpz_t __gen_e_acsl_o;
        int __gen_e_acsl_lt;
        __gmpz_init_set_ui(__gen_e_acsl_o,o);
        __gen_e_acsl_lt = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_i),
                                     (__e_acsl_mpz_struct const *)(__gen_e_acsl_o));
        if (__gen_e_acsl_lt < 0) ; else break;
        __gmpz_clear(__gen_e_acsl_o);
      }
      {
        long __gen_e_acsl_i_2;
        int __gen_e_acsl_valid_read;
        __gen_e_acsl_i_2 = __gmpz_get_si((__e_acsl_mpz_struct const *)(__gen_e_acsl_i));
        __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
          {.values = (void *)0};
        __gen_e_acsl_valid_read = __e_acsl_valid_read((void *)(a + __gen_e_acsl_i_2),
                                                      sizeof(int const),
                                                      (void *)a,
                                                      (void *)(& a));
        __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"a",
                                     (void *)a);
        __e_acsl_assert_register_long(& __gen_e_acsl_assert_data_2,
                                      "__gen_e_acsl_i_2",0,__gen_e_acsl_i_2);
        __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                       "sizeof(int const)",0,
                                       sizeof(int const));
        __gen_e_acsl_assert_data_2.blocking = 1;
        __gen_e_acsl_assert_data_2.kind = "RTE";
        __gen_e_acsl_assert_data_2.pred_txt = "\\valid_read(a + __gen_e_acsl_i_2)";
        __gen_e_acsl_assert_data_2.file = "issue-framac-1119.c";
        __gen_e_acsl_assert_data_2.fct = "find_last_of";
        __gen_e_acsl_assert_data_2.line = 10;
        __gen_e_acsl_assert_data_2.name = "mem_access";
        __e_acsl_assert(__gen_e_acsl_valid_read,& __gen_e_acsl_assert_data_2);
        __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
        /*@ assert Eva: mem_access: \valid_read(a + __gen_e_acsl_i_2); */
        if (*(a + __gen_e_acsl_i_2) != value) ;
        else {
          __gen_e_acsl_forall = 0;
          goto e_acsl_end_loop1;
        }
      }
      {
        __e_acsl_mpz_t __gen_e_acsl_;
        __e_acsl_mpz_t __gen_e_acsl_add;
        __gmpz_init_set_str(__gen_e_acsl_,"1",10);
        __gmpz_init(__gen_e_acsl_add);
        __gmpz_add(__gen_e_acsl_add,
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_i),
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_));
        __gmpz_set(__gen_e_acsl_i,
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_add));
        __gmpz_clear(__gen_e_acsl_);
        __gmpz_clear(__gen_e_acsl_add);
      }
    }
    e_acsl_end_loop1: ;
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,
                                 "\\forall integer i; len <= i < o ==> *(a + i) != value",
                                 0,__gen_e_acsl_forall);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Invariant";
    __gen_e_acsl_assert_data.pred_txt = "\\forall integer i; len <= i < o ==> *(a + i) != value";
    __gen_e_acsl_assert_data.file = "issue-framac-1119.c";
    __gen_e_acsl_assert_data.fct = "find_last_of";
    __gen_e_acsl_assert_data.line = 10;
    __e_acsl_assert(__gen_e_acsl_forall,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    __gmpz_clear(__gen_e_acsl_i);
  }
  /*@ loop invariant \forall integer i; len <= i < o ==> *(a + i) != value;
  */
  while (len) {
    int __gen_e_acsl_forall_2;
    __e_acsl_mpz_t __gen_e_acsl_i_3;
    len --;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
      {.values = (void *)0};
    __gen_e_acsl_forall_2 = 1;
    __gmpz_init(__gen_e_acsl_i_3);
    {
      __e_acsl_mpz_t __gen_e_acsl_len_2;
      __gmpz_init_set_si(__gen_e_acsl_len_2,(long)len);
      __gmpz_set(__gen_e_acsl_i_3,
                 (__e_acsl_mpz_struct const *)(__gen_e_acsl_len_2));
      __gmpz_clear(__gen_e_acsl_len_2);
    }
    while (1) {
      {
        __e_acsl_mpz_t __gen_e_acsl_o_2;
        int __gen_e_acsl_lt_2;
        __gmpz_init_set_ui(__gen_e_acsl_o_2,o);
        __gen_e_acsl_lt_2 = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_i_3),
                                       (__e_acsl_mpz_struct const *)(__gen_e_acsl_o_2));
        if (__gen_e_acsl_lt_2 < 0) ; else break;
        __gmpz_clear(__gen_e_acsl_o_2);
      }
      {
        long __gen_e_acsl_i_4;
        int __gen_e_acsl_valid_read_2;
        __gen_e_acsl_i_4 = __gmpz_get_si((__e_acsl_mpz_struct const *)(__gen_e_acsl_i_3));
        __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
          {.values = (void *)0};
        __gen_e_acsl_valid_read_2 = __e_acsl_valid_read((void *)(a + __gen_e_acsl_i_4),
                                                        sizeof(int const),
                                                        (void *)a,
                                                        (void *)(& a));
        __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"a",
                                     (void *)a);
        __e_acsl_assert_register_long(& __gen_e_acsl_assert_data_4,
                                      "__gen_e_acsl_i_4",0,__gen_e_acsl_i_4);
        __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,
                                       "sizeof(int const)",0,
                                       sizeof(int const));
        __gen_e_acsl_assert_data_4.blocking = 1;
        __gen_e_acsl_assert_data_4.kind = "RTE";
        __gen_e_acsl_assert_data_4.pred_txt = "\\valid_read(a + __gen_e_acsl_i_4)";
        __gen_e_acsl_assert_data_4.file = "issue-framac-1119.c";
        __gen_e_acsl_assert_data_4.fct = "find_last_of";
        __gen_e_acsl_assert_data_4.line = 10;
        __gen_e_acsl_assert_data_4.name = "mem_access";
        __e_acsl_assert(__gen_e_acsl_valid_read_2,
                        & __gen_e_acsl_assert_data_4);
        __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
        /*@ assert Eva: mem_access: \valid_read(a + __gen_e_acsl_i_4); */
        if (*(a + __gen_e_acsl_i_4) != value) ;
        else {
          __gen_e_acsl_forall_2 = 0;
          goto e_acsl_end_loop2;
        }
      }
      {
        __e_acsl_mpz_t __gen_e_acsl__2;
        __e_acsl_mpz_t __gen_e_acsl_add_2;
        __gmpz_init_set_str(__gen_e_acsl__2,"1",10);
        __gmpz_init(__gen_e_acsl_add_2);
        __gmpz_add(__gen_e_acsl_add_2,
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_i_3),
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl__2));
        __gmpz_set(__gen_e_acsl_i_3,
                   (__e_acsl_mpz_struct const *)(__gen_e_acsl_add_2));
        __gmpz_clear(__gen_e_acsl__2);
        __gmpz_clear(__gen_e_acsl_add_2);
      }
    }
    e_acsl_end_loop2: ;
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,
                                 "\\forall integer i; len <= i < o ==> *(a + i) != value",
                                 0,__gen_e_acsl_forall_2);
    __gen_e_acsl_assert_data_3.blocking = 1;
    __gen_e_acsl_assert_data_3.kind = "Invariant";
    __gen_e_acsl_assert_data_3.pred_txt = "\\forall integer i; len <= i < o ==> *(a + i) != value";
    __gen_e_acsl_assert_data_3.file = "issue-framac-1119.c";
    __gen_e_acsl_assert_data_3.fct = "find_last_of";
    __gen_e_acsl_assert_data_3.line = 10;
    __e_acsl_assert(__gen_e_acsl_forall_2,& __gen_e_acsl_assert_data_3);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
    __gmpz_clear(__gen_e_acsl_i_3);
  }
  __retres = 2147483647;
  __e_acsl_delete_block((void *)(& a));
  return __retres;
}

int main(void)
{
  int __retres;
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  int a[1] = {1};
  __e_acsl_store_block((void *)(a),4UL);
  __e_acsl_full_init((void *)(& a));
  find_last_of((int const *)(a),1,0);
  __retres = 0;
  __e_acsl_delete_block((void *)(a));
  __e_acsl_memory_clean();
  return __retres;
}

