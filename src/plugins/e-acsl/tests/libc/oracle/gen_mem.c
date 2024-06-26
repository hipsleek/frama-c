/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "string.h"
#include "time.h"
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

/*@ requires valid_dest: valid_or_empty(dest, n);
    requires valid_src: valid_read_or_empty(src, n);
    requires
      separation:
        \separated((char *)dest + (0 .. n - 1), (char *)src + (0 .. n - 1));
    ensures
      copied_contents:
        memcmp{Post, Pre}((char *)\old(dest), (char *)\old(src), \old(n)) ==
        0;
    ensures result_ptr: \result == \old(dest);
    assigns *((char *)dest + (0 .. n - 1)), \result;
    assigns *((char *)dest + (0 .. n - 1))
      \from *((char *)src + (0 .. n - 1));
    assigns \result \from dest;
 */
void *__gen_e_acsl_memcpy(void * restrict dest, void const * restrict src,
                          size_t n);

/*@ requires valid_dest: valid_or_empty(dest, n);
    requires valid_src: valid_read_or_empty(src, n);
    ensures
      copied_contents:
        memcmp{Post, Pre}((char *)\old(dest), (char *)\old(src), \old(n)) ==
        0;
    ensures result_ptr: \result == \old(dest);
    assigns *((char *)dest + (0 .. n - 1)), \result;
    assigns *((char *)dest + (0 .. n - 1))
      \from *((char *)src + (0 .. n - 1));
    assigns \result \from dest;
 */
void *__gen_e_acsl_memmove(void *dest, void const *src, size_t n);

/*@ requires valid_s: valid_or_empty(s, n);
    ensures acsl_c_equiv: memset((char *)\old(s), \old(c), \old(n)) == \true;
    ensures result_ptr: \result == \old(s);
    assigns *((char *)s + (0 .. n - 1)), \result;
    assigns *((char *)s + (0 .. n - 1)) \from c;
    assigns \result \from s;
 */
void *__gen_e_acsl_memset(void *s, int c, size_t n);

int main(void)
{
  int __retres;
  char a[2];
  int b[5];
  char c[4];
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  __e_acsl_store_block((void *)(c),4UL);
  __e_acsl_store_block((void *)(b),20UL);
  __e_acsl_store_block((void *)(a),2UL);
  __gen_e_acsl_memset((void *)(a),1,(size_t)1);
  {
    int __gen_e_acsl_initialized;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_initialized = __e_acsl_initialized((void *)(a),sizeof(char));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"(char *)a",
                                 (void *)(a));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,"sizeof(char)",
                                   0,sizeof(char));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,
                                 "\\initialized((char *)a)",0,
                                 __gen_e_acsl_initialized);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "\\initialized((char *)a)";
    __gen_e_acsl_assert_data.file = "mem.c";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 10;
    __e_acsl_assert(__gen_e_acsl_initialized,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert \initialized((char *)a); */ ;
  {
    int __gen_e_acsl_initialized_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __gen_e_acsl_initialized_2 = __e_acsl_initialized((void *)(& a[1]),
                                                      sizeof(char));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"&a[1]",
                                 (void *)(& a[1]));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                   "sizeof(char)",0,sizeof(char));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                 "\\initialized(&a[1])",0,
                                 __gen_e_acsl_initialized_2);
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Assertion";
    __gen_e_acsl_assert_data_2.pred_txt = "!\\initialized(&a[1])";
    __gen_e_acsl_assert_data_2.file = "mem.c";
    __gen_e_acsl_assert_data_2.fct = "main";
    __gen_e_acsl_assert_data_2.line = 11;
    __e_acsl_assert(! __gen_e_acsl_initialized_2,
                    & __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
  }
  /*@ assert !\initialized(&a[1]); */ ;
  __gen_e_acsl_memset((void *)(& a[1]),1,(size_t)1);
  {
    int __gen_e_acsl_initialized_3;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
      {.values = (void *)0};
    __gen_e_acsl_initialized_3 = __e_acsl_initialized((void *)(& a[1]),
                                                      sizeof(char));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,"&a[1]",
                                 (void *)(& a[1]));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_3,
                                   "sizeof(char)",0,sizeof(char));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,
                                 "\\initialized(&a[1])",0,
                                 __gen_e_acsl_initialized_3);
    __gen_e_acsl_assert_data_3.blocking = 1;
    __gen_e_acsl_assert_data_3.kind = "Assertion";
    __gen_e_acsl_assert_data_3.pred_txt = "\\initialized(&a[1])";
    __gen_e_acsl_assert_data_3.file = "mem.c";
    __gen_e_acsl_assert_data_3.fct = "main";
    __gen_e_acsl_assert_data_3.line = 13;
    __e_acsl_assert(__gen_e_acsl_initialized_3,& __gen_e_acsl_assert_data_3);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
  }
  /*@ assert \initialized(&a[1]); */ ;
  __gen_e_acsl_memset((void *)(& b[2]),42,(unsigned long)2 * sizeof(b[0]));
  {
    int __gen_e_acsl_initialized_4;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
      {.values = (void *)0};
    __gen_e_acsl_initialized_4 = __e_acsl_initialized((void *)(b),
                                                      sizeof(int));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"(int *)b",
                                 (void *)(b));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,
                                   "sizeof(int)",0,sizeof(int));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,
                                 "\\initialized((int *)b)",0,
                                 __gen_e_acsl_initialized_4);
    __gen_e_acsl_assert_data_4.blocking = 1;
    __gen_e_acsl_assert_data_4.kind = "Assertion";
    __gen_e_acsl_assert_data_4.pred_txt = "!\\initialized((int *)b)";
    __gen_e_acsl_assert_data_4.file = "mem.c";
    __gen_e_acsl_assert_data_4.fct = "main";
    __gen_e_acsl_assert_data_4.line = 16;
    __e_acsl_assert(! __gen_e_acsl_initialized_4,
                    & __gen_e_acsl_assert_data_4);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
  }
  /*@ assert !\initialized((int *)b); */ ;
  {
    int __gen_e_acsl_initialized_5;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
      {.values = (void *)0};
    __gen_e_acsl_initialized_5 = __e_acsl_initialized((void *)(& b[1]),
                                                      sizeof(int));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_5,"&b[1]",
                                 (void *)(& b[1]));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_5,
                                   "sizeof(int)",0,sizeof(int));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,
                                 "\\initialized(&b[1])",0,
                                 __gen_e_acsl_initialized_5);
    __gen_e_acsl_assert_data_5.blocking = 1;
    __gen_e_acsl_assert_data_5.kind = "Assertion";
    __gen_e_acsl_assert_data_5.pred_txt = "!\\initialized(&b[1])";
    __gen_e_acsl_assert_data_5.file = "mem.c";
    __gen_e_acsl_assert_data_5.fct = "main";
    __gen_e_acsl_assert_data_5.line = 17;
    __e_acsl_assert(! __gen_e_acsl_initialized_5,
                    & __gen_e_acsl_assert_data_5);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
  }
  /*@ assert !\initialized(&b[1]); */ ;
  {
    int __gen_e_acsl_size;
    int __gen_e_acsl_if;
    int __gen_e_acsl_initialized_6;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_6 =
      {.values = (void *)0};
    __gen_e_acsl_size = 4 * ((3 - 2) + 1);
    if (__gen_e_acsl_size <= 0) __gen_e_acsl_if = 0;
    else __gen_e_acsl_if = __gen_e_acsl_size;
    __gen_e_acsl_initialized_6 = __e_acsl_initialized((void *)((char *)(b) + 
                                                               4 * 2),
                                                      (size_t)__gen_e_acsl_if);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_6,"(int *)b",
                                 (void *)(b));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_6,"sizeof(int)",
                                 0,4);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_6,"sizeof(int)",
                                 0,4);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_6,"size",0,
                                 __gen_e_acsl_size);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_6,"size",0,
                                 __gen_e_acsl_size);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_6,
                                 "\\initialized(&b[2 .. 3])",0,
                                 __gen_e_acsl_initialized_6);
    __gen_e_acsl_assert_data_6.blocking = 1;
    __gen_e_acsl_assert_data_6.kind = "Assertion";
    __gen_e_acsl_assert_data_6.pred_txt = "\\initialized(&b[2 .. 3])";
    __gen_e_acsl_assert_data_6.file = "mem.c";
    __gen_e_acsl_assert_data_6.fct = "main";
    __gen_e_acsl_assert_data_6.line = 18;
    __e_acsl_assert(__gen_e_acsl_initialized_6,& __gen_e_acsl_assert_data_6);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_6);
  }
  /*@ assert \initialized(&b[2 .. 3]); */ ;
  {
    int __gen_e_acsl_initialized_7;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_7 =
      {.values = (void *)0};
    __gen_e_acsl_initialized_7 = __e_acsl_initialized((void *)(& b[4]),
                                                      sizeof(int));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_7,"&b[4]",
                                 (void *)(& b[4]));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_7,
                                   "sizeof(int)",0,sizeof(int));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_7,
                                 "\\initialized(&b[4])",0,
                                 __gen_e_acsl_initialized_7);
    __gen_e_acsl_assert_data_7.blocking = 1;
    __gen_e_acsl_assert_data_7.kind = "Assertion";
    __gen_e_acsl_assert_data_7.pred_txt = "!\\initialized(&b[4])";
    __gen_e_acsl_assert_data_7.file = "mem.c";
    __gen_e_acsl_assert_data_7.fct = "main";
    __gen_e_acsl_assert_data_7.line = 19;
    __e_acsl_assert(! __gen_e_acsl_initialized_7,
                    & __gen_e_acsl_assert_data_7);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_7);
  }
  /*@ assert !\initialized(&b[4]); */ ;
  __gen_e_acsl_memcpy((void *)(& c[1]),(void const *)(a),(size_t)2);
  {
    int __gen_e_acsl_initialized_8;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_8 =
      {.values = (void *)0};
    __gen_e_acsl_initialized_8 = __e_acsl_initialized((void *)(c),
                                                      sizeof(char));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_8,"(char *)c",
                                 (void *)(c));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_8,
                                   "sizeof(char)",0,sizeof(char));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_8,
                                 "\\initialized((char *)c)",0,
                                 __gen_e_acsl_initialized_8);
    __gen_e_acsl_assert_data_8.blocking = 1;
    __gen_e_acsl_assert_data_8.kind = "Assertion";
    __gen_e_acsl_assert_data_8.pred_txt = "!\\initialized((char *)c)";
    __gen_e_acsl_assert_data_8.file = "mem.c";
    __gen_e_acsl_assert_data_8.fct = "main";
    __gen_e_acsl_assert_data_8.line = 23;
    __e_acsl_assert(! __gen_e_acsl_initialized_8,
                    & __gen_e_acsl_assert_data_8);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_8);
  }
  /*@ assert !\initialized((char *)c); */ ;
  {
    int __gen_e_acsl_size_2;
    int __gen_e_acsl_if_2;
    int __gen_e_acsl_initialized_9;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_9 =
      {.values = (void *)0};
    __gen_e_acsl_size_2 = 1 * ((2 - 1) + 1);
    if (__gen_e_acsl_size_2 <= 0) __gen_e_acsl_if_2 = 0;
    else __gen_e_acsl_if_2 = __gen_e_acsl_size_2;
    __gen_e_acsl_initialized_9 = __e_acsl_initialized((void *)(c + 1 * 1),
                                                      (size_t)__gen_e_acsl_if_2);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_9,"(char *)c",
                                 (void *)(c));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_9,"sizeof(char)",
                                 0,1);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_9,"sizeof(char)",
                                 0,1);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_9,"size",0,
                                 __gen_e_acsl_size_2);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_9,"size",0,
                                 __gen_e_acsl_size_2);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_9,
                                 "\\initialized(&c[1 .. 2])",0,
                                 __gen_e_acsl_initialized_9);
    __gen_e_acsl_assert_data_9.blocking = 1;
    __gen_e_acsl_assert_data_9.kind = "Assertion";
    __gen_e_acsl_assert_data_9.pred_txt = "\\initialized(&c[1 .. 2])";
    __gen_e_acsl_assert_data_9.file = "mem.c";
    __gen_e_acsl_assert_data_9.fct = "main";
    __gen_e_acsl_assert_data_9.line = 24;
    __e_acsl_assert(__gen_e_acsl_initialized_9,& __gen_e_acsl_assert_data_9);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_9);
  }
  /*@ assert \initialized(&c[1 .. 2]); */ ;
  {
    int __gen_e_acsl_initialized_10;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_10 =
      {.values = (void *)0};
    __gen_e_acsl_initialized_10 = __e_acsl_initialized((void *)(& c[3]),
                                                       sizeof(char));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_10,"&c[3]",
                                 (void *)(& c[3]));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_10,
                                   "sizeof(char)",0,sizeof(char));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_10,
                                 "\\initialized(&c[3])",0,
                                 __gen_e_acsl_initialized_10);
    __gen_e_acsl_assert_data_10.blocking = 1;
    __gen_e_acsl_assert_data_10.kind = "Assertion";
    __gen_e_acsl_assert_data_10.pred_txt = "!\\initialized(&c[3])";
    __gen_e_acsl_assert_data_10.file = "mem.c";
    __gen_e_acsl_assert_data_10.fct = "main";
    __gen_e_acsl_assert_data_10.line = 25;
    __e_acsl_assert(! __gen_e_acsl_initialized_10,
                    & __gen_e_acsl_assert_data_10);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_10);
  }
  /*@ assert !\initialized(&c[3]); */ ;
  __gen_e_acsl_memmove((void *)(c),(void const *)(& c[1]),(size_t)2);
  {
    int __gen_e_acsl_size_3;
    int __gen_e_acsl_if_3;
    int __gen_e_acsl_initialized_11;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_11 =
      {.values = (void *)0};
    __gen_e_acsl_size_3 = 1 * ((2 - 0) + 1);
    if (__gen_e_acsl_size_3 <= 0) __gen_e_acsl_if_3 = 0;
    else __gen_e_acsl_if_3 = __gen_e_acsl_size_3;
    __gen_e_acsl_initialized_11 = __e_acsl_initialized((void *)(c + 1 * 0),
                                                       (size_t)__gen_e_acsl_if_3);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_11,"(char *)c",
                                 (void *)(c));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_11,
                                 "sizeof(char)",0,1);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_11,
                                 "sizeof(char)",0,1);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_11,"size",0,
                                 __gen_e_acsl_size_3);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_11,"size",0,
                                 __gen_e_acsl_size_3);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_11,
                                 "\\initialized(&c[0 .. 2])",0,
                                 __gen_e_acsl_initialized_11);
    __gen_e_acsl_assert_data_11.blocking = 1;
    __gen_e_acsl_assert_data_11.kind = "Assertion";
    __gen_e_acsl_assert_data_11.pred_txt = "\\initialized(&c[0 .. 2])";
    __gen_e_acsl_assert_data_11.file = "mem.c";
    __gen_e_acsl_assert_data_11.fct = "main";
    __gen_e_acsl_assert_data_11.line = 28;
    __e_acsl_assert(__gen_e_acsl_initialized_11,
                    & __gen_e_acsl_assert_data_11);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_11);
  }
  /*@ assert \initialized(&c[0 .. 2]); */ ;
  {
    int __gen_e_acsl_initialized_12;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_12 =
      {.values = (void *)0};
    __gen_e_acsl_initialized_12 = __e_acsl_initialized((void *)(& c[3]),
                                                       sizeof(char));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_12,"&c[3]",
                                 (void *)(& c[3]));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_12,
                                   "sizeof(char)",0,sizeof(char));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_12,
                                 "\\initialized(&c[3])",0,
                                 __gen_e_acsl_initialized_12);
    __gen_e_acsl_assert_data_12.blocking = 1;
    __gen_e_acsl_assert_data_12.kind = "Assertion";
    __gen_e_acsl_assert_data_12.pred_txt = "!\\initialized(&c[3])";
    __gen_e_acsl_assert_data_12.file = "mem.c";
    __gen_e_acsl_assert_data_12.fct = "main";
    __gen_e_acsl_assert_data_12.line = 29;
    __e_acsl_assert(! __gen_e_acsl_initialized_12,
                    & __gen_e_acsl_assert_data_12);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_12);
  }
  /*@ assert !\initialized(&c[3]); */ ;
  __retres = 0;
  __e_acsl_delete_block((void *)(c));
  __e_acsl_delete_block((void *)(b));
  __e_acsl_delete_block((void *)(a));
  __e_acsl_memory_clean();
  return __retres;
}

/*@ requires valid_s: valid_or_empty(s, n);
    ensures acsl_c_equiv: memset((char *)\old(s), \old(c), \old(n)) == \true;
    ensures result_ptr: \result == \old(s);
    assigns *((char *)s + (0 .. n - 1)), \result;
    assigns *((char *)s + (0 .. n - 1)) \from c;
    assigns \result \from s;
 */
void *__gen_e_acsl_memset(void *s, int c, size_t n)
{
  void *__gen_e_acsl_at;
  void *__retres;
  __e_acsl_store_block((void *)(& s),8UL);
  __gen_e_acsl_at = s;
  __retres = memset(s,c,n);
  __e_acsl_initialize(s,n);
  {
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
      {.values = (void *)0};
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,"\\result",
                                 __retres);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,"\\old(s)",
                                 __gen_e_acsl_at);
    __gen_e_acsl_assert_data_3.blocking = 1;
    __gen_e_acsl_assert_data_3.kind = "Postcondition";
    __gen_e_acsl_assert_data_3.pred_txt = "\\result == \\old(s)";
    __gen_e_acsl_assert_data_3.file = "FRAMAC_SHARE/libc/string.h";
    __gen_e_acsl_assert_data_3.fct = "memset";
    __gen_e_acsl_assert_data_3.line = 152;
    __gen_e_acsl_assert_data_3.name = "result_ptr";
    __e_acsl_assert(__retres == __gen_e_acsl_at,& __gen_e_acsl_assert_data_3);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
    __e_acsl_delete_block((void *)(& s));
    return __retres;
  }
}

/*@ requires valid_dest: valid_or_empty(dest, n);
    requires valid_src: valid_read_or_empty(src, n);
    ensures
      copied_contents:
        memcmp{Post, Pre}((char *)\old(dest), (char *)\old(src), \old(n)) ==
        0;
    ensures result_ptr: \result == \old(dest);
    assigns *((char *)dest + (0 .. n - 1)), \result;
    assigns *((char *)dest + (0 .. n - 1))
      \from *((char *)src + (0 .. n - 1));
    assigns \result \from dest;
 */
void *__gen_e_acsl_memmove(void *dest, void const *src, size_t n)
{
  void *__gen_e_acsl_at;
  void *__retres;
  __e_acsl_store_block((void *)(& src),8UL);
  __e_acsl_store_block((void *)(& dest),8UL);
  __gen_e_acsl_at = dest;
  __retres = memmove(dest,src,n);
  __e_acsl_initialize(dest,n);
  {
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
      {.values = (void *)0};
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"\\result",
                                 __retres);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"\\old(dest)",
                                 __gen_e_acsl_at);
    __gen_e_acsl_assert_data_4.blocking = 1;
    __gen_e_acsl_assert_data_4.kind = "Postcondition";
    __gen_e_acsl_assert_data_4.pred_txt = "\\result == \\old(dest)";
    __gen_e_acsl_assert_data_4.file = "FRAMAC_SHARE/libc/string.h";
    __gen_e_acsl_assert_data_4.fct = "memmove";
    __gen_e_acsl_assert_data_4.line = 142;
    __gen_e_acsl_assert_data_4.name = "result_ptr";
    __e_acsl_assert(__retres == __gen_e_acsl_at,& __gen_e_acsl_assert_data_4);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
    __e_acsl_delete_block((void *)(& src));
    __e_acsl_delete_block((void *)(& dest));
    return __retres;
  }
}

/*@ requires valid_dest: valid_or_empty(dest, n);
    requires valid_src: valid_read_or_empty(src, n);
    requires
      separation:
        \separated((char *)dest + (0 .. n - 1), (char *)src + (0 .. n - 1));
    ensures
      copied_contents:
        memcmp{Post, Pre}((char *)\old(dest), (char *)\old(src), \old(n)) ==
        0;
    ensures result_ptr: \result == \old(dest);
    assigns *((char *)dest + (0 .. n - 1)), \result;
    assigns *((char *)dest + (0 .. n - 1))
      \from *((char *)src + (0 .. n - 1));
    assigns \result \from dest;
 */
void *__gen_e_acsl_memcpy(void * restrict dest, void const * restrict src,
                          size_t n)
{
  void *__gen_e_acsl_at;
  void *__retres;
  {
    unsigned long __gen_e_acsl_size;
    __e_acsl_mpz_t __gen_e_acsl_n;
    __e_acsl_mpz_t __gen_e_acsl_;
    __e_acsl_mpz_t __gen_e_acsl_sub;
    __e_acsl_mpz_t __gen_e_acsl__2;
    __e_acsl_mpz_t __gen_e_acsl_sub_2;
    __e_acsl_mpz_t __gen_e_acsl_add;
    unsigned long __gen_e_acsl__3;
    unsigned long __gen_e_acsl_if;
    int __gen_e_acsl_valid_read;
    unsigned long __gen_e_acsl_size_2;
    unsigned long __gen_e_acsl__4;
    unsigned long __gen_e_acsl_if_2;
    int __gen_e_acsl_valid_read_2;
    unsigned long __gen_e_acsl_size_3;
    unsigned long __gen_e_acsl__5;
    unsigned long __gen_e_acsl_if_3;
    unsigned long __gen_e_acsl_size_4;
    unsigned long __gen_e_acsl__6;
    unsigned long __gen_e_acsl_if_4;
    int __gen_e_acsl_separated;
    __e_acsl_store_block((void *)(& src),8UL);
    __e_acsl_store_block((void *)(& dest),8UL);
    __gen_e_acsl_at = dest;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
      {.values = (void *)0};
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
      {.values = (void *)0};
    __gmpz_init_set_ui(__gen_e_acsl_n,n);
    __gmpz_init_set_si(__gen_e_acsl_,1L);
    __gmpz_init(__gen_e_acsl_sub);
    __gmpz_sub(__gen_e_acsl_sub,
               (__e_acsl_mpz_struct const *)(__gen_e_acsl_n),
               (__e_acsl_mpz_struct const *)(__gen_e_acsl_));
    __gmpz_init_set_si(__gen_e_acsl__2,0L);
    __gmpz_init(__gen_e_acsl_sub_2);
    __gmpz_sub(__gen_e_acsl_sub_2,
               (__e_acsl_mpz_struct const *)(__gen_e_acsl_sub),
               (__e_acsl_mpz_struct const *)(__gen_e_acsl__2));
    __gmpz_init(__gen_e_acsl_add);
    __gmpz_add(__gen_e_acsl_add,
               (__e_acsl_mpz_struct const *)(__gen_e_acsl_sub_2),
               (__e_acsl_mpz_struct const *)(__gen_e_acsl_));
    __gen_e_acsl__3 = __gmpz_get_ui((__e_acsl_mpz_struct const *)(__gen_e_acsl_add));
    __gen_e_acsl_size = 1UL * __gen_e_acsl__3;
    if (__gen_e_acsl_size <= 0UL) __gen_e_acsl_if = 0UL;
    else __gen_e_acsl_if = __gen_e_acsl_size;
    __gen_e_acsl_valid_read = __e_acsl_valid_read((void *)((char *)dest + 
                                                           1 * 0),
                                                  __gen_e_acsl_if,dest,
                                                  (void *)(& dest));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"dest",dest);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,"sizeof(char)",
                                 0,1);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,"sizeof(char)",
                                 0,1);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,"n",0,n);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,"size",0,
                                   __gen_e_acsl_size);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,"size",0,
                                   __gen_e_acsl_size);
    __gen_e_acsl_assert_data_4.blocking = 1;
    __gen_e_acsl_assert_data_4.kind = "RTE";
    __gen_e_acsl_assert_data_4.pred_txt = "\\valid_read((char *)dest + (0 .. n - 1))";
    __gen_e_acsl_assert_data_4.file = "FRAMAC_SHARE/libc/string.h";
    __gen_e_acsl_assert_data_4.fct = "memcpy";
    __gen_e_acsl_assert_data_4.line = 115;
    __gen_e_acsl_assert_data_4.name = "separated_guard";
    __e_acsl_assert(__gen_e_acsl_valid_read,& __gen_e_acsl_assert_data_4);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
      {.values = (void *)0};
    __gen_e_acsl__4 = __gmpz_get_ui((__e_acsl_mpz_struct const *)(__gen_e_acsl_add));
    __gen_e_acsl_size_2 = 1UL * __gen_e_acsl__4;
    if (__gen_e_acsl_size_2 <= 0UL) __gen_e_acsl_if_2 = 0UL;
    else __gen_e_acsl_if_2 = __gen_e_acsl_size_2;
    __gen_e_acsl_valid_read_2 = __e_acsl_valid_read((void *)((char *)src + 
                                                             1 * 0),
                                                    __gen_e_acsl_if_2,
                                                    (void *)src,
                                                    (void *)(& src));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_5,"src",
                                 (void *)src);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"sizeof(char)",
                                 0,1);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"sizeof(char)",
                                 0,1);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_5,"n",0,n);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_5,"size",0,
                                   __gen_e_acsl_size_2);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_5,"size",0,
                                   __gen_e_acsl_size_2);
    __gen_e_acsl_assert_data_5.blocking = 1;
    __gen_e_acsl_assert_data_5.kind = "RTE";
    __gen_e_acsl_assert_data_5.pred_txt = "\\valid_read((char *)src + (0 .. n - 1))";
    __gen_e_acsl_assert_data_5.file = "FRAMAC_SHARE/libc/string.h";
    __gen_e_acsl_assert_data_5.fct = "memcpy";
    __gen_e_acsl_assert_data_5.line = 115;
    __gen_e_acsl_assert_data_5.name = "separated_guard";
    __e_acsl_assert(__gen_e_acsl_valid_read_2,& __gen_e_acsl_assert_data_5);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
    __gen_e_acsl__5 = __gmpz_get_ui((__e_acsl_mpz_struct const *)(__gen_e_acsl_add));
    __gen_e_acsl_size_3 = 1UL * __gen_e_acsl__5;
    if (__gen_e_acsl_size_3 <= 0UL) __gen_e_acsl_if_3 = 0UL;
    else __gen_e_acsl_if_3 = __gen_e_acsl_size_3;
    __gen_e_acsl__6 = __gmpz_get_ui((__e_acsl_mpz_struct const *)(__gen_e_acsl_add));
    __gen_e_acsl_size_4 = 1UL * __gen_e_acsl__6;
    if (__gen_e_acsl_size_4 <= 0UL) __gen_e_acsl_if_4 = 0UL;
    else __gen_e_acsl_if_4 = __gen_e_acsl_size_4;
    __gen_e_acsl_separated = __e_acsl_separated(2UL,(char *)dest + 1 * 0,
                                                __gen_e_acsl_if_3,
                                                (char *)src + 1 * 0,
                                                __gen_e_acsl_if_4);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,"dest",dest);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"sizeof(char)",
                                 0,1);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"sizeof(char)",
                                 0,1);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_3,"n",0,n);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_3,"size",0,
                                   __gen_e_acsl_size_3);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_3,"size",0,
                                   __gen_e_acsl_size_3);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,"src",
                                 (void *)src);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"sizeof(char)",
                                 0,1);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"sizeof(char)",
                                 0,1);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_3,"n",0,n);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_3,"size",0,
                                   __gen_e_acsl_size_4);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_3,"size",0,
                                   __gen_e_acsl_size_4);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,
                                 "separation:\n  \\separated((char *)dest + (0 .. n - 1), (char *)src + (0 .. n - 1))",
                                 0,__gen_e_acsl_separated);
    __gen_e_acsl_assert_data_3.blocking = 1;
    __gen_e_acsl_assert_data_3.kind = "Precondition";
    __gen_e_acsl_assert_data_3.pred_txt = "\\separated((char *)dest + (0 .. n - 1), (char *)src + (0 .. n - 1))";
    __gen_e_acsl_assert_data_3.file = "FRAMAC_SHARE/libc/string.h";
    __gen_e_acsl_assert_data_3.fct = "memcpy";
    __gen_e_acsl_assert_data_3.line = 115;
    __gen_e_acsl_assert_data_3.name = "separation";
    __e_acsl_assert(__gen_e_acsl_separated,& __gen_e_acsl_assert_data_3);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
    __gmpz_clear(__gen_e_acsl_n);
    __gmpz_clear(__gen_e_acsl_);
    __gmpz_clear(__gen_e_acsl_sub);
    __gmpz_clear(__gen_e_acsl__2);
    __gmpz_clear(__gen_e_acsl_sub_2);
    __gmpz_clear(__gen_e_acsl_add);
  }
  __retres = memcpy(dest,src,n);
  __e_acsl_initialize(dest,n);
  {
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_7 =
      {.values = (void *)0};
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_7,"\\result",
                                 __retres);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_7,"\\old(dest)",
                                 __gen_e_acsl_at);
    __gen_e_acsl_assert_data_7.blocking = 1;
    __gen_e_acsl_assert_data_7.kind = "Postcondition";
    __gen_e_acsl_assert_data_7.pred_txt = "\\result == \\old(dest)";
    __gen_e_acsl_assert_data_7.file = "FRAMAC_SHARE/libc/string.h";
    __gen_e_acsl_assert_data_7.fct = "memcpy";
    __gen_e_acsl_assert_data_7.line = 119;
    __gen_e_acsl_assert_data_7.name = "result_ptr";
    __e_acsl_assert(__retres == __gen_e_acsl_at,& __gen_e_acsl_assert_data_7);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_7);
    __e_acsl_delete_block((void *)(& src));
    __e_acsl_delete_block((void *)(& dest));
    return __retres;
  }
}


