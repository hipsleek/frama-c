/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "time.h"
char *__gen_e_acsl_literal_string;
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

void __e_acsl_globals_init(void)
{
  static char __e_acsl_already_run = 0;
  if (! __e_acsl_already_run) {
    __e_acsl_already_run = 1;
    __gen_e_acsl_literal_string = "%d";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string,sizeof("%d"));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string);
  }
  return;
}

int main(void)
{
  int __retres;
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  __e_acsl_globals_init();
  {
    char buf[4];
    __e_acsl_store_block((void *)(buf),4UL);
    {
      int __gen_e_acsl_size;
      int __gen_e_acsl_if;
      int __gen_e_acsl_initialized;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data =
        {.values = (void *)0};
      __gen_e_acsl_size = 1 * ((3 - 0) + 1);
      if (__gen_e_acsl_size <= 0) __gen_e_acsl_if = 0;
      else __gen_e_acsl_if = __gen_e_acsl_size;
      __gen_e_acsl_initialized = __e_acsl_initialized((void *)(buf + 1 * 0),
                                                      (size_t)__gen_e_acsl_if);
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"(char *)buf",
                                   (void *)(buf));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"sizeof(char)",
                                   0,1);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"sizeof(char)",
                                   0,1);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"size",0,
                                   __gen_e_acsl_size);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"size",0,
                                   __gen_e_acsl_size);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,
                                   "\\initialized(&buf[0 .. 3])",0,
                                   __gen_e_acsl_initialized);
      __gen_e_acsl_assert_data.blocking = 1;
      __gen_e_acsl_assert_data.kind = "Assertion";
      __gen_e_acsl_assert_data.pred_txt = "!\\initialized(&buf[0 .. 3])";
      __gen_e_acsl_assert_data.file = "sprintf.c";
      __gen_e_acsl_assert_data.fct = "main";
      __gen_e_acsl_assert_data.line = 10;
      __e_acsl_assert(! __gen_e_acsl_initialized,& __gen_e_acsl_assert_data);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    }
    /*@ assert !\initialized(&buf[0 .. 3]); */ ;
    __e_acsl_builtin_sprintf("d",buf,__gen_e_acsl_literal_string,10);
    {
      int __gen_e_acsl_size_2;
      int __gen_e_acsl_if_2;
      int __gen_e_acsl_initialized_2;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
        {.values = (void *)0};
      __gen_e_acsl_size_2 = 1 * ((2 - 0) + 1);
      if (__gen_e_acsl_size_2 <= 0) __gen_e_acsl_if_2 = 0;
      else __gen_e_acsl_if_2 = __gen_e_acsl_size_2;
      __gen_e_acsl_initialized_2 = __e_acsl_initialized((void *)(buf + 1 * 0),
                                                        (size_t)__gen_e_acsl_if_2);
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,
                                   "(char *)buf",(void *)(buf));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                   "sizeof(char)",0,1);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                   "sizeof(char)",0,1);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"size",0,
                                   __gen_e_acsl_size_2);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"size",0,
                                   __gen_e_acsl_size_2);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                   "\\initialized(&buf[0 .. 2])",0,
                                   __gen_e_acsl_initialized_2);
      __gen_e_acsl_assert_data_2.blocking = 1;
      __gen_e_acsl_assert_data_2.kind = "Assertion";
      __gen_e_acsl_assert_data_2.pred_txt = "\\initialized(&buf[0 .. 2])";
      __gen_e_acsl_assert_data_2.file = "sprintf.c";
      __gen_e_acsl_assert_data_2.fct = "main";
      __gen_e_acsl_assert_data_2.line = 13;
      __e_acsl_assert(__gen_e_acsl_initialized_2,
                      & __gen_e_acsl_assert_data_2);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
    }
    /*@ assert \initialized(&buf[0 .. 2]); */ ;
    {
      int __gen_e_acsl_initialized_3;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
        {.values = (void *)0};
      __gen_e_acsl_initialized_3 = __e_acsl_initialized((void *)(& buf[3]),
                                                        sizeof(char));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,"&buf[3]",
                                   (void *)(& buf[3]));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_3,
                                     "sizeof(char)",0,sizeof(char));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,
                                   "\\initialized(&buf[3])",0,
                                   __gen_e_acsl_initialized_3);
      __gen_e_acsl_assert_data_3.blocking = 1;
      __gen_e_acsl_assert_data_3.kind = "Assertion";
      __gen_e_acsl_assert_data_3.pred_txt = "!\\initialized(&buf[3])";
      __gen_e_acsl_assert_data_3.file = "sprintf.c";
      __gen_e_acsl_assert_data_3.fct = "main";
      __gen_e_acsl_assert_data_3.line = 14;
      __e_acsl_assert(! __gen_e_acsl_initialized_3,
                      & __gen_e_acsl_assert_data_3);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
    }
    /*@ assert !\initialized(&buf[3]); */ ;
    __e_acsl_delete_block((void *)(buf));
  }
  {
    char buf_0[4];
    __e_acsl_store_block((void *)(buf_0),4UL);
    {
      int __gen_e_acsl_size_3;
      int __gen_e_acsl_if_3;
      int __gen_e_acsl_initialized_4;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
        {.values = (void *)0};
      __gen_e_acsl_size_3 = 1 * ((3 - 0) + 1);
      if (__gen_e_acsl_size_3 <= 0) __gen_e_acsl_if_3 = 0;
      else __gen_e_acsl_if_3 = __gen_e_acsl_size_3;
      __gen_e_acsl_initialized_4 = __e_acsl_initialized((void *)(buf_0 + 
                                                                 1 * 0),
                                                        (size_t)__gen_e_acsl_if_3);
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,
                                   "(char *)buf_0",(void *)(buf_0));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,
                                   "sizeof(char)",0,1);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,
                                   "sizeof(char)",0,1);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,"size",0,
                                   __gen_e_acsl_size_3);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,"size",0,
                                   __gen_e_acsl_size_3);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,
                                   "\\initialized(&buf_0[0 .. 3])",0,
                                   __gen_e_acsl_initialized_4);
      __gen_e_acsl_assert_data_4.blocking = 1;
      __gen_e_acsl_assert_data_4.kind = "Assertion";
      __gen_e_acsl_assert_data_4.pred_txt = "!\\initialized(&buf_0[0 .. 3])";
      __gen_e_acsl_assert_data_4.file = "sprintf.c";
      __gen_e_acsl_assert_data_4.fct = "main";
      __gen_e_acsl_assert_data_4.line = 18;
      __e_acsl_assert(! __gen_e_acsl_initialized_4,
                      & __gen_e_acsl_assert_data_4);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
    }
    /*@ assert !\initialized(&buf_0[0 .. 3]); */ ;
    __e_acsl_builtin_snprintf("d",buf_0,(size_t)2,
                              __gen_e_acsl_literal_string,10);
    {
      int __gen_e_acsl_size_4;
      int __gen_e_acsl_if_4;
      int __gen_e_acsl_initialized_5;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
        {.values = (void *)0};
      __gen_e_acsl_size_4 = 1 * ((1 - 0) + 1);
      if (__gen_e_acsl_size_4 <= 0) __gen_e_acsl_if_4 = 0;
      else __gen_e_acsl_if_4 = __gen_e_acsl_size_4;
      __gen_e_acsl_initialized_5 = __e_acsl_initialized((void *)(buf_0 + 
                                                                 1 * 0),
                                                        (size_t)__gen_e_acsl_if_4);
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_5,
                                   "(char *)buf_0",(void *)(buf_0));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,
                                   "sizeof(char)",0,1);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,
                                   "sizeof(char)",0,1);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"size",0,
                                   __gen_e_acsl_size_4);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"size",0,
                                   __gen_e_acsl_size_4);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,
                                   "\\initialized(&buf_0[0 .. 1])",0,
                                   __gen_e_acsl_initialized_5);
      __gen_e_acsl_assert_data_5.blocking = 1;
      __gen_e_acsl_assert_data_5.kind = "Assertion";
      __gen_e_acsl_assert_data_5.pred_txt = "\\initialized(&buf_0[0 .. 1])";
      __gen_e_acsl_assert_data_5.file = "sprintf.c";
      __gen_e_acsl_assert_data_5.fct = "main";
      __gen_e_acsl_assert_data_5.line = 21;
      __e_acsl_assert(__gen_e_acsl_initialized_5,
                      & __gen_e_acsl_assert_data_5);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
    }
    /*@ assert \initialized(&buf_0[0 .. 1]); */ ;
    {
      int __gen_e_acsl_initialized_6;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_6 =
        {.values = (void *)0};
      __gen_e_acsl_initialized_6 = __e_acsl_initialized((void *)(& buf_0[2]),
                                                        sizeof(char));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_6,"&buf_0[2]",
                                   (void *)(& buf_0[2]));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_6,
                                     "sizeof(char)",0,sizeof(char));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_6,
                                   "\\initialized(&buf_0[2])",0,
                                   __gen_e_acsl_initialized_6);
      __gen_e_acsl_assert_data_6.blocking = 1;
      __gen_e_acsl_assert_data_6.kind = "Assertion";
      __gen_e_acsl_assert_data_6.pred_txt = "!\\initialized(&buf_0[2])";
      __gen_e_acsl_assert_data_6.file = "sprintf.c";
      __gen_e_acsl_assert_data_6.fct = "main";
      __gen_e_acsl_assert_data_6.line = 22;
      __e_acsl_assert(! __gen_e_acsl_initialized_6,
                      & __gen_e_acsl_assert_data_6);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_6);
    }
    /*@ assert !\initialized(&buf_0[2]); */ ;
    {
      int __gen_e_acsl_initialized_7;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_7 =
        {.values = (void *)0};
      __gen_e_acsl_initialized_7 = __e_acsl_initialized((void *)(& buf_0[3]),
                                                        sizeof(char));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_7,"&buf_0[3]",
                                   (void *)(& buf_0[3]));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_7,
                                     "sizeof(char)",0,sizeof(char));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_7,
                                   "\\initialized(&buf_0[3])",0,
                                   __gen_e_acsl_initialized_7);
      __gen_e_acsl_assert_data_7.blocking = 1;
      __gen_e_acsl_assert_data_7.kind = "Assertion";
      __gen_e_acsl_assert_data_7.pred_txt = "!\\initialized(&buf_0[3])";
      __gen_e_acsl_assert_data_7.file = "sprintf.c";
      __gen_e_acsl_assert_data_7.fct = "main";
      __gen_e_acsl_assert_data_7.line = 23;
      __e_acsl_assert(! __gen_e_acsl_initialized_7,
                      & __gen_e_acsl_assert_data_7);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_7);
    }
    /*@ assert !\initialized(&buf_0[3]); */ ;
    __e_acsl_delete_block((void *)(buf_0));
  }
  {
    char buf_1[4];
    __e_acsl_store_block((void *)(buf_1),4UL);
    {
      int __gen_e_acsl_size_5;
      int __gen_e_acsl_if_5;
      int __gen_e_acsl_initialized_8;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_8 =
        {.values = (void *)0};
      __gen_e_acsl_size_5 = 1 * ((3 - 0) + 1);
      if (__gen_e_acsl_size_5 <= 0) __gen_e_acsl_if_5 = 0;
      else __gen_e_acsl_if_5 = __gen_e_acsl_size_5;
      __gen_e_acsl_initialized_8 = __e_acsl_initialized((void *)(buf_1 + 
                                                                 1 * 0),
                                                        (size_t)__gen_e_acsl_if_5);
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_8,
                                   "(char *)buf_1",(void *)(buf_1));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_8,
                                   "sizeof(char)",0,1);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_8,
                                   "sizeof(char)",0,1);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_8,"size",0,
                                   __gen_e_acsl_size_5);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_8,"size",0,
                                   __gen_e_acsl_size_5);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_8,
                                   "\\initialized(&buf_1[0 .. 3])",0,
                                   __gen_e_acsl_initialized_8);
      __gen_e_acsl_assert_data_8.blocking = 1;
      __gen_e_acsl_assert_data_8.kind = "Assertion";
      __gen_e_acsl_assert_data_8.pred_txt = "!\\initialized(&buf_1[0 .. 3])";
      __gen_e_acsl_assert_data_8.file = "sprintf.c";
      __gen_e_acsl_assert_data_8.fct = "main";
      __gen_e_acsl_assert_data_8.line = 27;
      __e_acsl_assert(! __gen_e_acsl_initialized_8,
                      & __gen_e_acsl_assert_data_8);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_8);
    }
    /*@ assert !\initialized(&buf_1[0 .. 3]); */ ;
    __e_acsl_builtin_snprintf("d",buf_1,(size_t)4,
                              __gen_e_acsl_literal_string,10);
    {
      int __gen_e_acsl_size_6;
      int __gen_e_acsl_if_6;
      int __gen_e_acsl_initialized_9;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_9 =
        {.values = (void *)0};
      __gen_e_acsl_size_6 = 1 * ((2 - 0) + 1);
      if (__gen_e_acsl_size_6 <= 0) __gen_e_acsl_if_6 = 0;
      else __gen_e_acsl_if_6 = __gen_e_acsl_size_6;
      __gen_e_acsl_initialized_9 = __e_acsl_initialized((void *)(buf_1 + 
                                                                 1 * 0),
                                                        (size_t)__gen_e_acsl_if_6);
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_9,
                                   "(char *)buf_1",(void *)(buf_1));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_9,
                                   "sizeof(char)",0,1);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_9,
                                   "sizeof(char)",0,1);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_9,"size",0,
                                   __gen_e_acsl_size_6);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_9,"size",0,
                                   __gen_e_acsl_size_6);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_9,
                                   "\\initialized(&buf_1[0 .. 2])",0,
                                   __gen_e_acsl_initialized_9);
      __gen_e_acsl_assert_data_9.blocking = 1;
      __gen_e_acsl_assert_data_9.kind = "Assertion";
      __gen_e_acsl_assert_data_9.pred_txt = "\\initialized(&buf_1[0 .. 2])";
      __gen_e_acsl_assert_data_9.file = "sprintf.c";
      __gen_e_acsl_assert_data_9.fct = "main";
      __gen_e_acsl_assert_data_9.line = 30;
      __e_acsl_assert(__gen_e_acsl_initialized_9,
                      & __gen_e_acsl_assert_data_9);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_9);
    }
    /*@ assert \initialized(&buf_1[0 .. 2]); */ ;
    {
      int __gen_e_acsl_initialized_10;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_10 =
        {.values = (void *)0};
      __gen_e_acsl_initialized_10 = __e_acsl_initialized((void *)(& buf_1[3]),
                                                         sizeof(char));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_10,"&buf_1[3]",
                                   (void *)(& buf_1[3]));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_10,
                                     "sizeof(char)",0,sizeof(char));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_10,
                                   "\\initialized(&buf_1[3])",0,
                                   __gen_e_acsl_initialized_10);
      __gen_e_acsl_assert_data_10.blocking = 1;
      __gen_e_acsl_assert_data_10.kind = "Assertion";
      __gen_e_acsl_assert_data_10.pred_txt = "!\\initialized(&buf_1[3])";
      __gen_e_acsl_assert_data_10.file = "sprintf.c";
      __gen_e_acsl_assert_data_10.fct = "main";
      __gen_e_acsl_assert_data_10.line = 31;
      __e_acsl_assert(! __gen_e_acsl_initialized_10,
                      & __gen_e_acsl_assert_data_10);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_10);
    }
    /*@ assert !\initialized(&buf_1[3]); */ ;
    __e_acsl_delete_block((void *)(buf_1));
  }
  __retres = 0;
  __e_acsl_memory_clean();
  return __retres;
}


