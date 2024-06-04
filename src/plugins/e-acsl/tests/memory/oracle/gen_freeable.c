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

char array[1024];
void __e_acsl_globals_init(void)
{
  static char __e_acsl_already_run = 0;
  if (! __e_acsl_already_run) {
    __e_acsl_already_run = 1;
    __e_acsl_store_block((void *)(array),1024UL);
    __e_acsl_full_init((void *)(& array));
  }
  return;
}

void __e_acsl_globals_clean(void)
{
  __e_acsl_delete_block((void *)(array));
  return;
}

int main(void)
{
  int __retres;
  int *p;
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  __e_acsl_globals_init();
  __e_acsl_store_block((void *)(& p),8UL);
  {
    int __gen_e_acsl_freeable;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    /*@ assert Eva: initialization: \initialized(&p); */
    __gen_e_acsl_freeable = __e_acsl_freeable((void *)p);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"p",(void *)p);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"\\freeable(p)",
                                 0,__gen_e_acsl_freeable);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "!\\freeable(p)";
    __gen_e_acsl_assert_data.file = "freeable.c";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 14;
    __e_acsl_assert(! __gen_e_acsl_freeable,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert !\freeable(p); */ ;
  {
    int __gen_e_acsl_freeable_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __gen_e_acsl_freeable_2 = __e_acsl_freeable((void *)0);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                 "\\freeable((void *)0)",0,
                                 __gen_e_acsl_freeable_2);
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Assertion";
    __gen_e_acsl_assert_data_2.pred_txt = "!\\freeable((void *)0)";
    __gen_e_acsl_assert_data_2.file = "freeable.c";
    __gen_e_acsl_assert_data_2.fct = "main";
    __gen_e_acsl_assert_data_2.line = 15;
    __e_acsl_assert(! __gen_e_acsl_freeable_2,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
  }
  /*@ assert !\freeable((void *)0); */ ;
  __e_acsl_full_init((void *)(& p));
  p = (int *)malloc((unsigned long)4 * sizeof(int));
  {
    int __gen_e_acsl_freeable_3;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
      {.values = (void *)0};
    __gen_e_acsl_freeable_3 = __e_acsl_freeable((void *)(p + 1));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,"p",(void *)p);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,
                                 "\\freeable(p + 1)",0,
                                 __gen_e_acsl_freeable_3);
    __gen_e_acsl_assert_data_3.blocking = 1;
    __gen_e_acsl_assert_data_3.kind = "Assertion";
    __gen_e_acsl_assert_data_3.pred_txt = "!\\freeable(p + 1)";
    __gen_e_acsl_assert_data_3.file = "freeable.c";
    __gen_e_acsl_assert_data_3.fct = "main";
    __gen_e_acsl_assert_data_3.line = 17;
    __e_acsl_assert(! __gen_e_acsl_freeable_3,& __gen_e_acsl_assert_data_3);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
  }
  /*@ assert !\freeable(p + 1); */ ;
  {
    int __gen_e_acsl_freeable_4;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
      {.values = (void *)0};
    __gen_e_acsl_freeable_4 = __e_acsl_freeable((void *)p);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"p",(void *)p);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,
                                 "\\freeable(p)",0,__gen_e_acsl_freeable_4);
    __gen_e_acsl_assert_data_4.blocking = 1;
    __gen_e_acsl_assert_data_4.kind = "Assertion";
    __gen_e_acsl_assert_data_4.pred_txt = "\\freeable(p)";
    __gen_e_acsl_assert_data_4.file = "freeable.c";
    __gen_e_acsl_assert_data_4.fct = "main";
    __gen_e_acsl_assert_data_4.line = 18;
    __e_acsl_assert(__gen_e_acsl_freeable_4,& __gen_e_acsl_assert_data_4);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
  }
  /*@ assert \freeable(p); */ ;
  free((void *)p);
  {
    int __gen_e_acsl_freeable_5;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
      {.values = (void *)0};
    __gen_e_acsl_freeable_5 = __e_acsl_freeable((void *)p);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_5,"p",(void *)p);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,
                                 "\\freeable(p)",0,__gen_e_acsl_freeable_5);
    __gen_e_acsl_assert_data_5.blocking = 1;
    __gen_e_acsl_assert_data_5.kind = "Assertion";
    __gen_e_acsl_assert_data_5.pred_txt = "!\\freeable(p)";
    __gen_e_acsl_assert_data_5.file = "freeable.c";
    __gen_e_acsl_assert_data_5.fct = "main";
    __gen_e_acsl_assert_data_5.line = 20;
    __e_acsl_assert(! __gen_e_acsl_freeable_5,& __gen_e_acsl_assert_data_5);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
  }
  /*@ assert !\freeable(p); */ ;
  {
    int __gen_e_acsl_freeable_6;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_6 =
      {.values = (void *)0};
    __gen_e_acsl_freeable_6 = __e_acsl_freeable((void *)(array));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_6,
                                 "(char *)array",(void *)(array));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_6,
                                 "\\freeable((char *)array)",0,
                                 __gen_e_acsl_freeable_6);
    __gen_e_acsl_assert_data_6.blocking = 1;
    __gen_e_acsl_assert_data_6.kind = "Assertion";
    __gen_e_acsl_assert_data_6.pred_txt = "!\\freeable((char *)array)";
    __gen_e_acsl_assert_data_6.file = "freeable.c";
    __gen_e_acsl_assert_data_6.fct = "main";
    __gen_e_acsl_assert_data_6.line = 23;
    __e_acsl_assert(! __gen_e_acsl_freeable_6,& __gen_e_acsl_assert_data_6);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_6);
  }
  /*@ assert !\freeable((char *)array); */ ;
  {
    int __gen_e_acsl_freeable_7;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_7 =
      {.values = (void *)0};
    __gen_e_acsl_freeable_7 = __e_acsl_freeable((void *)(& array[5]));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_7,"&array[5]",
                                 (void *)(& array[5]));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_7,
                                 "\\freeable(&array[5])",0,
                                 __gen_e_acsl_freeable_7);
    __gen_e_acsl_assert_data_7.blocking = 1;
    __gen_e_acsl_assert_data_7.kind = "Assertion";
    __gen_e_acsl_assert_data_7.pred_txt = "!\\freeable(&array[5])";
    __gen_e_acsl_assert_data_7.file = "freeable.c";
    __gen_e_acsl_assert_data_7.fct = "main";
    __gen_e_acsl_assert_data_7.line = 24;
    __e_acsl_assert(! __gen_e_acsl_freeable_7,& __gen_e_acsl_assert_data_7);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_7);
  }
  /*@ assert !\freeable(&array[5]); */ ;
  __retres = 0;
  __e_acsl_delete_block((void *)(& p));
  __e_acsl_globals_clean();
  __e_acsl_memory_clean();
  return __retres;
}

