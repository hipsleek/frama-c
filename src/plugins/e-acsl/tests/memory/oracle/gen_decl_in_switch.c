/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "time.h"
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

void decl_in_switch(int value)
{
  __e_acsl_store_block((void *)(& value),4UL);
  switch (value) {
    int *p;
    __e_acsl_store_block((void *)(& p),8UL);
    default:
    __e_acsl_store_block_duplicate((void *)(& p),8UL);
    __e_acsl_full_init((void *)(& p));
    p = & value;
    __e_acsl_delete_block((void *)(& p));
    break;
    __e_acsl_delete_block((void *)(& p));
  }
  __e_acsl_delete_block((void *)(& value));
  return;
}

void compound_decl_and_init(int value)
{
  int a = 0;
  __e_acsl_store_block((void *)(& a),4UL);
  __e_acsl_full_init((void *)(& a));
  {
    int __gen_e_acsl_valid;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_valid = __e_acsl_valid((void *)(& a),sizeof(int),
                                        (void *)(& a),(void *)0);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"&a",
                                 (void *)(& a));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,"sizeof(int)",
                                   0,sizeof(int));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"\\valid(&a)",0,
                                 __gen_e_acsl_valid);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "\\valid(&a)";
    __gen_e_acsl_assert_data.file = "decl_in_switch.c";
    __gen_e_acsl_assert_data.fct = "compound_decl_and_init";
    __gen_e_acsl_assert_data.line = 22;
    __e_acsl_assert(__gen_e_acsl_valid,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert \valid(&a); */ ;
  switch (value) {
    int b = 2;
    {
      int __gen_e_acsl_valid_2;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
        {.values = (void *)0};
      __gen_e_acsl_valid_2 = __e_acsl_valid((void *)(& b),sizeof(int),
                                            (void *)(& b),(void *)0);
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"&b",
                                   (void *)(& b));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                   "\\valid(&b)",0,__gen_e_acsl_valid_2);
      __gen_e_acsl_assert_data_2.blocking = 1;
      __gen_e_acsl_assert_data_2.kind = "Assertion";
      __gen_e_acsl_assert_data_2.pred_txt = "\\valid(&b)";
      __gen_e_acsl_assert_data_2.file = "decl_in_switch.c";
      __gen_e_acsl_assert_data_2.fct = "compound_decl_and_init";
      __gen_e_acsl_assert_data_2.line = 26;
      __e_acsl_assert(__gen_e_acsl_valid_2,& __gen_e_acsl_assert_data_2);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
    }
    /*@ assert \valid(&b); */ ;
    case 0: ;
    int c = 3;
    __e_acsl_store_block((void *)(& c),4UL);
    __e_acsl_full_init((void *)(& c));
    {
      int __gen_e_acsl_valid_3;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
        {.values = (void *)0};
      __gen_e_acsl_valid_3 = __e_acsl_valid((void *)(& c),sizeof(int),
                                            (void *)(& c),(void *)0);
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,"&c",
                                   (void *)(& c));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_3,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,
                                   "\\valid(&c)",0,__gen_e_acsl_valid_3);
      __gen_e_acsl_assert_data_3.blocking = 1;
      __gen_e_acsl_assert_data_3.kind = "Assertion";
      __gen_e_acsl_assert_data_3.pred_txt = "\\valid(&c)";
      __gen_e_acsl_assert_data_3.file = "decl_in_switch.c";
      __gen_e_acsl_assert_data_3.fct = "compound_decl_and_init";
      __gen_e_acsl_assert_data_3.line = 30;
      __e_acsl_assert(__gen_e_acsl_valid_3,& __gen_e_acsl_assert_data_3);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
    }
    /*@ assert \valid(&c); */ ;
    __e_acsl_delete_block((void *)(& c));
    break;
    case 1: __e_acsl_store_block_duplicate((void *)(& c),4UL);
            ;
    int d = 4;
    __e_acsl_store_block((void *)(& d),4UL);
    __e_acsl_full_init((void *)(& d));
    {
      int __gen_e_acsl_valid_4;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
        {.values = (void *)0};
      __gen_e_acsl_valid_4 = __e_acsl_valid((void *)(& d),sizeof(int),
                                            (void *)(& d),(void *)0);
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"&d",
                                   (void *)(& d));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,
                                   "\\valid(&d)",0,__gen_e_acsl_valid_4);
      __gen_e_acsl_assert_data_4.blocking = 1;
      __gen_e_acsl_assert_data_4.kind = "Assertion";
      __gen_e_acsl_assert_data_4.pred_txt = "\\valid(&d)";
      __gen_e_acsl_assert_data_4.file = "decl_in_switch.c";
      __gen_e_acsl_assert_data_4.fct = "compound_decl_and_init";
      __gen_e_acsl_assert_data_4.line = 35;
      __e_acsl_assert(__gen_e_acsl_valid_4,& __gen_e_acsl_assert_data_4);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
    }
    /*@ assert \valid(&d); */ ;
    __e_acsl_delete_block((void *)(& c));
    __e_acsl_delete_block((void *)(& d));
    break;
    __e_acsl_delete_block((void *)(& d));
    __e_acsl_delete_block((void *)(& c));
  }
  __e_acsl_delete_block((void *)(& a));
  return;
}

void separate_decl_and_init(int value)
{
  int a;
  __e_acsl_store_block((void *)(& a),4UL);
  __e_acsl_full_init((void *)(& a));
  a = 1;
  {
    int __gen_e_acsl_valid;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_valid = __e_acsl_valid((void *)(& a),sizeof(int),
                                        (void *)(& a),(void *)0);
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"&a",
                                 (void *)(& a));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,"sizeof(int)",
                                   0,sizeof(int));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"\\valid(&a)",0,
                                 __gen_e_acsl_valid);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "\\valid(&a)";
    __gen_e_acsl_assert_data.file = "decl_in_switch.c";
    __gen_e_acsl_assert_data.fct = "separate_decl_and_init";
    __gen_e_acsl_assert_data.line = 45;
    __e_acsl_assert(__gen_e_acsl_valid,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert \valid(&a); */ ;
  switch (value) {
    int b;
    int c;
    int d;
    __e_acsl_store_block((void *)(& d),4UL);
    __e_acsl_store_block((void *)(& c),4UL);
    b = 2;
    {
      int __gen_e_acsl_valid_2;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
        {.values = (void *)0};
      __gen_e_acsl_valid_2 = __e_acsl_valid((void *)(& b),sizeof(int),
                                            (void *)(& b),(void *)0);
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"&b",
                                   (void *)(& b));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                   "\\valid(&b)",0,__gen_e_acsl_valid_2);
      __gen_e_acsl_assert_data_2.blocking = 1;
      __gen_e_acsl_assert_data_2.kind = "Assertion";
      __gen_e_acsl_assert_data_2.pred_txt = "\\valid(&b)";
      __gen_e_acsl_assert_data_2.file = "decl_in_switch.c";
      __gen_e_acsl_assert_data_2.fct = "separate_decl_and_init";
      __gen_e_acsl_assert_data_2.line = 50;
      __e_acsl_assert(__gen_e_acsl_valid_2,& __gen_e_acsl_assert_data_2);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
    }
    /*@ assert \valid(&b); */ ;
    case 0:
    __e_acsl_store_block_duplicate((void *)(& c),4UL);
    __e_acsl_store_block_duplicate((void *)(& d),4UL);
    ;
    __e_acsl_full_init((void *)(& c));
    c = 3;
    {
      int __gen_e_acsl_valid_3;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
        {.values = (void *)0};
      __gen_e_acsl_valid_3 = __e_acsl_valid((void *)(& c),sizeof(int),
                                            (void *)(& c),(void *)0);
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,"&c",
                                   (void *)(& c));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_3,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,
                                   "\\valid(&c)",0,__gen_e_acsl_valid_3);
      __gen_e_acsl_assert_data_3.blocking = 1;
      __gen_e_acsl_assert_data_3.kind = "Assertion";
      __gen_e_acsl_assert_data_3.pred_txt = "\\valid(&c)";
      __gen_e_acsl_assert_data_3.file = "decl_in_switch.c";
      __gen_e_acsl_assert_data_3.fct = "separate_decl_and_init";
      __gen_e_acsl_assert_data_3.line = 55;
      __e_acsl_assert(__gen_e_acsl_valid_3,& __gen_e_acsl_assert_data_3);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
    }
    /*@ assert \valid(&c); */ ;
    __e_acsl_delete_block((void *)(& c));
    __e_acsl_delete_block((void *)(& d));
    break;
    case 1:
    __e_acsl_store_block_duplicate((void *)(& c),4UL);
    __e_acsl_store_block_duplicate((void *)(& d),4UL);
    ;
    __e_acsl_full_init((void *)(& d));
    d = 4;
    {
      int __gen_e_acsl_valid_4;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
        {.values = (void *)0};
      __gen_e_acsl_valid_4 = __e_acsl_valid((void *)(& d),sizeof(int),
                                            (void *)(& d),(void *)0);
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"&d",
                                   (void *)(& d));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,
                                   "\\valid(&d)",0,__gen_e_acsl_valid_4);
      __gen_e_acsl_assert_data_4.blocking = 1;
      __gen_e_acsl_assert_data_4.kind = "Assertion";
      __gen_e_acsl_assert_data_4.pred_txt = "\\valid(&d)";
      __gen_e_acsl_assert_data_4.file = "decl_in_switch.c";
      __gen_e_acsl_assert_data_4.fct = "separate_decl_and_init";
      __gen_e_acsl_assert_data_4.line = 61;
      __e_acsl_assert(__gen_e_acsl_valid_4,& __gen_e_acsl_assert_data_4);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
    }
    /*@ assert \valid(&d); */ ;
    __e_acsl_delete_block((void *)(& c));
    __e_acsl_delete_block((void *)(& d));
    break;
    __e_acsl_delete_block((void *)(& d));
    __e_acsl_delete_block((void *)(& c));
  }
  __e_acsl_delete_block((void *)(& a));
  return;
}

void label_in_switch(int value)
{
  int done = 0;
  switch (value) {
    K: ;
    int d = 0;
    __e_acsl_store_block((void *)(& d),4UL);
    __e_acsl_full_init((void *)(& d));
    {
      int __gen_e_acsl_valid;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data =
        {.values = (void *)0};
      __gen_e_acsl_valid = __e_acsl_valid((void *)(& d),sizeof(int),
                                          (void *)(& d),(void *)0);
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"&d",
                                   (void *)(& d));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"\\valid(&d)",
                                   0,__gen_e_acsl_valid);
      __gen_e_acsl_assert_data.blocking = 1;
      __gen_e_acsl_assert_data.kind = "Assertion";
      __gen_e_acsl_assert_data.pred_txt = "\\valid(&d)";
      __gen_e_acsl_assert_data.file = "decl_in_switch.c";
      __gen_e_acsl_assert_data.fct = "label_in_switch";
      __gen_e_acsl_assert_data.line = 74;
      __e_acsl_assert(__gen_e_acsl_valid,& __gen_e_acsl_assert_data);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    }
    /*@ assert \valid(&d); */ ;
    L: case 0: __e_acsl_store_block_duplicate((void *)(& d),4UL);
               ;
    int e = 1;
    __e_acsl_store_block((void *)(& e),4UL);
    __e_acsl_full_init((void *)(& e));
    {
      int __gen_e_acsl_valid_2;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
        {.values = (void *)0};
      __gen_e_acsl_valid_2 = __e_acsl_valid((void *)(& e),sizeof(int),
                                            (void *)(& e),(void *)0);
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"&e",
                                   (void *)(& e));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                   "\\valid(&e)",0,__gen_e_acsl_valid_2);
      __gen_e_acsl_assert_data_2.blocking = 1;
      __gen_e_acsl_assert_data_2.kind = "Assertion";
      __gen_e_acsl_assert_data_2.pred_txt = "\\valid(&e)";
      __gen_e_acsl_assert_data_2.file = "decl_in_switch.c";
      __gen_e_acsl_assert_data_2.fct = "label_in_switch";
      __gen_e_acsl_assert_data_2.line = 80;
      __e_acsl_assert(__gen_e_acsl_valid_2,& __gen_e_acsl_assert_data_2);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
    }
    /*@ assert \valid(&e); */ ;
    __e_acsl_delete_block((void *)(& d));
    __e_acsl_delete_block((void *)(& e));
    break;
    case 1:
    __e_acsl_store_block_duplicate((void *)(& d),4UL);
    __e_acsl_store_block_duplicate((void *)(& e),4UL);
    ;
    int ff = 2;
    __e_acsl_store_block((void *)(& ff),4UL);
    __e_acsl_full_init((void *)(& ff));
    {
      int __gen_e_acsl_valid_3;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
        {.values = (void *)0};
      __gen_e_acsl_valid_3 = __e_acsl_valid((void *)(& ff),sizeof(int),
                                            (void *)(& ff),(void *)0);
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,"&ff",
                                   (void *)(& ff));
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_3,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,
                                   "\\valid(&ff)",0,__gen_e_acsl_valid_3);
      __gen_e_acsl_assert_data_3.blocking = 1;
      __gen_e_acsl_assert_data_3.kind = "Assertion";
      __gen_e_acsl_assert_data_3.pred_txt = "\\valid(&ff)";
      __gen_e_acsl_assert_data_3.file = "decl_in_switch.c";
      __gen_e_acsl_assert_data_3.fct = "label_in_switch";
      __gen_e_acsl_assert_data_3.line = 84;
      __e_acsl_assert(__gen_e_acsl_valid_3,& __gen_e_acsl_assert_data_3);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
    }
    /*@ assert \valid(&ff); */ ;
    __e_acsl_delete_block((void *)(& d));
    __e_acsl_delete_block((void *)(& e));
    __e_acsl_delete_block((void *)(& ff));
    break;
    __e_acsl_delete_block((void *)(& ff));
    __e_acsl_delete_block((void *)(& e));
    __e_acsl_delete_block((void *)(& d));
  }
  if (! done) {
    done = 1;
    if (value < 10) goto L; else goto K;
  }
  return;
}

int main(int argc, char **argv)
{
  int __retres;
  __e_acsl_memory_init(& argc,& argv,8UL);
  decl_in_switch(argc);
  compound_decl_and_init(argc);
  separate_decl_and_init(argc);
  label_in_switch(argc);
  __retres = 0;
  __e_acsl_delete_block((void *)(& argc));
  __e_acsl_memory_clean();
  return __retres;
}


