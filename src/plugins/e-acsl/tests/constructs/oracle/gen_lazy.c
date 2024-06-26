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
  int x = 0;
  int y = 1;
  {
    int __gen_e_acsl_and;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"x",0,x);
    if (x == 0) {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"y",0,y);
      __gen_e_acsl_and = y == 1;
    }
    else __gen_e_acsl_and = 0;
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "x == 0 && y == 1";
    __gen_e_acsl_assert_data.file = "lazy.i";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 9;
    __e_acsl_assert(__gen_e_acsl_and,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert x == 0 && y == 1; */ ;
  {
    int __gen_e_acsl_and_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"x",0,x);
    if (x != 0) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
        {.values = (void *)0};
      __gen_e_acsl_assert_data_3.blocking = 1;
      __gen_e_acsl_assert_data_3.kind = "RTE";
      __gen_e_acsl_assert_data_3.pred_txt = "0 != 0";
      __gen_e_acsl_assert_data_3.file = "lazy.i";
      __gen_e_acsl_assert_data_3.fct = "main";
      __gen_e_acsl_assert_data_3.line = 10;
      __gen_e_acsl_assert_data_3.name = "division_by_zero";
      __e_acsl_assert(0,& __gen_e_acsl_assert_data_3);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"y",0,y);
      __gen_e_acsl_and_2 = y == 1 / 0;
    }
    else __gen_e_acsl_and_2 = 0;
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Assertion";
    __gen_e_acsl_assert_data_2.pred_txt = "!(x != 0 && y == 1 / 0)";
    __gen_e_acsl_assert_data_2.file = "lazy.i";
    __gen_e_acsl_assert_data_2.fct = "main";
    __gen_e_acsl_assert_data_2.line = 10;
    __e_acsl_assert(! __gen_e_acsl_and_2,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
  }
  /*@ assert !(x != 0 && y == 1 / 0); */ ;
  {
    int __gen_e_acsl_or;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,"y",0,y);
    if (y == 1) __gen_e_acsl_or = 1;
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,"x",0,x);
      __gen_e_acsl_or = x == 1;
    }
    __gen_e_acsl_assert_data_4.blocking = 1;
    __gen_e_acsl_assert_data_4.kind = "Assertion";
    __gen_e_acsl_assert_data_4.pred_txt = "y == 1 || x == 1";
    __gen_e_acsl_assert_data_4.file = "lazy.i";
    __gen_e_acsl_assert_data_4.fct = "main";
    __gen_e_acsl_assert_data_4.line = 11;
    __e_acsl_assert(__gen_e_acsl_or,& __gen_e_acsl_assert_data_4);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
  }
  /*@ assert y == 1 || x == 1; */ ;
  {
    int __gen_e_acsl_or_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"x",0,x);
    if (x == 0) __gen_e_acsl_or_2 = 1;
    else {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_6 =
        {.values = (void *)0};
      __gen_e_acsl_assert_data_6.blocking = 1;
      __gen_e_acsl_assert_data_6.kind = "RTE";
      __gen_e_acsl_assert_data_6.pred_txt = "0 != 0";
      __gen_e_acsl_assert_data_6.file = "lazy.i";
      __gen_e_acsl_assert_data_6.fct = "main";
      __gen_e_acsl_assert_data_6.line = 12;
      __gen_e_acsl_assert_data_6.name = "division_by_zero";
      __e_acsl_assert(0,& __gen_e_acsl_assert_data_6);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"y",0,y);
      __gen_e_acsl_or_2 = y == 1 / 0;
    }
    __gen_e_acsl_assert_data_5.blocking = 1;
    __gen_e_acsl_assert_data_5.kind = "Assertion";
    __gen_e_acsl_assert_data_5.pred_txt = "x == 0 || y == 1 / 0";
    __gen_e_acsl_assert_data_5.file = "lazy.i";
    __gen_e_acsl_assert_data_5.fct = "main";
    __gen_e_acsl_assert_data_5.line = 12;
    __e_acsl_assert(__gen_e_acsl_or_2,& __gen_e_acsl_assert_data_5);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
  }
  /*@ assert x == 0 || y == 1 / 0; */ ;
  {
    int __gen_e_acsl_implies;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_7 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_7,"x",0,x);
    if (! (x == 0)) __gen_e_acsl_implies = 1;
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_7,"y",0,y);
      __gen_e_acsl_implies = y == 1;
    }
    __gen_e_acsl_assert_data_7.blocking = 1;
    __gen_e_acsl_assert_data_7.kind = "Assertion";
    __gen_e_acsl_assert_data_7.pred_txt = "x == 0 ==> y == 1";
    __gen_e_acsl_assert_data_7.file = "lazy.i";
    __gen_e_acsl_assert_data_7.fct = "main";
    __gen_e_acsl_assert_data_7.line = 13;
    __e_acsl_assert(__gen_e_acsl_implies,& __gen_e_acsl_assert_data_7);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_7);
  }
  /*@ assert x == 0 ==> y == 1; */ ;
  {
    int __gen_e_acsl_implies_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_8 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_8,"x",0,x);
    if (! (x == 1)) __gen_e_acsl_implies_2 = 1;
    else {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_9 =
        {.values = (void *)0};
      __gen_e_acsl_assert_data_9.blocking = 1;
      __gen_e_acsl_assert_data_9.kind = "RTE";
      __gen_e_acsl_assert_data_9.pred_txt = "0 != 0";
      __gen_e_acsl_assert_data_9.file = "lazy.i";
      __gen_e_acsl_assert_data_9.fct = "main";
      __gen_e_acsl_assert_data_9.line = 14;
      __gen_e_acsl_assert_data_9.name = "division_by_zero";
      __e_acsl_assert(0,& __gen_e_acsl_assert_data_9);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_8,"y",0,y);
      __gen_e_acsl_implies_2 = y == 1 / 0;
    }
    __gen_e_acsl_assert_data_8.blocking = 1;
    __gen_e_acsl_assert_data_8.kind = "Assertion";
    __gen_e_acsl_assert_data_8.pred_txt = "x == 1 ==> y == 1 / 0";
    __gen_e_acsl_assert_data_8.file = "lazy.i";
    __gen_e_acsl_assert_data_8.fct = "main";
    __gen_e_acsl_assert_data_8.line = 14;
    __e_acsl_assert(__gen_e_acsl_implies_2,& __gen_e_acsl_assert_data_8);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_8);
  }
  /*@ assert x == 1 ==> y == 1 / 0; */ ;
  {
    int __gen_e_acsl_if;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_10 =
      {.values = (void *)0};
    if (x != 0) {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_10,"x",0,x);
      __gen_e_acsl_if = x != 0;
    }
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_10,"y",0,y);
      __gen_e_acsl_if = y != 0;
    }
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_10,"x",0,x);
    __gen_e_acsl_assert_data_10.blocking = 1;
    __gen_e_acsl_assert_data_10.kind = "Assertion";
    __gen_e_acsl_assert_data_10.pred_txt = "x != 0? x != 0: y != 0";
    __gen_e_acsl_assert_data_10.file = "lazy.i";
    __gen_e_acsl_assert_data_10.fct = "main";
    __gen_e_acsl_assert_data_10.line = 15;
    __e_acsl_assert(__gen_e_acsl_if,& __gen_e_acsl_assert_data_10);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_10);
  }
  /*@ assert x != 0? x != 0: y != 0; */ ;
  {
    int __gen_e_acsl_if_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_11 =
      {.values = (void *)0};
    if (y != 0) {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_11,"y",0,y);
      __gen_e_acsl_if_2 = y != 0;
    }
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_11,"x",0,x);
      __gen_e_acsl_if_2 = x != 0;
    }
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_11,"y",0,y);
    __gen_e_acsl_assert_data_11.blocking = 1;
    __gen_e_acsl_assert_data_11.kind = "Assertion";
    __gen_e_acsl_assert_data_11.pred_txt = "y != 0? y != 0: x != 0";
    __gen_e_acsl_assert_data_11.file = "lazy.i";
    __gen_e_acsl_assert_data_11.fct = "main";
    __gen_e_acsl_assert_data_11.line = 16;
    __e_acsl_assert(__gen_e_acsl_if_2,& __gen_e_acsl_assert_data_11);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_11);
  }
  /*@ assert y != 0? y != 0: x != 0; */ ;
  {
    int __gen_e_acsl_if_3;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_12 =
      {.values = (void *)0};
    if (x == 1) {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_12,"x",0,x);
      __gen_e_acsl_if_3 = x == 18;
    }
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_12,"x",0,x);
      __gen_e_acsl_if_3 = x == 0;
    }
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_12,"x",0,x);
    __gen_e_acsl_assert_data_12.blocking = 1;
    __gen_e_acsl_assert_data_12.kind = "Assertion";
    __gen_e_acsl_assert_data_12.pred_txt = "x == 1? x == 18: x == 0";
    __gen_e_acsl_assert_data_12.file = "lazy.i";
    __gen_e_acsl_assert_data_12.fct = "main";
    __gen_e_acsl_assert_data_12.line = 17;
    __e_acsl_assert(__gen_e_acsl_if_3,& __gen_e_acsl_assert_data_12);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_12);
  }
  /*@ assert x == 1? x == 18: x == 0; */ ;
  {
    int __gen_e_acsl_implies_3;
    int __gen_e_acsl_equiv;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_13 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_13,"x",0,x);
    if (! (x == 2)) __gen_e_acsl_implies_3 = 1;
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_13,"y",0,y);
      __gen_e_acsl_implies_3 = y == 3;
    }
    if (__gen_e_acsl_implies_3) {
      int __gen_e_acsl_implies_4;
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_13,"y",0,y);
      if (! (y == 3)) __gen_e_acsl_implies_4 = 1;
      else {
        __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_13,"x",0,x);
        __gen_e_acsl_implies_4 = x == 2;
      }
      __gen_e_acsl_equiv = __gen_e_acsl_implies_4;
    }
    else __gen_e_acsl_equiv = 0;
    __gen_e_acsl_assert_data_13.blocking = 1;
    __gen_e_acsl_assert_data_13.kind = "Assertion";
    __gen_e_acsl_assert_data_13.pred_txt = "x == 2 <==> y == 3";
    __gen_e_acsl_assert_data_13.file = "lazy.i";
    __gen_e_acsl_assert_data_13.fct = "main";
    __gen_e_acsl_assert_data_13.line = 20;
    __e_acsl_assert(__gen_e_acsl_equiv,& __gen_e_acsl_assert_data_13);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_13);
  }
  /*@ assert x == 2 <==> y == 3; */ ;
  {
    int __gen_e_acsl_implies_5;
    int __gen_e_acsl_equiv_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_14 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_14,"x",0,x);
    if (! (x == 0)) __gen_e_acsl_implies_5 = 1;
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_14,"y",0,y);
      __gen_e_acsl_implies_5 = y == 1;
    }
    if (__gen_e_acsl_implies_5) {
      int __gen_e_acsl_implies_6;
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_14,"y",0,y);
      if (! (y == 1)) __gen_e_acsl_implies_6 = 1;
      else {
        __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_14,"x",0,x);
        __gen_e_acsl_implies_6 = x == 0;
      }
      __gen_e_acsl_equiv_2 = __gen_e_acsl_implies_6;
    }
    else __gen_e_acsl_equiv_2 = 0;
    __gen_e_acsl_assert_data_14.blocking = 1;
    __gen_e_acsl_assert_data_14.kind = "Assertion";
    __gen_e_acsl_assert_data_14.pred_txt = "x == 0 <==> y == 1";
    __gen_e_acsl_assert_data_14.file = "lazy.i";
    __gen_e_acsl_assert_data_14.fct = "main";
    __gen_e_acsl_assert_data_14.line = 21;
    __e_acsl_assert(__gen_e_acsl_equiv_2,& __gen_e_acsl_assert_data_14);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_14);
  }
  /*@ assert x == 0 <==> y == 1; */ ;
  {
    int __gen_e_acsl_if_4;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_15 =
      {.values = (void *)0};
    if (x != 0) __gen_e_acsl_if_4 = x; else __gen_e_acsl_if_4 = y;
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_15,"x",0,x);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_15,"x",0,x);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_15,"y",0,y);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_15,"x",0,x);
    __gen_e_acsl_assert_data_15.blocking = 1;
    __gen_e_acsl_assert_data_15.kind = "Assertion";
    __gen_e_acsl_assert_data_15.pred_txt = "((x != 0? x: y) != 0) == (x == 0)";
    __gen_e_acsl_assert_data_15.file = "lazy.i";
    __gen_e_acsl_assert_data_15.fct = "main";
    __gen_e_acsl_assert_data_15.line = 24;
    __e_acsl_assert((__gen_e_acsl_if_4 != 0) == (x == 0),
                    & __gen_e_acsl_assert_data_15);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_15);
  }
  /*@ assert ((x != 0? x: y) != 0) == (x == 0); */ ;
  {
    int __gen_e_acsl_and_3;
    int __gen_e_acsl_or_3;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_16 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_16,"x",0,x);
    if (x != 0) {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_16,"y",0,y);
      __gen_e_acsl_and_3 = y != 0;
    }
    else __gen_e_acsl_and_3 = 0;
    if (__gen_e_acsl_and_3) __gen_e_acsl_or_3 = 1;
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_16,"y",0,y);
      __gen_e_acsl_or_3 = y != 0;
    }
    __gen_e_acsl_assert_data_16.blocking = 1;
    __gen_e_acsl_assert_data_16.kind = "Assertion";
    __gen_e_acsl_assert_data_16.pred_txt = "(x != 0 && y != 0) || y != 0";
    __gen_e_acsl_assert_data_16.file = "lazy.i";
    __gen_e_acsl_assert_data_16.fct = "main";
    __gen_e_acsl_assert_data_16.line = 25;
    __e_acsl_assert(__gen_e_acsl_or_3,& __gen_e_acsl_assert_data_16);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_16);
  }
  /*@ assert (x != 0 && y != 0) || y != 0; */ ;
  {
    int __gen_e_acsl_or_4;
    int __gen_e_acsl_and_4;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_17 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_17,"x",0,x);
    if (x != 0) __gen_e_acsl_or_4 = 1;
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_17,"y",0,y);
      __gen_e_acsl_or_4 = y != 0;
    }
    if (__gen_e_acsl_or_4) {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_17,"y",0,y);
      __gen_e_acsl_and_4 = y == 1;
    }
    else __gen_e_acsl_and_4 = 0;
    __gen_e_acsl_assert_data_17.blocking = 1;
    __gen_e_acsl_assert_data_17.kind = "Assertion";
    __gen_e_acsl_assert_data_17.pred_txt = "(x != 0 || y != 0) && y == 1";
    __gen_e_acsl_assert_data_17.file = "lazy.i";
    __gen_e_acsl_assert_data_17.fct = "main";
    __gen_e_acsl_assert_data_17.line = 26;
    __e_acsl_assert(__gen_e_acsl_and_4,& __gen_e_acsl_assert_data_17);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_17);
  }
  /*@ assert (x != 0 || y != 0) && y == 1; */ ;
  {
    int __gen_e_acsl_or_5;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_18 =
      {.values = (void *)0};
    if (x != 0) __gen_e_acsl_or_5 = 1; else __gen_e_acsl_or_5 = y != 0;
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_18,"x",0,x);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_18,"y",0,y);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_18,"y",0,y);
    __gen_e_acsl_assert_data_18.blocking = 1;
    __gen_e_acsl_assert_data_18.kind = "Assertion";
    __gen_e_acsl_assert_data_18.pred_txt = "(x != 0 || y != 0) == (y != 0)";
    __gen_e_acsl_assert_data_18.file = "lazy.i";
    __gen_e_acsl_assert_data_18.fct = "main";
    __gen_e_acsl_assert_data_18.line = 27;
    __e_acsl_assert(__gen_e_acsl_or_5 == (y != 0),
                    & __gen_e_acsl_assert_data_18);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_18);
  }
  /*@ assert (x != 0 || y != 0) == (y != 0); */ ;
  {
    int __gen_e_acsl_and_5;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_19 =
      {.values = (void *)0};
    if (x != 0) __gen_e_acsl_and_5 = y != 0; else __gen_e_acsl_and_5 = 0;
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_19,"x",0,x);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_19,"y",0,y);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_19,"x",0,x);
    __gen_e_acsl_assert_data_19.blocking = 1;
    __gen_e_acsl_assert_data_19.kind = "Assertion";
    __gen_e_acsl_assert_data_19.pred_txt = "(x != 0 && y != 0) == (x != 0)";
    __gen_e_acsl_assert_data_19.file = "lazy.i";
    __gen_e_acsl_assert_data_19.fct = "main";
    __gen_e_acsl_assert_data_19.line = 28;
    __e_acsl_assert(__gen_e_acsl_and_5 == (x != 0),
                    & __gen_e_acsl_assert_data_19);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_19);
  }
  /*@ assert (x != 0 && y != 0) == (x != 0); */ ;
  __retres = 0;
  __e_acsl_memory_clean();
  return __retres;
}


