/* Generated by Frama-C */
#include "ctype.h"
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "time.h"
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

/*@ requires c_uchar_or_eof: (0 <= c <= 255) || c == -1;
    assigns \result;
    assigns \result \from c;
    
    behavior definitely_match:
      assumes c_upper: 'A' <= c <= 'Z';
      ensures nonzero_result: \result < 0 || \result > 0;
    
    behavior definitely_not_match:
      assumes c_non_upper: c == -1 || (0 <= c < 'A') || ('Z' < c <= 127);
      ensures zero_result: \result == 0;
    
    disjoint behaviors definitely_not_match, definitely_match;
 */
int __gen_e_acsl_isupper(int c);

int main(int argc, char const **argv)
{
  int __retres;
  int tmp;
  __e_acsl_memory_init(& argc,(char ***)(& argv),8UL);
  tmp = __gen_e_acsl_isupper(argc);
  char c = (char)tmp;
  __e_acsl_store_block((void *)(& c),1UL);
  __e_acsl_full_init((void *)(& c));
  char *d = & c;
  __e_acsl_store_block((void *)(& d),8UL);
  __e_acsl_full_init((void *)(& d));
  {
    int __gen_e_acsl_initialized;
    int __gen_e_acsl_and;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_initialized = __e_acsl_initialized((void *)(& d),
                                                    sizeof(char *));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"&d",
                                 (void *)(& d));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                   "sizeof(char *)",0,sizeof(char *));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,
                                 "\\initialized(&d)",0,
                                 __gen_e_acsl_initialized);
    if (__gen_e_acsl_initialized) {
      int __gen_e_acsl_valid;
      __gen_e_acsl_valid = __e_acsl_valid((void *)d,sizeof(char),(void *)d,
                                          (void *)(& d));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"d",(void *)d);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                     "sizeof(char)",0,sizeof(char));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"\\valid(d)",0,
                                   __gen_e_acsl_valid);
      __gen_e_acsl_and = __gen_e_acsl_valid;
    }
    else __gen_e_acsl_and = 0;
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "\\valid(d)";
    __gen_e_acsl_assert_data.file = "ctype_macros.c";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 39;
    __e_acsl_assert(__gen_e_acsl_and,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert \valid(d); */ ;
  __retres = 0;
  __e_acsl_delete_block((void *)(& d));
  __e_acsl_delete_block((void *)(& c));
  __e_acsl_memory_clean();
  return __retres;
}

/*@ requires c_uchar_or_eof: (0 <= c <= 255) || c == -1;
    assigns \result;
    assigns \result \from c;
    
    behavior definitely_match:
      assumes c_upper: 'A' <= c <= 'Z';
      ensures nonzero_result: \result < 0 || \result > 0;
    
    behavior definitely_not_match:
      assumes c_non_upper: c == -1 || (0 <= c < 'A') || ('Z' < c <= 127);
      ensures zero_result: \result == 0;
    
    disjoint behaviors definitely_not_match, definitely_match;
 */
int __gen_e_acsl_isupper(int c)
{
  __e_acsl_contract_t *__gen_e_acsl_contract;
  int __retres;
  {
    int __gen_e_acsl_and;
    int __gen_e_acsl_or;
    int __gen_e_acsl_and_2;
    int __gen_e_acsl_or_2;
    int __gen_e_acsl_or_3;
    int __gen_e_acsl_active_bhvrs;
    __gen_e_acsl_contract = __e_acsl_contract_init(2UL);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"c",0,c);
    if (0 <= c) {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"c",0,c);
      __gen_e_acsl_and = c <= 255;
    }
    else __gen_e_acsl_and = 0;
    if (__gen_e_acsl_and) __gen_e_acsl_or = 1;
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"c",0,c);
      __gen_e_acsl_or = c == -1;
    }
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Precondition";
    __gen_e_acsl_assert_data.pred_txt = "(0 <= c <= 255) || c == -1";
    __gen_e_acsl_assert_data.file = "FRAMAC_SHARE/libc/ctype.h";
    __gen_e_acsl_assert_data.fct = "isupper";
    __gen_e_acsl_assert_data.line = 174;
    __gen_e_acsl_assert_data.name = "c_uchar_or_eof";
    __e_acsl_assert(__gen_e_acsl_or,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    if (65 <= c) __gen_e_acsl_and_2 = c <= 90; else __gen_e_acsl_and_2 = 0;
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,0UL,
                                           __gen_e_acsl_and_2);
    if (c == -1) __gen_e_acsl_or_2 = 1;
    else {
      int __gen_e_acsl_and_3;
      if (0 <= c) __gen_e_acsl_and_3 = c < 65; else __gen_e_acsl_and_3 = 0;
      __gen_e_acsl_or_2 = __gen_e_acsl_and_3;
    }
    if (__gen_e_acsl_or_2) __gen_e_acsl_or_3 = 1;
    else {
      int __gen_e_acsl_and_4;
      if (90 < c) __gen_e_acsl_and_4 = c <= 127; else __gen_e_acsl_and_4 = 0;
      __gen_e_acsl_or_3 = __gen_e_acsl_and_4;
    }
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,1UL,
                                           __gen_e_acsl_or_3);
    __gen_e_acsl_active_bhvrs = __e_acsl_contract_partial_count_all_behaviors
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
      {.values = (void *)0};
    __gen_e_acsl_assert_data_3.blocking = 1;
    __gen_e_acsl_assert_data_3.kind = "Precondition";
    __gen_e_acsl_assert_data_3.pred_txt = "all behaviors disjoint";
    __gen_e_acsl_assert_data_3.file = "FRAMAC_SHARE/libc/ctype.h";
    __gen_e_acsl_assert_data_3.fct = "isupper";
    __gen_e_acsl_assert_data_3.line = 173;
    __e_acsl_assert(__gen_e_acsl_active_bhvrs <= 1,
                    & __gen_e_acsl_assert_data_3);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
  }
  __retres = isupper(c);
  {
    int __gen_e_acsl_assumes_value;
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,0UL);
    if (__gen_e_acsl_assumes_value) {
      int __gen_e_acsl_or_4;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,"\\result",0,
                                   __retres);
      if (__retres < 0) __gen_e_acsl_or_4 = 1;
      else {
        __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,"\\result",
                                     0,__retres);
        __gen_e_acsl_or_4 = __retres > 0;
      }
      __gen_e_acsl_assert_data_4.blocking = 1;
      __gen_e_acsl_assert_data_4.kind = "Postcondition";
      __gen_e_acsl_assert_data_4.pred_txt = "\\result < 0 || \\result > 0";
      __gen_e_acsl_assert_data_4.file = "FRAMAC_SHARE/libc/ctype.h";
      __gen_e_acsl_assert_data_4.fct = "isupper";
      __gen_e_acsl_assert_data_4.line = 178;
      __gen_e_acsl_assert_data_4.name = "definitely_match/nonzero_result";
      __e_acsl_assert(__gen_e_acsl_or_4,& __gen_e_acsl_assert_data_4);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
    }
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,1UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"\\result",0,
                                   __retres);
      __gen_e_acsl_assert_data_5.blocking = 1;
      __gen_e_acsl_assert_data_5.kind = "Postcondition";
      __gen_e_acsl_assert_data_5.pred_txt = "\\result == 0";
      __gen_e_acsl_assert_data_5.file = "FRAMAC_SHARE/libc/ctype.h";
      __gen_e_acsl_assert_data_5.fct = "isupper";
      __gen_e_acsl_assert_data_5.line = 181;
      __gen_e_acsl_assert_data_5.name = "definitely_not_match/zero_result";
      __e_acsl_assert(__retres == 0,& __gen_e_acsl_assert_data_5);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
    }
    __e_acsl_contract_clean(__gen_e_acsl_contract);
    return __retres;
  }
}


