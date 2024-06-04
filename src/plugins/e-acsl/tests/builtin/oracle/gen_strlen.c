/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "sys/select.h"
#include "sys/time.h"
#include "sys/types.h"
#include "sys/wait.h"
#include "time.h"
#include "unistd.h"
char *__gen_e_acsl_literal_string_8;
char *__gen_e_acsl_literal_string_7;
char *__gen_e_acsl_literal_string_15;
char *__gen_e_acsl_literal_string_14;
char *__gen_e_acsl_literal_string_13;
char *__gen_e_acsl_literal_string_12;
char *__gen_e_acsl_literal_string_11;
char *__gen_e_acsl_literal_string_10;
char *__gen_e_acsl_literal_string_9;
char *__gen_e_acsl_literal_string;
char *__gen_e_acsl_literal_string_2;
char *__gen_e_acsl_literal_string_3;
char *__gen_e_acsl_literal_string_5;
char *__gen_e_acsl_literal_string_4;
char *__gen_e_acsl_literal_string_6;
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

/*@ exits status: \exit_status != 0;
    ensures never_terminates: \false;
    
    assigns \exit_status \from \nothing;
 */
void __gen_e_acsl_abort(void);

/*@ exits status: \exit_status == \old(status);
    ensures never_terminates: \false;
    
    assigns \exit_status \from status;
 */
void __gen_e_acsl_exit(int status);

/*@ ensures result_ok_or_error: \result == -1 || \result >= 0;
    ensures
      initialization: stat_loc_init_on_success:
        \result >= 0 && \old(stat_loc) != \null ==>
        \initialized(\old(stat_loc));
    assigns \result, *stat_loc;
    assigns \result \from (indirect: options);
    assigns *stat_loc \from (indirect: options);
    
    behavior stat_loc_null:
      assumes stat_loc_null: stat_loc == \null;
      assigns \result;
      assigns \result \from \nothing;
    
    behavior stat_loc_non_null:
      assumes stat_loc_non_null: stat_loc != \null;
      requires valid_stat_loc: \valid(stat_loc);
 */
pid_t __gen_e_acsl_waitpid(pid_t pid, int *stat_loc, int options);

/*@ ensures
      result_ok_child_or_error: \result == 0 || \result > 0 || \result == -1;
    assigns \result;
    assigns \result \from \nothing;
 */
pid_t __gen_e_acsl_fork(void);

/*@ requires valid_string_s: valid_read_string(s);
    assigns \result;
    assigns \result
      \from (indirect: *(s + (0 .. strlen{Old}(s)))),
            (indirect: __fc_heap_status);
    allocates \result;
    
    behavior allocation:
      assumes can_allocate: is_allocable(strlen(s) + 1);
      ensures allocation: \fresh{Old, Here}(\result,strlen(\old(s)) + 1);
      ensures
        result_valid_string_and_same_contents:
          valid_string(\result) && strcmp(\result, \old(s)) == 0;
      assigns __fc_heap_status, \result;
      assigns __fc_heap_status
        \from __fc_heap_status, (indirect: *(s + (0 .. strlen{Old}(s))));
      assigns \result
        \from (indirect: *(s + (0 .. strlen{Old}(s)))),
              (indirect: __fc_heap_status);
    
    behavior no_allocation:
      assumes cannot_allocate: !is_allocable(strlen(s) + 1);
      ensures result_null: \result == \null;
      assigns \result;
      assigns \result \from \nothing;
      allocates \nothing;
 */
char *__gen_e_acsl_strdup(char const *s);

/*@ requires valid_string_s: valid_read_string(s);
    assigns \result;
    assigns \result
      \from (indirect: *(s + (0 .. strlen{Old}(s)))),
            (indirect: __fc_heap_status);
    allocates \result;
    
    behavior allocation:
      assumes can_allocate: is_allocable(strlen(s) + 1);
      ensures allocation: \fresh{Old, Here}(\result,strlen(\old(s)) + 1);
      ensures
        result_valid_string_and_same_contents:
          valid_string(\result) && strcmp(\result, \old(s)) == 0;
      assigns __fc_heap_status, \result;
      assigns __fc_heap_status
        \from __fc_heap_status, (indirect: *(s + (0 .. strlen{Old}(s))));
      assigns \result
        \from (indirect: *(s + (0 .. strlen{Old}(s)))),
              (indirect: __fc_heap_status);
    
    behavior no_allocation:
      assumes cannot_allocate: !is_allocable(strlen(s) + 1);
      ensures result_null: \result == \null;
      assigns \result;
      assigns \result \from \nothing;
      allocates \nothing;
 */
char *__gen_e_acsl_strdup(char const *s)
{
  __e_acsl_contract_t *__gen_e_acsl_contract;
  char *__retres;
  __gen_e_acsl_contract = __e_acsl_contract_init(2UL);
  __retres = strdup(s);
  {
    int __gen_e_acsl_assumes_value;
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,1UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
        {.values = (void *)0};
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"\\result",
                                   (void *)__retres);
      __gen_e_acsl_assert_data_4.blocking = 1;
      __gen_e_acsl_assert_data_4.kind = "Postcondition";
      __gen_e_acsl_assert_data_4.pred_txt = "\\result == \\null";
      __gen_e_acsl_assert_data_4.file = "FRAMAC_SHARE/libc/string.h";
      __gen_e_acsl_assert_data_4.fct = "strdup";
      __gen_e_acsl_assert_data_4.line = 580;
      __gen_e_acsl_assert_data_4.name = "no_allocation/result_null";
      __e_acsl_assert(__retres == (char *)0,& __gen_e_acsl_assert_data_4);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
    }
    __e_acsl_contract_clean(__gen_e_acsl_contract);
    return __retres;
  }
}

/*@ ensures
      result_ok_child_or_error: \result == 0 || \result > 0 || \result == -1;
    assigns \result;
    assigns \result \from \nothing;
 */
pid_t __gen_e_acsl_fork(void)
{
  pid_t __retres;
  __retres = fork();
  {
    int __gen_e_acsl_or;
    int __gen_e_acsl_or_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"\\result",0,
                                 __retres);
    if (__retres == 0) __gen_e_acsl_or = 1;
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"\\result",0,
                                   __retres);
      __gen_e_acsl_or = __retres > 0;
    }
    if (__gen_e_acsl_or) __gen_e_acsl_or_2 = 1;
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"\\result",0,
                                   __retres);
      __gen_e_acsl_or_2 = __retres == -1;
    }
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Postcondition";
    __gen_e_acsl_assert_data.pred_txt = "\\result == 0 || \\result > 0 || \\result == -1";
    __gen_e_acsl_assert_data.file = "FRAMAC_SHARE/libc/unistd.h";
    __gen_e_acsl_assert_data.fct = "fork";
    __gen_e_acsl_assert_data.line = 853;
    __gen_e_acsl_assert_data.name = "result_ok_child_or_error";
    __e_acsl_assert(__gen_e_acsl_or_2,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    return __retres;
  }
}

/*@ ensures result_ok_or_error: \result == -1 || \result >= 0;
    ensures
      initialization: stat_loc_init_on_success:
        \result >= 0 && \old(stat_loc) != \null ==>
        \initialized(\old(stat_loc));
    assigns \result, *stat_loc;
    assigns \result \from (indirect: options);
    assigns *stat_loc \from (indirect: options);
    
    behavior stat_loc_null:
      assumes stat_loc_null: stat_loc == \null;
      assigns \result;
      assigns \result \from \nothing;
    
    behavior stat_loc_non_null:
      assumes stat_loc_non_null: stat_loc != \null;
      requires valid_stat_loc: \valid(stat_loc);
 */
pid_t __gen_e_acsl_waitpid(pid_t pid, int *stat_loc, int options)
{
  __e_acsl_contract_t *__gen_e_acsl_contract;
  int *__gen_e_acsl_at;
  pid_t __retres;
  {
    int __gen_e_acsl_assumes_value;
    __e_acsl_store_block((void *)(& stat_loc),8UL);
    __gen_e_acsl_at = stat_loc;
    __gen_e_acsl_contract = __e_acsl_contract_init(2UL);
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,0UL,
                                           stat_loc == (int *)0);
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,1UL,
                                           stat_loc != (int *)0);
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,1UL);
    if (__gen_e_acsl_assumes_value) {
      int __gen_e_acsl_valid;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data =
        {.values = (void *)0};
      __gen_e_acsl_valid = __e_acsl_valid((void *)stat_loc,sizeof(int),
                                          (void *)stat_loc,
                                          (void *)(& stat_loc));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"stat_loc",
                                   (void *)stat_loc);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,
                                   "\\valid(stat_loc)",0,__gen_e_acsl_valid);
      __gen_e_acsl_assert_data.blocking = 1;
      __gen_e_acsl_assert_data.kind = "Precondition";
      __gen_e_acsl_assert_data.pred_txt = "\\valid(stat_loc)";
      __gen_e_acsl_assert_data.file = "FRAMAC_SHARE/libc/sys/wait.h";
      __gen_e_acsl_assert_data.fct = "waitpid";
      __gen_e_acsl_assert_data.line = 95;
      __gen_e_acsl_assert_data.name = "stat_loc_non_null/valid_stat_loc";
      __e_acsl_assert(__gen_e_acsl_valid,& __gen_e_acsl_assert_data);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    }
  }
  __retres = waitpid(pid,stat_loc,options);
  {
    int __gen_e_acsl_or;
    int __gen_e_acsl_and;
    int __gen_e_acsl_implies;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"\\result",0,
                                 __retres);
    if (__retres == -1) __gen_e_acsl_or = 1;
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"\\result",0,
                                   __retres);
      __gen_e_acsl_or = __retres >= 0;
    }
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Postcondition";
    __gen_e_acsl_assert_data_2.pred_txt = "\\result == -1 || \\result >= 0";
    __gen_e_acsl_assert_data_2.file = "FRAMAC_SHARE/libc/sys/wait.h";
    __gen_e_acsl_assert_data_2.fct = "waitpid";
    __gen_e_acsl_assert_data_2.line = 87;
    __gen_e_acsl_assert_data_2.name = "result_ok_or_error";
    __e_acsl_assert(__gen_e_acsl_or,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"\\result",0,
                                 __retres);
    if (__retres >= 0) {
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,
                                   "\\old(stat_loc)",(void *)__gen_e_acsl_at);
      __gen_e_acsl_and = __gen_e_acsl_at != (int *)0;
    }
    else __gen_e_acsl_and = 0;
    if (! __gen_e_acsl_and) __gen_e_acsl_implies = 1;
    else {
      int __gen_e_acsl_initialized;
      __gen_e_acsl_initialized = __e_acsl_initialized((void *)__gen_e_acsl_at,
                                                      sizeof(int));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,
                                   "\\old(stat_loc)",(void *)__gen_e_acsl_at);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_3,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,
                                   "\\initialized(\\old(stat_loc))",0,
                                   __gen_e_acsl_initialized);
      __gen_e_acsl_implies = __gen_e_acsl_initialized;
    }
    __gen_e_acsl_assert_data_3.blocking = 1;
    __gen_e_acsl_assert_data_3.kind = "Postcondition";
    __gen_e_acsl_assert_data_3.pred_txt = "\\result >= 0 && \\old(stat_loc) != \\null ==> \\initialized(\\old(stat_loc))";
    __gen_e_acsl_assert_data_3.file = "FRAMAC_SHARE/libc/sys/wait.h";
    __gen_e_acsl_assert_data_3.fct = "waitpid";
    __gen_e_acsl_assert_data_3.line = 89;
    __gen_e_acsl_assert_data_3.name = "initialization/stat_loc_init_on_success";
    __e_acsl_assert(__gen_e_acsl_implies,& __gen_e_acsl_assert_data_3);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
    __e_acsl_contract_clean(__gen_e_acsl_contract);
    __e_acsl_delete_block((void *)(& stat_loc));
    return __retres;
  }
}

/*@ exits status: \exit_status == \old(status);
    ensures never_terminates: \false;
    
    assigns \exit_status \from status;
 */
void __gen_e_acsl_exit(int status)
{
  exit(status);
  {
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Postcondition";
    __gen_e_acsl_assert_data.pred_txt = "\\false";
    __gen_e_acsl_assert_data.file = "FRAMAC_SHARE/libc/stdlib.h";
    __gen_e_acsl_assert_data.fct = "exit";
    __gen_e_acsl_assert_data.line = 541;
    __gen_e_acsl_assert_data.name = "never_terminates";
    __e_acsl_assert(0,& __gen_e_acsl_assert_data);
    return;
  }
}

/*@ exits status: \exit_status != 0;
    ensures never_terminates: \false;
    
    assigns \exit_status \from \nothing;
 */
void __gen_e_acsl_abort(void)
{
  abort();
  {
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Postcondition";
    __gen_e_acsl_assert_data.pred_txt = "\\false";
    __gen_e_acsl_assert_data.file = "FRAMAC_SHARE/libc/stdlib.h";
    __gen_e_acsl_assert_data.fct = "abort";
    __gen_e_acsl_assert_data.line = 528;
    __gen_e_acsl_assert_data.name = "never_terminates";
    __e_acsl_assert(0,& __gen_e_acsl_assert_data);
    return;
  }
}

void __e_acsl_globals_init(void)
{
  static char __e_acsl_already_run = 0;
  if (! __e_acsl_already_run) {
    __e_acsl_already_run = 1;
    __gen_e_acsl_literal_string_8 = "the hog";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string_8,
                         sizeof("the hog"));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string_8);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string_8);
    __gen_e_acsl_literal_string_7 = "the cat";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string_7,
                         sizeof("the cat"));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string_7);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string_7);
    __gen_e_acsl_literal_string_15 = "strlen.c:40";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string_15,
                         sizeof("strlen.c:40"));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string_15);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string_15);
    __gen_e_acsl_literal_string_14 = "strlen.c:38";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string_14,
                         sizeof("strlen.c:38"));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string_14);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string_14);
    __gen_e_acsl_literal_string_13 = "strlen.c:36";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string_13,
                         sizeof("strlen.c:36"));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string_13);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string_13);
    __gen_e_acsl_literal_string_12 = "strlen.c:31";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string_12,
                         sizeof("strlen.c:31"));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string_12);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string_12);
    __gen_e_acsl_literal_string_11 = "strlen.c:30";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string_11,
                         sizeof("strlen.c:30"));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string_11);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string_11);
    __gen_e_acsl_literal_string_10 = "strlen.c:29";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string_10,
                         sizeof("strlen.c:29"));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string_10);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string_10);
    __gen_e_acsl_literal_string_9 = "strlen.c:28";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string_9,
                         sizeof("strlen.c:28"));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string_9);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string_9);
    __gen_e_acsl_literal_string = "TEST %d: ";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string,
                         sizeof("TEST %d: "));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string);
    __gen_e_acsl_literal_string_2 = "OK: Expected signal at %s\n";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string_2,
                         sizeof("OK: Expected signal at %s\n"));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string_2);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string_2);
    __gen_e_acsl_literal_string_3 = "OK: Expected execution at %s\n";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string_3,
                         sizeof("OK: Expected execution at %s\n"));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string_3);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string_3);
    __gen_e_acsl_literal_string_5 = "FAIL: Unexpected signal at %s\n";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string_5,
                         sizeof("FAIL: Unexpected signal at %s\n"));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string_5);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string_5);
    __gen_e_acsl_literal_string_4 = "FAIL: Unexpected execution at %s\n";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string_4,
                         sizeof("FAIL: Unexpected execution at %s\n"));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string_4);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string_4);
    __gen_e_acsl_literal_string_6 = "";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string_6,sizeof(""));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string_6);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string_6);
  }
  return;
}

int main(int argc, char const **argv)
{
  int __retres;
  int len;
  __e_acsl_memory_init(& argc,(char ***)(& argv),8UL);
  __e_acsl_globals_init();
  char *empty_str = (char *)__gen_e_acsl_literal_string_6;
  char *heap_str = __gen_e_acsl_strdup(__gen_e_acsl_literal_string_7);
  char stack_str[8] =
    {(char)'t',
     (char)'h',
     (char)'e',
     (char)' ',
     (char)'d',
     (char)'o',
     (char)'g',
     (char)'\000'};
  char *const_str = (char *)__gen_e_acsl_literal_string_8;
  {
    pid_t pid = __gen_e_acsl_fork();
    if (! pid) {
      size_t tmp_1;
      tmp_1 = __e_acsl_builtin_strlen((char const *)empty_str);
      len = tmp_1 != (size_t)0;
      if (len) __gen_e_acsl_abort();
      __gen_e_acsl_exit(0);
    }
    else {
      int process_status;
      __e_acsl_store_block((void *)(& process_status),4UL);
      __gen_e_acsl_waitpid(pid,& process_status,0);
      signal_eval(process_status,0,__gen_e_acsl_literal_string_9);
      __e_acsl_delete_block((void *)(& process_status));
    }
  }
  {
    pid_t pid_0 = __gen_e_acsl_fork();
    if (! pid_0) {
      size_t tmp_3;
      tmp_3 = __e_acsl_builtin_strlen((char const *)heap_str);
      len = tmp_3 != (size_t)7;
      if (len) __gen_e_acsl_abort();
      __gen_e_acsl_exit(0);
    }
    else {
      int process_status_0;
      __e_acsl_store_block((void *)(& process_status_0),4UL);
      __gen_e_acsl_waitpid(pid_0,& process_status_0,0);
      signal_eval(process_status_0,0,__gen_e_acsl_literal_string_10);
      __e_acsl_delete_block((void *)(& process_status_0));
    }
  }
  {
    pid_t pid_1 = __gen_e_acsl_fork();
    if (! pid_1) {
      size_t tmp_5;
      tmp_5 = __e_acsl_builtin_strlen((char const *)(stack_str));
      len = tmp_5 != (size_t)7;
      if (len) __gen_e_acsl_abort();
      __gen_e_acsl_exit(0);
    }
    else {
      int process_status_1;
      __e_acsl_store_block((void *)(& process_status_1),4UL);
      __gen_e_acsl_waitpid(pid_1,& process_status_1,0);
      signal_eval(process_status_1,0,__gen_e_acsl_literal_string_11);
      __e_acsl_delete_block((void *)(& process_status_1));
    }
  }
  {
    pid_t pid_2 = __gen_e_acsl_fork();
    if (! pid_2) {
      size_t tmp_7;
      tmp_7 = __e_acsl_builtin_strlen((char const *)const_str);
      len = tmp_7 != (size_t)7;
      if (len) __gen_e_acsl_abort();
      __gen_e_acsl_exit(0);
    }
    else {
      int process_status_2;
      __e_acsl_store_block((void *)(& process_status_2),4UL);
      __gen_e_acsl_waitpid(pid_2,& process_status_2,0);
      signal_eval(process_status_2,0,__gen_e_acsl_literal_string_12);
      __e_acsl_delete_block((void *)(& process_status_2));
    }
  }
  *(heap_str + 7) = (char)'a';
  stack_str[7] = (char)'a';
  {
    pid_t pid_3 = __gen_e_acsl_fork();
    if (! pid_3) {
      size_t tmp_9;
      tmp_9 = __e_acsl_builtin_strlen((char const *)heap_str);
      len = tmp_9 != (size_t)7;
      if (len) __gen_e_acsl_abort();
      __gen_e_acsl_exit(0);
    }
    else {
      int process_status_3;
      __e_acsl_store_block((void *)(& process_status_3),4UL);
      __gen_e_acsl_waitpid(pid_3,& process_status_3,0);
      signal_eval(process_status_3,1,__gen_e_acsl_literal_string_13);
      __e_acsl_delete_block((void *)(& process_status_3));
    }
  }
  {
    pid_t pid_4 = __gen_e_acsl_fork();
    if (! pid_4) {
      size_t tmp_11;
      tmp_11 = __e_acsl_builtin_strlen((char const *)(stack_str));
      len = tmp_11 != (size_t)7;
      if (len) __gen_e_acsl_abort();
      __gen_e_acsl_exit(0);
    }
    else {
      int process_status_4;
      __e_acsl_store_block((void *)(& process_status_4),4UL);
      __gen_e_acsl_waitpid(pid_4,& process_status_4,0);
      signal_eval(process_status_4,1,__gen_e_acsl_literal_string_14);
      __e_acsl_delete_block((void *)(& process_status_4));
    }
  }
  free((void *)heap_str);
  {
    pid_t pid_5 = __gen_e_acsl_fork();
    if (! pid_5) {
      size_t tmp_13;
      tmp_13 = __e_acsl_builtin_strlen((char const *)heap_str);
      len = tmp_13 != (size_t)7;
      if (len) __gen_e_acsl_abort();
      __gen_e_acsl_exit(0);
    }
    else {
      int process_status_5;
      __e_acsl_store_block((void *)(& process_status_5),4UL);
      __gen_e_acsl_waitpid(pid_5,& process_status_5,0);
      signal_eval(process_status_5,1,__gen_e_acsl_literal_string_15);
      __e_acsl_delete_block((void *)(& process_status_5));
    }
  }
  __retres = 0;
  __e_acsl_memory_clean();
  return __retres;
}


