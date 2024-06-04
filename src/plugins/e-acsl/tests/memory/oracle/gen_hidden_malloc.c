/* Generated by Frama-C */
#include "errno.h"
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "time.h"
char *__gen_e_acsl_literal_string;
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

/*@ requires
      valid_file_name_or_null:
        file_name == \null || valid_read_string(file_name);
    requires
      resolved_name_null_or_allocated:
        resolved_name == \null || \valid(resolved_name + (0 .. 4096 - 1));
    assigns __fc_heap_status, *(\result + (0 .. 4096 - 1)),
            *(resolved_name + (0 .. 4096 - 1)), __fc_errno;
    assigns __fc_heap_status
      \from (indirect: resolved_name), __fc_heap_status;
    assigns *(\result + (0 .. 4096 - 1))
      \from (indirect: __fc_heap_status), (indirect: resolved_name),
            (indirect: *(file_name + (0 ..)));
    assigns *(resolved_name + (0 .. 4096 - 1))
      \from (indirect: __fc_heap_status), (indirect: resolved_name),
            (indirect: *(file_name + (0 ..)));
    assigns __fc_errno
      \from (indirect: file_name), (indirect: *(file_name + (0 ..))),
            (indirect: resolved_name), (indirect: __fc_heap_status);
    
    behavior null_file_name:
      assumes null_file_name: file_name == \null;
      ensures null_result: \result == \null;
      ensures errno_set: __fc_errno == 22;
      assigns \result, __fc_errno;
      assigns \result \from (indirect: file_name);
      assigns __fc_errno \from (indirect: file_name);
    
    behavior allocate_resolved_name:
      assumes valid_file_name: valid_read_string(file_name);
      assumes resolved_name_null: resolved_name == \null;
      assumes can_allocate: is_allocable(4096);
      ensures allocation: \fresh{Old, Here}(\result,4096);
      assigns __fc_heap_status, \result;
      assigns __fc_heap_status \from __fc_heap_status;
      assigns \result \from (indirect: __fc_heap_status);
    
    behavior not_enough_memory:
      assumes valid_file_name: valid_read_string(file_name);
      assumes resolved_name_null: resolved_name == \null;
      assumes cannot_allocate: !is_allocable(4096);
      ensures null_result: \result == \null;
      ensures errno_set: __fc_errno == 12;
      assigns \result;
      assigns \result \from \nothing;
      allocates \nothing;
    
    behavior resolved_name_buffer:
      assumes valid_file_name: valid_read_string(file_name);
      assumes
        allocated_resolved_name_or_fail:
          \valid(resolved_name + (0 .. 4096 - 1));
      ensures
        valid_string_resolved_name:
          valid_string(\old(resolved_name)) &&
          strlen(\old(resolved_name)) < 4096;
      ensures resolved_result: \result == \old(resolved_name);
      assigns \result, *(resolved_name + (0 .. 4096 - 1));
      assigns \result \from resolved_name;
      assigns *(resolved_name + (0 .. 4096 - 1))
        \from (indirect: *(file_name + (0 ..)));
      allocates \nothing;
    
    behavior filesystem_error:
      assumes valid_file_name: valid_read_string(file_name);
      ensures null_result: \result == \null;
      ensures errno_set: __fc_errno \in {13, 5, 40, 36, 2, 20};
      assigns \result, __fc_errno;
      assigns \result \from (indirect: *(file_name + (0 ..)));
      assigns __fc_errno \from (indirect: *(file_name + (0 ..)));
      allocates \nothing;
 */
char *__gen_e_acsl_realpath(char const * restrict file_name,
                            char * restrict resolved_name);

/*@ requires
      valid_file_name_or_null:
        file_name == \null || valid_read_string(file_name);
    requires
      resolved_name_null_or_allocated:
        resolved_name == \null || \valid(resolved_name + (0 .. 4096 - 1));
    assigns __fc_heap_status, *(\result + (0 .. 4096 - 1)),
            *(resolved_name + (0 .. 4096 - 1)), __fc_errno;
    assigns __fc_heap_status
      \from (indirect: resolved_name), __fc_heap_status;
    assigns *(\result + (0 .. 4096 - 1))
      \from (indirect: __fc_heap_status), (indirect: resolved_name),
            (indirect: *(file_name + (0 ..)));
    assigns *(resolved_name + (0 .. 4096 - 1))
      \from (indirect: __fc_heap_status), (indirect: resolved_name),
            (indirect: *(file_name + (0 ..)));
    assigns __fc_errno
      \from (indirect: file_name), (indirect: *(file_name + (0 ..))),
            (indirect: resolved_name), (indirect: __fc_heap_status);
    
    behavior null_file_name:
      assumes null_file_name: file_name == \null;
      ensures null_result: \result == \null;
      ensures errno_set: __fc_errno == 22;
      assigns \result, __fc_errno;
      assigns \result \from (indirect: file_name);
      assigns __fc_errno \from (indirect: file_name);
    
    behavior allocate_resolved_name:
      assumes valid_file_name: valid_read_string(file_name);
      assumes resolved_name_null: resolved_name == \null;
      assumes can_allocate: is_allocable(4096);
      ensures allocation: \fresh{Old, Here}(\result,4096);
      assigns __fc_heap_status, \result;
      assigns __fc_heap_status \from __fc_heap_status;
      assigns \result \from (indirect: __fc_heap_status);
    
    behavior not_enough_memory:
      assumes valid_file_name: valid_read_string(file_name);
      assumes resolved_name_null: resolved_name == \null;
      assumes cannot_allocate: !is_allocable(4096);
      ensures null_result: \result == \null;
      ensures errno_set: __fc_errno == 12;
      assigns \result;
      assigns \result \from \nothing;
      allocates \nothing;
    
    behavior resolved_name_buffer:
      assumes valid_file_name: valid_read_string(file_name);
      assumes
        allocated_resolved_name_or_fail:
          \valid(resolved_name + (0 .. 4096 - 1));
      ensures
        valid_string_resolved_name:
          valid_string(\old(resolved_name)) &&
          strlen(\old(resolved_name)) < 4096;
      ensures resolved_result: \result == \old(resolved_name);
      assigns \result, *(resolved_name + (0 .. 4096 - 1));
      assigns \result \from resolved_name;
      assigns *(resolved_name + (0 .. 4096 - 1))
        \from (indirect: *(file_name + (0 ..)));
      allocates \nothing;
    
    behavior filesystem_error:
      assumes valid_file_name: valid_read_string(file_name);
      ensures null_result: \result == \null;
      ensures errno_set: __fc_errno \in {13, 5, 40, 36, 2, 20};
      assigns \result, __fc_errno;
      assigns \result \from (indirect: *(file_name + (0 ..)));
      assigns __fc_errno \from (indirect: *(file_name + (0 ..)));
      allocates \nothing;
 */
char *__gen_e_acsl_realpath(char const * restrict file_name,
                            char * restrict resolved_name)
{
  __e_acsl_contract_t *__gen_e_acsl_contract;
  char *__gen_e_acsl_at;
  char *__retres;
  {
    int __gen_e_acsl_or;
    __e_acsl_store_block((void *)(& resolved_name),8UL);
    __gen_e_acsl_at = resolved_name;
    __gen_e_acsl_contract = __e_acsl_contract_init(5UL);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,
                                 "resolved_name",(void *)resolved_name);
    if (resolved_name == (char *)0) __gen_e_acsl_or = 1;
    else {
      int __gen_e_acsl_size;
      int __gen_e_acsl_if;
      int __gen_e_acsl_valid;
      __gen_e_acsl_size = 1 * (((4096 - 1) - 0) + 1);
      if (__gen_e_acsl_size <= 0) __gen_e_acsl_if = 0;
      else __gen_e_acsl_if = __gen_e_acsl_size;
      __gen_e_acsl_valid = __e_acsl_valid((void *)(resolved_name + 1 * 0),
                                          (size_t)__gen_e_acsl_if,
                                          (void *)resolved_name,
                                          (void *)(& resolved_name));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,
                                   "resolved_name",(void *)resolved_name);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                   "sizeof(char)",0,1);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                   "sizeof(char)",0,1);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"size",0,
                                   __gen_e_acsl_size);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"size",0,
                                   __gen_e_acsl_size);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                   "\\valid(resolved_name + (0 .. 4096 - 1))",
                                   0,__gen_e_acsl_valid);
      __gen_e_acsl_or = __gen_e_acsl_valid;
    }
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Precondition";
    __gen_e_acsl_assert_data_2.pred_txt = "resolved_name == \\null || \\valid(resolved_name + (0 .. 4096 - 1))";
    __gen_e_acsl_assert_data_2.file = "FRAMAC_SHARE/libc/stdlib.h";
    __gen_e_acsl_assert_data_2.fct = "realpath";
    __gen_e_acsl_assert_data_2.line = 842;
    __gen_e_acsl_assert_data_2.name = "resolved_name_null_or_allocated";
    __e_acsl_assert(__gen_e_acsl_or,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,0UL,
                                           file_name == (char const *)0);
  }
  __retres = realpath(file_name,resolved_name);
  {
    int __gen_e_acsl_assumes_value;
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,0UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
        {.values = (void *)0};
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,"\\result",
                                   (void *)__retres);
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,"__fc_errno",
                                   0,errno);
      __gen_e_acsl_assert_data_4.blocking = 1;
      __gen_e_acsl_assert_data_4.kind = "Postcondition";
      __gen_e_acsl_assert_data_4.pred_txt = "__fc_errno == 22";
      __gen_e_acsl_assert_data_4.file = "FRAMAC_SHARE/libc/stdlib.h";
      __gen_e_acsl_assert_data_4.fct = "realpath";
      __gen_e_acsl_assert_data_4.line = 853;
      __gen_e_acsl_assert_data_4.name = "null_file_name/errno_set";
      __e_acsl_assert(errno == 22,& __gen_e_acsl_assert_data_4);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
      __gen_e_acsl_assert_data_3.blocking = 1;
      __gen_e_acsl_assert_data_3.kind = "Postcondition";
      __gen_e_acsl_assert_data_3.pred_txt = "\\result == \\null";
      __gen_e_acsl_assert_data_3.file = "FRAMAC_SHARE/libc/stdlib.h";
      __gen_e_acsl_assert_data_3.fct = "realpath";
      __gen_e_acsl_assert_data_3.line = 852;
      __gen_e_acsl_assert_data_3.name = "null_file_name/null_result";
      __e_acsl_assert(__retres == (char *)0,& __gen_e_acsl_assert_data_3);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
    }
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,2UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_6 =
        {.values = (void *)0};
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_6,"\\result",
                                   (void *)__retres);
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_7 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_7,"__fc_errno",
                                   0,errno);
      __gen_e_acsl_assert_data_7.blocking = 1;
      __gen_e_acsl_assert_data_7.kind = "Postcondition";
      __gen_e_acsl_assert_data_7.pred_txt = "__fc_errno == 12";
      __gen_e_acsl_assert_data_7.file = "FRAMAC_SHARE/libc/stdlib.h";
      __gen_e_acsl_assert_data_7.fct = "realpath";
      __gen_e_acsl_assert_data_7.line = 868;
      __gen_e_acsl_assert_data_7.name = "not_enough_memory/errno_set";
      __e_acsl_assert(errno == 12,& __gen_e_acsl_assert_data_7);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_7);
      __gen_e_acsl_assert_data_6.blocking = 1;
      __gen_e_acsl_assert_data_6.kind = "Postcondition";
      __gen_e_acsl_assert_data_6.pred_txt = "\\result == \\null";
      __gen_e_acsl_assert_data_6.file = "FRAMAC_SHARE/libc/stdlib.h";
      __gen_e_acsl_assert_data_6.fct = "realpath";
      __gen_e_acsl_assert_data_6.line = 867;
      __gen_e_acsl_assert_data_6.name = "not_enough_memory/null_result";
      __e_acsl_assert(__retres == (char *)0,& __gen_e_acsl_assert_data_6);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_6);
    }
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,3UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_9 =
        {.values = (void *)0};
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_9,"\\result",
                                   (void *)__retres);
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_9,
                                   "\\old(resolved_name)",
                                   (void *)__gen_e_acsl_at);
      __gen_e_acsl_assert_data_9.blocking = 1;
      __gen_e_acsl_assert_data_9.kind = "Postcondition";
      __gen_e_acsl_assert_data_9.pred_txt = "\\result == \\old(resolved_name)";
      __gen_e_acsl_assert_data_9.file = "FRAMAC_SHARE/libc/stdlib.h";
      __gen_e_acsl_assert_data_9.fct = "realpath";
      __gen_e_acsl_assert_data_9.line = 880;
      __gen_e_acsl_assert_data_9.name = "resolved_name_buffer/resolved_result";
      __e_acsl_assert(__retres == __gen_e_acsl_at,
                      & __gen_e_acsl_assert_data_9);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_9);
    }
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,4UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_10 =
        {.values = (void *)0};
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_10,"\\result",
                                   (void *)__retres);
      __gen_e_acsl_assert_data_10.blocking = 1;
      __gen_e_acsl_assert_data_10.kind = "Postcondition";
      __gen_e_acsl_assert_data_10.pred_txt = "\\result == \\null";
      __gen_e_acsl_assert_data_10.file = "FRAMAC_SHARE/libc/stdlib.h";
      __gen_e_acsl_assert_data_10.fct = "realpath";
      __gen_e_acsl_assert_data_10.line = 886;
      __gen_e_acsl_assert_data_10.name = "filesystem_error/null_result";
      __e_acsl_assert(__retres == (char *)0,& __gen_e_acsl_assert_data_10);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_10);
    }
    __e_acsl_contract_clean(__gen_e_acsl_contract);
    __e_acsl_delete_block((void *)(& resolved_name));
    return __retres;
  }
}

void __e_acsl_globals_init(void)
{
  static char __e_acsl_already_run = 0;
  if (! __e_acsl_already_run) {
    __e_acsl_already_run = 1;
    __gen_e_acsl_literal_string = ".";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string,sizeof("."));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string);
  }
  return;
}

int main(int argc, char const **argv)
{
  int __retres;
  __e_acsl_memory_init(& argc,(char ***)(& argv),8UL);
  __e_acsl_globals_init();
  char *cwd = __gen_e_acsl_realpath(__gen_e_acsl_literal_string,(char *)0);
  __retres = 0;
  __e_acsl_memory_clean();
  return __retres;
}

