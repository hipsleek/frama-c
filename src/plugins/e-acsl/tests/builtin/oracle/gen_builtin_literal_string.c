/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "string.h"
#include "time.h"
char *__gen_e_acsl_literal_string;
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

void __e_acsl_globals_init(void)
{
  static char __e_acsl_already_run = 0;
  if (! __e_acsl_already_run) {
    __e_acsl_already_run = 1;
    __gen_e_acsl_literal_string = "a cow";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string,sizeof("a cow"));
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
  __e_acsl_builtin_strlen(__gen_e_acsl_literal_string);
  __retres = 0;
  __e_acsl_memory_clean();
  return __retres;
}


