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

struct list {
   int element ;
   struct list *next ;
};
struct list *add(struct list *l, int i)
{
  struct list *new;
  __e_acsl_store_block((void *)(& new),8UL);
  __e_acsl_store_block((void *)(& l),8UL);
  __e_acsl_full_init((void *)(& new));
  new = (struct list *)malloc(sizeof(struct list));
  {
    int __gen_e_acsl_initialized;
    int __gen_e_acsl_and;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_initialized = __e_acsl_initialized((void *)(& new),
                                                    sizeof(struct list *));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"&new",
                                 (void *)(& new));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                   "sizeof(struct list *)",0,
                                   sizeof(struct list *));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,
                                 "\\initialized(&new)",0,
                                 __gen_e_acsl_initialized);
    if (__gen_e_acsl_initialized) {
      int __gen_e_acsl_valid;
      __gen_e_acsl_valid = __e_acsl_valid((void *)new,sizeof(struct list),
                                          (void *)new,(void *)(& new));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"new",
                                   (void *)new);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                     "sizeof(struct list)",0,
                                     sizeof(struct list));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"\\valid(new)",
                                   0,__gen_e_acsl_valid);
      __gen_e_acsl_and = __gen_e_acsl_valid;
    }
    else __gen_e_acsl_and = 0;
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "\\valid(new)";
    __gen_e_acsl_assert_data.file = "local_var.c";
    __gen_e_acsl_assert_data.fct = "add";
    __gen_e_acsl_assert_data.line = 15;
    __e_acsl_assert(__gen_e_acsl_and,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert \valid(new); */ ;
  __e_acsl_initialize((void *)(& new->element),sizeof(int));
  new->element = i;
  __e_acsl_initialize((void *)(& new->next),sizeof(struct list *));
  new->next = l;
  __e_acsl_delete_block((void *)(& l));
  __e_acsl_delete_block((void *)(& new));
  return new;
}

int main(void)
{
  int __retres;
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  struct list *l = (struct list *)0;
  __e_acsl_store_block((void *)(& l),8UL);
  __e_acsl_full_init((void *)(& l));
  __e_acsl_full_init((void *)(& l));
  l = add(l,4);
  __e_acsl_full_init((void *)(& l));
  l = add(l,7);
  __retres = 0;
  __e_acsl_delete_block((void *)(& l));
  __e_acsl_memory_clean();
  return __retres;
}


