/* run.config*
 EXIT: 1
 DEPS: merge_unused.h
  OPT: -cpp-extra-args="-I./" %{dep:./@PTEST_NAME@_2.c} -print
*/
#pragma pack(1)

#include "merge_unused.h"

extern void f(void);

struct s G1;

struct s G3 = { 1 };

int main() {
  int i = G1.i;
  f();
  return G3.i;
}
