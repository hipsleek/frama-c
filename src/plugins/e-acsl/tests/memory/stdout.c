/* run.config
   COMMENT: ensure that standard streams are properly tracked
*/

#include <stdio.h>

int main(void) {
  /*@assert \valid(stderr); */
  /*@assert \valid_read(stdin); */
  /*@assert \valid(stdout); */
  return 0;
}
