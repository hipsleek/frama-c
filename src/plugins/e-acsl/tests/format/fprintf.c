/* run.config
   COMMENT: Check behaviours of format functions
   DEPS: @PTEST_DEPS@ utils/signalled.h
   STDOPT: +"-eva-precision=1"
*/
/* run.config_dev
   MACRO: INCLUDED_HEADERS utils/signalled.h
   COMMENT: This part is blank on purpose (test stability + Dune)

*/

#include "utils/signalled.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, const char **argv) {
  char *pstr = "Hello world!";
  char template[256];

  /* *** fprintf *** */
  // The first argument to printf should be allocated valid FILE
  OK(fprintf(stdout, "foobar\n"));
  ABRT(fprintf(NULL, "foobar\n"));
  FILE *fh = tmpfile();
  if (fh) {
    OK(fprintf(fh, "foobar %s\n", "foobar"));
    fclose(fh);
    ABRT(fprintf(fh, "foobar %s\n", "foobar"));
    ABRT(fprintf((FILE *)&argc, "foobar %s\n", "foobar"));
  }

  /* *** dprintf *** */
  // The first argument to dprintf should be opened file descriptor
  OK(dprintf(1, "foobar\n"));
  ABRT(dprintf(3, "foobar\n"));

  /* *** sprintf *** */
  // The buffer used with sprintf should be allocated, writeable and large
  // enough
  char buf[5];
  OK(sprintf(buf, "-%s-", "1"));      // 4 chars, fits
  OK(sprintf(buf, "-%s-", "12"));     // 5 chars, still fits
  ABRT(sprintf(buf, "-%s-", "123"));  // 6 chars, no space for NUL
  ABRT(sprintf(NULL, "-%s-", "123")); // try NULL
  ABRT(sprintf(pstr, "-%s-", "123")); // try read-only

  /* *** snprintf *** */
  OK(snprintf(buf, 4, "-%s-", "123"));    // 4 chars, fits
  OK(snprintf(buf, 5, "-%s-", "123"));    // 4 chars, fits
  ABRT(snprintf(pstr, 6, "-%s-", "123")); // try read-only
  ABRT(snprintf(buf, 6, "-%s-", "123"));  // not enough space
  ABRT(snprintf(NULL, 6, "-%s-", "123")); // not enough space
  // NULL should be fine because of 0 chars to write:
  OK(snprintf(NULL, 0, "-%s-", "123"));
  return 0;
}
