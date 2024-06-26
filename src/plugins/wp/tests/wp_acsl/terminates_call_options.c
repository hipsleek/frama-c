/* run.config
   OPT: -generated-spec-custom terminates:skip,exits:skip
   OPT:
*/
/* run.config_qualif
   OPT: -generated-spec-custom terminates:skip,exits:skip
   OPT:
*/

#include <stdlib.h>

// -wp-declarations-terminate   <--- default to FALSE
// -wp-definitions-terminate    <--- default to FALSE
// -wp-frama-c-stdlib-terminate <--- default to FALSE

//@ assigns \nothing ;
void declaration(void);

//@ assigns \nothing ;
void definition(void){}

//@ terminates \true ;
void call_declaration(void){
  declaration();
}

//@ terminates \true ;
void call_definition(void){
  definition();
}

void no_spec_generates_goal(void){
  for(;;);
}

//@ terminates \true ;
void libc_call(void){
  (void) div(4,3);
  exit(0);
}
