/* run.config*
 EXIT: 1
   OPT: %{dep:./bts0990_link_1.i}
*/
// NB: This test is meant to return an error, as s is declared as an array in
// %{dep:./bts0990_link_1.i}

char *s;

void perror(const char *);

void f(void){
  perror(s);
}
