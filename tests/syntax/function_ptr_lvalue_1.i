/* run.config*

 EXIT: 1
   OPT:
*/

void f(void) {}

int main()
  {
  void (*p)(void) = &f ;
  p = 1 ;
  *p = 1 ;
  return 0 ;
  }
