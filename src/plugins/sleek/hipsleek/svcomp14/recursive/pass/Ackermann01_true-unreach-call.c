extern void __VERIFIER_error() __attribute__ ((__noreturn__));

/*
 * Implementation the Ackermann function.
 * http://en.wikipedia.org/wiki/Ackermann_function
 * 
 * Author: Matthias Heizmann
 * Date: 2013-07-13
 * 
 */

extern int __VERIFIER_nondet_int(void);

int ackermann(int m, int n)
/*@
  infer [@post_n]
  requires true
  ensures true;
 */
{
    if (m==0) {
        return n+1;
    }
    if (n==0) {
        return ackermann(m-1,1);
    }
    return ackermann(m-1,ackermann(m,n-1));
}

// Expect SUCCESS
// Return SUCCESS

int main()
/*@
  requires true
  ensures res!=1;
*/
{
    int m = __VERIFIER_nondet_int();
    if (m < 0 || m > 3) {
        return 0;
    }
    int n = __VERIFIER_nondet_int();
    if (n < 0 || n > 23) {
        return 0;
    }
    int result = ackermann(m,n);
    if (m < 0 || n < 0 || result >= 0) {
        return 0;
    } else {
      return 1;
      //ERROR: __VERIFIER_error();
    }
}
