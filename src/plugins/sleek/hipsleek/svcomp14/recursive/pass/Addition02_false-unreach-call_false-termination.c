extern void __VERIFIER_error() __attribute__ ((__noreturn__));

/*
 * Recursive implementation integer addition.
 * 
 * Author: Matthias Heizmann
 * Date: 2013-07-13
 * 
 */

extern int __VERIFIER_nondet_int(void);

int addition(int m, int n)
/*@
  infer [@post_n]
  requires true
  ensures true;
 */
{
    if (n == 0) {
        return m;
    }
    if (n > 0) {
        return addition(m+1, n-1);
    }
    /* if (n < 0) { */
    /*     return addition(m-1, n+1); */
    /* } */
    return addition(m-1, n+1);
}

// Expect FAIL
// Return FAIL

int main()
/*@
  requires true
  ensures res!=1;
*/
{
    int m = __VERIFIER_nondet_int();
    int n = __VERIFIER_nondet_int();
    int result = addition(m,n);
    if (result == m - n) {
        return 0;
    } else {
      return 1;
      //  ERROR: __VERIFIER_error();
    }
}
