void __error()
/*@
  requires emp & true
  ensures emp & true & flow __Error;
*/;

int sum(int n, int m)
/*
  requires true
  ensures res=n+m;
*/
/*@ infer[@pre_n,@post_n]
  requires true
  ensures true;
*/
{
    if (n <= 0) {
      return m + n;
    } else {
      return sum(n - 1, m + 1);
    }
}

void main()
/*@
  requires true
  ensures true;
*/
{
  int a = 2;
  int b = 3;
  int result = sum(a, b);
  if (result == a + b) {
    __error();
  }
}
