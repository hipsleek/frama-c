data arrI {
  int val;
}

arr_seg<i,n> == i=n & i>=0
  or x::arrI<_>*self::arr_seg<i+1,n> & x=self+i & i>=0
  inv n>=i & i>=0;

arr_seg_sorted<i,n,mi> == x::arrI<mi> & x=self+i & i=n-1 & i>=0
  or x::arrI<mi>*self::arr_seg_sorted<i+1,n,m2> & x=self+i 
       & i>=0 & i<n-1 & mi<=m2
  inv n>i & i>=0;

void upd_arr(arrI base, int i, int v)
  requires a::arrI<_> & a=base+i & i>=0
  ensures a::arrI<v>;

arrI arr_inc(arrI a)
  requires true //a::arrI<_>@L
  ensures res=a+1;

int get_arr(arrI base, int i)
  requires a::arrI<v>@L & a=base+i
  ensures res=v;

// can base be monomorphic recursive?
void init3(arrI base,int i,int m)
/*  requires base::arr_seg<i,m> * b2::arr_seg_map<i,m,M>@L//& 0<=i & i<=m
  ensures  base::arr_seg_sorted<i,m,i>;
*/
 requires base::arr_seg<i,m>
  case {
   i>=m -> ensures emp;
   i<m -> ensures base::arr_seg_sorted<i,m,i>;
  }
{
  if (i<m) {
    //assume false;
    upd_arr(base,i,i); // base[i]=0
    i=i+1;
    //a = arr_inc(a);
    init3(base,i,m);
  }
}

/*
# ex6d3.ss 

# folding fail at post-cond

 
[[ COND ==>  Fold ==>  UnmatchedRHSData]]falseStop z3... 213 invocations 
Stop Omega... 47 invocations 
!!!Number of log entries 536
!!

*/
