data node {
  int val;
  node next;
}

ll<n> == self = null & n = 0 
	or self::node<_, q> * q::ll<n-1> 
  inv n >= 0;


int foo(node x)
  requires x::ll<n>@L & Term[n]
  ensures res=0;  
{
  if (x==null) return 0;
  else {
    int m = foo(x.next);
    return m;
  }
}

