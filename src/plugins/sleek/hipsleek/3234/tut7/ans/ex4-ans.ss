data node {
  int val;
  node next;
}

ll<n> == self=null & n=0
  or self::node<v,q>*q::ll<n-1>
  inv n>=0;

lseg<p,n> == self=p & n=0
  or self::node<v,q>*q::lseg<p,n-1>
  inv n>=0;

/*
  (1) Write the strongest postcondition
  (2) Prove termination of this algorithm
      by adding a Term[..] constraint to precondition
*/

int length(node x)
  requires x::ll<n> & Term[n]
  ensures x::ll<n> & res=n;
  requires x::lseg<null,n> & Term[n]
  ensures x::lseg<null,n> & res=n;
{
  if (x==null) {
       return 0;
  } else {
       int v = length(x.next);
       return 1+v;
  }
}


