//#include <stddef.h>
// star_fields
data node {
  int x;
  node next;
}

ll<n> == self=null & n=0
  or self::node<_,q>*q::ll<n-1>
  inv n>=0;

int foo(node q)
  requires q::ll<n>
  ensures q::ll<n> & res=n;
{
  if (q==null) return 0;
  else return 1+foo(q.next);
}


int main() 
 requires true
 ensures res=0;
{
  node pcp sho=null;
  return foo(p);
}

/*
int foo2(struct node *q)
  requires q::ll<n>
  ensures q::ll<n>;
{
  struct node* tmp = q;
  if (tmp) return 0;
  else return 1+foo2((*q).next);
}


int star_foo(node star_q)
  requires *q::ll<n>
  ensures *q::ll<n>;
{
  if (star_q==0) return 0;
  else return 1+star_foo(star_q.next);
}
*/

