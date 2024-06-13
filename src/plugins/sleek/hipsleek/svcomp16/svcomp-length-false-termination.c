#include "../examples/working/cparser/stdhip.h"
struct node {
  int val;
  struct node* next;
}; 

struct node* new_lseg(struct node* p, int n)
{
  if (n==0)
    return p;
  struct node *x = malloc(sizeof *x);
  x->next = new_lseg(p, n-1);
  return x;
}

struct node* new_cll(int n)
{
  struct node *x = malloc(sizeof *x);
  x->next = new_lseg(x,n-1);
  return x;
}


int length(struct node* xs)
{
  if (xs == NULL) 
    return 0;
  return (1+length(xs->next));
}

void main()
{
  struct node *xs = new_cll(10);
  return (length(xs));
}
