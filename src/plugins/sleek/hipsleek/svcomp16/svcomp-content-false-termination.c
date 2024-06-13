#include "../examples/working/cparser/stdhip.h"
struct node{
  int val;
  struct node* next;
};

struct node* new_ll(int n)
{
  if (n == 0)
    return NULL;
  struct node *x = malloc(sizeof *x);
  x->val = 1;
  x->next = new_ll(n-1);
  return x;
}

int check(struct node* xs)
{
  if (xs->val == 0)
    return 1;
  return check(xs->next);
}

void main()
{
  struct node *xs = new_ll(10);
  return (check(xs));
}
