/*
 * Date: 30/09/2015
 * Created by: Ton Chanh Le (chanhle@comp.nus.edu.sg)
 */
 
#include <stdlib.h>

extern int __VERIFIER_nondet_int();

/*@
ll<n> == self=null & n=0
  or self::node<_,q>*q::ll<n-1>
inv n>=0.
*/

typedef struct node {
    int val;
    struct node* next;
} node_t;

//Initialize a null-terminating linked list with length n
node_t* init_ll (int n)
  /*@
    requires n>=0
    ensures res::ll<n>;
  */
{
  node_t* h = NULL;
  node_t* curr;
  
  for (int i = 0; i < n; i++) 
    /*@ 
       requires h::ll<i> & i<=n & Term[n-i]
       ensures  h'::ll<n> & i'=n & i'>=i;
    */
  {
    curr = alloca(sizeof(node_t));
    curr->val = i;
    curr->next = h;
    h = curr;
  }
  return h;
}

void safe_search (node_t* h, int i)
  /*@
    requires h::ll<n>
    ensures true;
  */
{
  node_t* curr = h;
  while (curr != NULL && curr->val != i) 
    /*@
      requires curr::ll<n>
      ensures curr'::node<i,_> or curr' = null;
    */
  {
    curr = curr->next;
  }
}

void main ()
{
  int n = __VERIFIER_nondet_int();
  node_t* head = init_ll(n);
  safe_search(head, __VERIFIER_nondet_int() % n);
}

/*
Proving precondition in method init_ll$int Failed.
  (may) cause:  nondet_int__(v_nd_64_1894') & n'=v_nd_64_1894' |-  0<=n'. LOCS:[64;-1;65] (may-bug)

Context of Verification Failure: _0:0_0:0

Last Proving Location: ll_search-alloca_true-termination_spec.c_65:17_65:27

Procedure main$ FAIL.(2)


Exception Failure("Proving precond failed") Occurred!
*/


