struct node {
  int val;
  struct node* next;
};

/*@
ll<> == self=null
  or self::node<_,p>*p::ll<>;
*/

/*@
HeapPred H( node a).
HeapPred G( node a).
*/

int main(struct node* l)
/*@
  requires l::ll<>
  ensures l::ll<>;
*/
{
  int i = 0;
  while (1)
    /*
      infer [H,G]
      requires H(l)
      ensures G(l');
     */
    /*@
      requires l::ll<>
      ensures l::ll<> & l'=null;
    */
    {
    if (l == NULL) {
      break;
    } else {
      l = l->next;
      i++;
    }
  }
  return i;
}
