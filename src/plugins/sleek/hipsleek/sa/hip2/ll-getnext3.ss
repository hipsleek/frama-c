/* circular lists */

/* representation of a node */
data node {
	int val; 
	node next;	
}

HeapPred H(node a).
HeapPred G(node a,node a).

/* function to delete the node after the head in a circular list */
void get_next(node x)

   requires x::node<_,q>
   ensures x::node<_,q> ;
/*
infer [H,x,G] 
requires H(x)
ensures G(x,res);
*/
{
        dprint;
}
/*
*/




