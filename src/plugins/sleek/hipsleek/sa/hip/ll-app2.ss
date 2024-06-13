data node {
	int val; 
	node next;	
}

HeapPred H1(node a).
HeapPred H2(node a).
HeapPred H3(node a).
HeapPred G1(node a, node b).
HeapPred G2(node a, node b).

ls<> == self=null 
  or self::node<_,q>*q::ls<>
 inv true;

void append(ref node x, node y)
  // requires x::ls<> * y::ls<> & x!=null
  // ensures x'::ls<>;
  infer [G1,G2]
  requires G1(x,y)
  ensures G2(x',y);
{
	if (x.next == null){
	      x.next = y;	
}
	else {
	      node r=x.next;
	      append(r, y);
              x.next=r;
}
}

