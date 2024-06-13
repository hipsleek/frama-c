data node{
	node prev;
	node next;
}

cdll<prev, p> ==  self= p
  or self::node<prev,n>* n::cdll<self, p>  & self!=p
  ;

HeapPred H1(node a, node@NI b, node@NI c).
  HeapPred G1(node a, node@NI b, node@NI c).

bool check_cdll (node l, node prv, node p)
// requires l::cdll<prv,p>@L ensures  res;
// infer [H1,G1] requires H1(l,prv,p) ensures G1(l,prv,p) & res;
  infer [@shape] requires true ensures res;
{
	if (/* l== prv && */ l== p) return true;
	else { bool r1 = check_cdll(l.next,l,p);
               bool e1 = (l.prev==prv);
               return e1 && r1;
             }
}
/*
# check-dll.ss --pred-en-eup

Why isn't eup working?

cdll<prev, p> ==  self= p
  or self::node<prev,n>* n::cdll<self, p> & self!=p;

[ H1(l,prv,p) ::= 
 l::node<prev,next>@M * H1(next,l,p)&l!=p & prev=prv
 or emp&l=p
 ,
 G1(l,prv,p) ::= 
 l::node<prv,next>@M * G1(next,l,p)&l!=p
 or emp&l=p
 ]
*/
