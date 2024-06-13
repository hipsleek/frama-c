data node {
 int val;
 node next;
}

HeapPred H(node x). // non-ptrs are @NI by default
PostPred G(node x). // non-ptrs are @NI by default

/*
sortll<n> == self=null
 or self::node<v,q>*q::sortll<v> & n<=v
 inv true; 
*/

  ll<> == self=null or self::node<_,q>*q::ll<>;

  lseg<p> == self=p or self::node<_,q>*q::lseg<p>;

  gg<p> == self=null or self=p or self::node<_,q>*q::gg<p>;

relation R1(bool r).
relation R2(bool r).


bool check_ones(node x)
  infer [R1,R2]
  requires x::ll<>
  ensures x::ll<> & R1(res) or x::lseg<p>*p::ll<> & R2(res);
{ 
  if (x==null) return true;
  else {
   int t = x.val;
   if (t==1) return check_ones(x.next);
   else {
      //dprint;
       return false;
   }
 }
} 

/*
# ex3c.ss

  infer [R1,R2]
  requires x::ll<>
  ensures x::ll<> & R1(res) or x::lseg<p>*p::ll<> & R2(res);

# pure bi-abduction is not working here. is it due
  to mutual recursion?

We need to investigate. Is it due to fixcalc 
or some other problems..

*************************************
******pure relation assumption 1 *******
*************************************
[RELDEFN R1: ( 1<=res) -->  R1(res),
RELDEFN R2: ( 1<=res) -->  R2(res),
RELDEFN R1: ( R1(res)) -->  R1(res),
RELDEFN R2: ( R1(res)) -->  R2(res),
RELDEFN R2: ( R2(res)) -->  R2(res),
RELDEFN R1: ( not(res)) -->  R1(res),
RELDEFN R2: ( not(res)) -->  R2(res)]
*************************************

!!! PROBLEM with fix-point calculation
ExceptionFailure("split_quantifiers: invalid argument (formula_or)")Occurred!


*/
