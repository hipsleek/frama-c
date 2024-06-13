
data node {
 int val;
 node next;
}


  ll<> == self=null or self::node<_,q>*q::ll<>;

  lseg<p> == self=p or self::node<_,q>*q::lseg<p>;

  lseg_ones<p> == self=p or self::node<1,q>*q::lseg_ones<p>;
  ll_not_one<> == self=null or self::node<v,q>*q::ll<> & v!=1;


bool check_ones(node x)
/*
# verifies..

  requires x::ll<>
  ensures x::lseg<p>*p::ll<> & (res & p=null | !res & p!=null);

  requires x::ll<>@L
  ensures emp;

  requires x::ll<>
  ensures x::lseg<p>*p::ll<> ;

  requires x::ll<>@L
  ensures x::lseg<p>@A*p::ll<>@A ;

  requires x::ll<>@L
  ensures x::lseg<p>@A*p::ll<>@A & (res & p=null | !res & p!=null);

   requires x::ll<>@L
  ensures x::lseg_ones<p>@A*p::ll_not_one<>@A 
           & (res & p=null | !res & p!=null);

  requires x::ll<>
  ensures x::lseg_ones<p>*p::ll_not_one<> 
           & (res & p=null | !res & p!=null);

  requires x::lseg_ones<p>@L*p::ll_not_one<>@L
  ensures (res & p=null | !res & p!=null);

Fails
-----
  requires x::ll<>@L
  ensures x::lseg<p>@A*p::ll<>@A & (res & p!=null | !res & p=null);
*/

  requires x::lseg_ones<p>@L*p::ll_not_one<>@L
  ensures (res & p=null | !res & p!=null);

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
# ex5a.ss

  requires x::ll<>
  ensures x::lseg<p>*p::ll<> & (res & p=null | !res & p!=null);

Above verifies! So does the following immutability spec..

  requires x::ll<>@L
  ensures x::lseg_ones<p>@A*p::ll_not_one<>@A 
           & (res & p=null | !res & p!=null);

Note the use of @A annotation. Also, this would also have been
a proof of the lemma:

  x::ll<> --> (ex p: x::lseg_ones<p>*p::ll_not_one<>)

In the end, we would have derived:

  requires x::lseg_ones<p>@L*p::ll_not_one<>@L
  ensures (res & p=null | !res & p!=null);


*/
