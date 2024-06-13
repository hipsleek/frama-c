/* selection sort */

data node {
	int val; 
	node next; 
}

// needs infinity
/*
sortA<v> == self::node<v,null> 
 or self::node<v, p> * p::sortA<v2> & v<=v2 
inv self!=null;
*/

sortHO<v,R:relation(int,int)> == 
  self::node<v,null> 
  or self::node<v, p> * p::sortHO<v2,R> & R(v,v2) 
inv self!=null;

relation R(int r, int a) == r<=a .
relation R0(int r, int a).
relation R1(int r, int a).

node insert(node x, node y)
     infer []
     requires x::sortHO<a,R> * y::node<v,null>
     ensures  res::sortHO<b,R> & (v>a & b=a | (a>=b & b=v));

node sort(node x)
     infer [R0,R1]
     requires x::sortHO<a,R0>
     ensures  res::sortHO<b,R1> ;
{
    node tmp = x.next;
    if (tmp==null) return x;
    else {
      x.next=null;
      tmp=sort(tmp);
      return insert(tmp,x);
    }
}

