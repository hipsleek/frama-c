data anode {
  int val;  // 0-apply; 1-K; 2-S
  anode fn;
  anode arg;
}

allowed<> ==
     self::anode<0,f,a> * f::allowed<> * a::allowed<>
  or self::anode<1,null,null> // denotes K
  or self::anode<2,null,null> // denotes S
  inv self!=null;

allowK<n> ==
     self::anode<0,f,a> * f::allowK<n1> * a::allowK<n2> & n=1+n1+n2
  or self::anode<1,null,null> & n=0  // denotes K
  inv self!=null & n>=0;


value<> ==
  self::anode<1,null,null>  // denotes K
  or self::anode<2,null,null>  // denotes S
  or self::anode<0,f,a> * f::anode<1,null,null> * a::value<> // K v
  or self::anode<0,f,a> * f::anode<2,null,null> * a::value<> // S v
  or self::anode<0,f,a> * f::anode<0,f1,a1> * 
      f1::anode<2,null,null> * a1::value<> * a::value<> // S v1 v2
  inv self!=null;

ks<n> == self::anode<1,null,null> & n = 0 // K
      or self::anode<0,f,a> * f::ks<n-1> * a::anode<1,null,null> 
inv n >= 0;

lemma self::value<> -> self::allowed<>;

anode clone (anode t)
requires t::value<>@I
ensures  res::value<>;

bool isApply(anode t)
  requires t::anode<v,_,_>@I
  ensures true & (v=0 & res | v!=0 & !res);
{
  return t.val == 0;
}

bool isCombK(anode t)
  requires t::anode<v,_,_>@I
  ensures true & (v=1 & res | v!=1 & !res);
{
  return t.val == 1;
}

bool isCombS(anode t)
  requires t::anode<v,_,_>@I
  ensures true & (v=2 & res | v!=2 & !res);
{
  return t.val == 2;
}


// BELOW is PERFORMANCE BUG
anode reduction (anode t)

requires t::allowed<>
ensures  res::value<>;

requires t::allowK<n>
variance (1) [n]
ensures  res::value<> ;

requires (exists k: t::ks<n> & n=2*k & k>=0)
ensures res::anode<1,null,null>;

requires (exists k: t::ks<n> & n=2*k+1 & k>=0)
ensures res::anode<0,f,a> * f::anode<1,null,null> * a::anode<1,null,null>;

{
 anode val1, val2, val11, val2c;
 anode tmp1, tmp2, tmp3;
 if (isApply(t)) {
   // apply
   val1 = reduction(t.fn);
   val2 = reduction(t.arg);
   t.fn = val1;
   t.arg = val2;
   if (isCombK(val1)) return t;
   else if (isCombS(val1)) return t;
   else { 
     // val1 is an apply
     val11 = val1.fn;
     if (isCombK(val11)) return val1.arg;
     else if (isCombS(val11)) return t;
     else {
       // val3 is an apply
       // it has to be an (S w1)
       val2c = clone(val2);
       tmp1 = new anode(0,val11,val2);
       tmp2 = new anode(0,val1.arg,val2c);
       t.fn = tmp1;
       t.arg = tmp2;
       return reduction(t);
     }
   }
 } else return t; 
}






