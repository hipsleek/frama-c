/* bubble sort */

data node {
	int val;
	node next;
}



sll<n, sm, lg, S> ==
		self::node<sm, null> & n=1 & sm =lg & S = {sm}
	or	self::node<sm, q> * q::sll<n1, qs, lg, S1> & q!=null &  n1 = n-1 & sm <= qs & S = union(S1,{sm})
	inv n>=1 & sm<=lg;


ll<n, S> == self=null & n=0 & S={}
	or self::node<d, r> * r::ll<n-1,S2> & S=union(S2,{d}) 
	inv n>=0;

//lemma self::sll<n, sm, lg, S> -> self::ll<n,S>;

//------------------------------------------------------------

// ------------------ FUNTIONS -----------------------------//

bool bubble(node xs)
	requires xs:: ll <n,S> & n >0
	ensures xs::sll<n, sm, lg, S> & !res
	or xs::ll<n, S> & res ;
{
  int aux, tmp1;
  bool tmp, flag;

  if (xs.next == null) {
    return false;
  }
  else {
    tmp = bubble(xs.next);
    if (xs.val <= xs.next.val) {
      flag = false;
    }
    else {
      aux = xs.val;
      tmp1 = xs.next.val;
      xs.val = tmp1;
      xs.next.val = aux;
      flag = true;
    }
    return (flag || tmp);
  }
}
