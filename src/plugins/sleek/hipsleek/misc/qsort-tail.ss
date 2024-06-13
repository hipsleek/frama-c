/* quick sort */

/* quick sort is now done over lists with tail pointer
   so that append can be done in constant time */

data node {
	int val; 
	node next; 
}

// sorted lseg
lseg<n, p, sm, lg> == self=p & n=0 & sm<=lg
		or self::node<sm, r> * r::lseg<n-1, p, sm1, lg> & sm<=sm1<=lg
	inv n >= 0 & sm<=lg;

// sorted list with tail
ll_tail<n, t, sm, lg> == self::node<sm, null> & t=self & n=1 & sm=lg
		or self::node<sm, r> * r::ll_tail<n-1, t, sm1, lg> & r!=null & sm<=sm1
	inv n>=0 & self!=null;

// bounded list with tail
bnd_tail<n, t, sm, lg> == self = null & n = 0 & t=null & sm <= lg
	or self::node<v, null> & t = self & n = 1 & sm <= v <= lg
	or self::node<d, p> * p::bnd_tail<n-1, t, sm, lg> & sm <= d <= lg & p!=null
inv n >= 0;


coercion "ll_tail2lseg" self::ll_tail<n, t, sm, lg> <-> self::lseg<n-1, t, sm, lg1> * t::node<lg, null> & lg1<=lg;


coercion "lsegmb" self::lseg<n, p, sm, lg> & n = n1+n2 & n1,n2 >=0  <-> self::lseg<n1, q, sm, lg1> * q::lseg<n2, p, sm2, lg> & lg1<=sm2;


void qsort(ref node x, ref node tx)
	requires x::bnd_tail<n, tx, sm, lg> & n>0
	ensures x'::ll_tail<m, tx', sm1, lg1> & sm <= sm1 & lg1 <= lg & m = n ;
{
	if (x == null) return; // not needed
	else if (x.next == null) {
      //assume false;
		return;
	}
	else {
		node y, ty, tmp1;
		int temp = x.val;

		assert x'::bnd_tail<xx, tx', sm, lg> & sm <= temp' <= lg;
		assert x'::bnd_tail<_, tx', sm, lg> & sm <= temp' <= lg;
                dprint;
		partition1(x, tx, y, ty, x.val);

		// recursive sorting

		if (x != null)
			qsort(x, tx);

		if (y != null)
			qsort(y, ty);

		if (x == null) {
          //assume false;
			x = y;
			tx = ty;
            assert x'::ll_tail<n,tx', _, _>;
			return;
		}
		else if (y != null) {
			tx.next = y;
			tx = ty;
            //dprint;
            assert x'::ll_tail<_, _, _, _>; //'
              //assume false;
			return;
		}
	}
}

/*
	partitions a list pointed to by x to 
	two lists. The new x list contains the smaller elements, the y list
	contains the bigger ones.
*/	
void partition1(ref node x, ref node tx, ref node y, ref node ty, int c)
	requires x::bnd_tail<n, tx, sm, lg> & sm <= c <= lg 
	ensures x'::bnd_tail<n1, tx', sm, c> * y'::bnd_tail<n2, ty', c, lg> & n=n1+n2;
{
	if (x == null) {
		tx = null;
		ty = null;
		y = null;
		return; //FIXIT: accept return null
	}
	else {
		int v; 

		if (x.val < c) {
			// x belongs to the first partition (the smaller one)
			if (x.next == null) {
				tx = x; // shoudlnt' be needed
				ty = null;
				y = null;
				return;
			}
			else {
				bind x to (xval, xnext) in {
					partition1(xnext, tx, y, ty, c);
				}

				if (x.next == null) {
					// If all the rest goes to the second list, 
					// the first list contains only one element.
					// After the recursive partition call, tx is null,
					// hence need to set it to point to x
					tx = x;
				}

				return;
			}
		}
		else {
			if (x.next == null) {
				y = x;
				ty = x;
				x = null;
				tx = null;
				return;
			}
			else {
				bind x to (xval, xnext) in {
					partition1(xnext, tx, y, ty, c);
				}

				// xs  belongs to the "greater than" partition, which is pointed
				// to by y.
				// xs must be updated to point to the "greater than" partition.

				if (y == null) ty = x;
	
				node tmp1 = x;
				x = x.next;

				tmp1.next = y;
				y = tmp1;


				return;
			}
		}
	}
}

