/* merge sort */

data node {
	int val;
	node next;
}

ll1<"n":n, "S":S> == self =  null & ["S" : S={} ; "n" : n = 0]
  or self::node<v, r> * r::ll1<n1, S1>  & ["n" : n = n1+1 ; "S" : S = union(S1, {v})]
  inv true & ["n" : n >= 0];

sll1<"n":n, "S":S> == self = null & ["S" : S={} ; "n" : n = 0]
  or self::node<v2, r> * r::sll1<n1, S1> & ["n" : n = n1+1;
                                            "S" : S = union(S1, {v2}) & forall(x: (x notin S1 | v2 <= x))]
  inv true & ["n" : n >= 0];

/* function to count the number of elements of a list */
int count1(node x)
  requires x::ll1<n, S>
  ensures x::ll1<n, S> & ["n" : res = n];

{
	int tmp;
	if (x == null) {
      return 0;
	} else {
	   tmp = 1 + count1(x.next);
       //dprint;
      return tmp;
    }
}

/* function to divide a list into 2 lists, the first one containing a elements and the second the rest */
node split1(ref node x, int a)
  requires x::ll1<n, S> & ["n" : n > a & a > 0]
  ensures x'::ll1<n1, S1> * /*'*/ res::ll1<n2, S2> & 
  ["n" : n = n1 + n2 & n1 > 0 & n2 > 0; "S" : S = union(S1, S2)];
{
	node tmp;

	if (a == 1)
	{
		tmp = x.next;
		x.next = null;
		return tmp;
	}
	else
	{
		a = a - 1;
		node tmp;
        //dprint;
		bind x to (_, xnext) in {
			tmp = split1(xnext, a);
		}
		return tmp;
	}
}

int div2(int c) requires true ensures res + res = c;

/* merge sort */
node merge_sort1(node xs)
  requires xs::ll1<n, S> & ["n" : n>0]
  ensures res::sll1<n, S>;
{
	int c, middle;
	node s1, s2, s3;

	if (xs.next != null)
	{
		c = count1(xs);
		middle = div2(c);
		s1 = split1(xs, middle);
		s2 = merge_sort1(s1);
		s3 = merge_sort1(xs);
	        return merge1(s2, s3);
	}
	else {
		return xs;
	}

}

node merge1(node x1, node x2)
  requires x1::sll1<n1, S1> * x2::sll1<n2, S2>
  ensures res::sll1<nn, S3> & ["n" : nn = n1 + n2; "S" : S3 = union(S1, S2)];
{
  if (x2 == null) {
    //dprint;
    return x1;
  } else {
    if (x1 == null)
      return x2;
    else {
      x1 = insert1(x1, x2.val);
      if (x2.next != null)
        return merge1(x1, x2.next);
      else
        return x1;
    }
  }
}

/* function to insert an element in a sorted list */
node insert1(node x, int v)
	requires x::sll1<n, S> & x != null
	ensures res::sll1<n+1, S1> & S1 = union(S, {v});
{
	node tmp_null = null;
	node tmp;

	if (v <= x.val)
		return new node(v, x);
	else
	{
		if (x.next != null)
		{
			tmp = insert1(x.next, v);
			x.next = tmp;
		}
		else
			x.next = new node(v, tmp_null);

		return x;
	}
}


