/* avl trees */

/* representation of a node in an avl tree */
data node {
	int val;
	int height;
	node left;
	node right;
}

/* view for avl trees */
avl1<h> == self = null & h = 0
	or self::node<_, h, p, q> * p::avl1<h1> * q::avl1<h2> &
        h2<=h1+1 & h1<=h2+1 & h = max(h1, h2) + 1
	inv h >= 0;

/* function to return the height of an avl tree */
int height1(node x)
     requires x::avl1<h>
     ensures x::avl1<h> & res = h;

int height(node x)
     infer @post []
     requires x::avl1<h>
     ensures x::avl1<h>;
{
	if (x == null)
      return 0;
	else
      return x.height;
}

/*  function to rotate left */
node rotate_left1(node l, node rl, node rr)
  requires l::avl1<ln> * rl::avl1<ln> * rr::avl1<ln+1>
  ensures res::avl1<2+ln>;

node rotate_left(node l, node rl, node rr)
  infer [rr]
  requires l::avl1<ln> * rl::avl1<ln> * rr::avl1<ln+1>
  ensures res::avl1<k>;
{
	node tmp;
	int v = 10, h;

	h = height1(l) + 1;
	tmp = new node(v, h, l, rl);
	h = h + 1;
	return new node(v, h, tmp, rr);
}

/* function to rotate right */
node rotate_right1(node ll, node lr, node r)
  requires ll::avl1<lln> * lr::avl1<lln - 1> * r::avl1<lln - 1>
  ensures res::avl1< 1 + lln>;

node rotate_right(node ll, node lr, node r)
  infer[ll]
  requires ll::avl1<lln> * lr::avl1<lln - 1> * r::avl1<lln - 1>
  ensures res::avl1<k>;
{
	node tmp;
	int v = 10, h;

	h = height1(r) + 1;
	tmp = new node(v, h, lr, r);
	h = h + 1;
	return new node(v, h, ll, tmp);
}

int get_max(int a , int b)
  requires true
  ensures res = max(a, b);

int get_max1(int a , int b)
  infer @post []
  requires true
  ensures true;
{
	if (a >= b)
		return a;
	else
		return b;
}

/* double left rotation */
node rotate_double_left1(node a, node b, node c, node d, int v1, int v2, int v3)
  requires a::avl1<an> * b::avl1<bn> * c::avl1<cn> * d::avl1<an>
  & an = max(bn, cn) & -1 <= bn - cn <= 1
     ensures res::avl1<k> & k=an + 2 & res!=null;

node rotate_double_left(node a, node b, node c, node d, int v1, int v2, int v3)
  infer @post []
  requires a::avl1<an> * b::avl1<bn> * c::avl1<cn> * d::avl1<an>
  & an = max(bn, cn) & -1 <= bn - cn <= 1
     ensures res::avl1<k>;
{
	node tmp1, tmp2;
	int h;

	h = get_max(height1(a), height1(b));
	h = h + 1;
	tmp1 = new node(v1, h, a, b);

	h = get_max(height1(c), height1(d));
	h = h + 1;
	tmp2 = new node(v3, h, c, d);

	h = get_max(height1(tmp1), height1(tmp2));
	h = h + 1;
	return new node(v2, h, tmp1, tmp2);
}

/* double right rotation */
node rotate_double_right1(node a, node b, node c, node d, int v1, int v2, int v3)
  requires a::avl1<an> * b::avl1<bn> * c::avl1<cn> * d::avl1<an>
	         & an = max(bn, cn) & -1 <= cn - bn <= 1
     ensures res::avl1<k> & k=2 + an & res!=null;

node rotate_double_right(node a, node b, node c, node d, int v1, int v2, int v3)
  infer @post []
  requires a::avl1<an> * b::avl1<bn> * c::avl1<cn> * d::avl1<an>
	         & an = max(bn, cn) & -1 <= cn - bn <= 1
     ensures res::avl1<k>;
{
	node tmp1, tmp2;
	int h;

	h = get_max(height1(a), height1(b));
	h = h + 1;
	tmp1 = new node(v3, h, a, b);

	h = get_max(height1(c), height1(d));
	h = h + 1;
	tmp2 = new node(v1, h, c, d);

	h = get_max(height1(tmp1), height1(tmp2));
	h = h + 1;
	return new node(v2, h, tmp1, tmp2);
}


/* functions to build avl trees */
node build_avl1(node x, node y)
  infer[x]
  requires x::avl1<nx1> * y::avl1<nx1> 
  ensures res::avl1<k>;
{
	int v = 0;
	int tmp;

	tmp = x.height;
	tmp = tmp + 1;
	return new node(v, tmp, x, y);
}

void build_avl2(ref node x, node y, node z)
  infer[y]
  requires y::avl1<ny> * z::avl1<ny> * x::node<_, _, _, _> 
  ensures  x'::avl1<k>;
{
	int tmp;

	x.left = y;
	x.right = z;
	x.height = y.height  + 1;
}

node node_error() requires true ensures false;

node insert1(node x, int a)
  requires x::avl1<n>
  ensures res::avl1< _>;

/* function to insert a node in an avl tree (using the rotate functions) */
relation INS(int a, int b).
node insert(node x, int a)
  infer[INS, x, a]
  requires x::avl1<n>
  ensures res::avl1<k> & INS(k,n);
{
	node tmp, tmp_null = null;

	if (x == null)
		return new node (a, 1, tmp_null, tmp_null);
	else
	{
		if (a <= x.val)
		{
          tmp = x.left;
          x.left = insert(tmp, a);
          if ((height1(x.left) - height1(x.right)) == 2)
			{
              if (height1(x.left.left) > height1(x.left.right))
				{
                  return rotate_right1(x.left.left, x.left.right, x.right);
				}
              else
				{
                  if (height1(x.left.left) == (height1(x.left.right) - 1))
                    return rotate_double_left1(x.left.left, x.left.right.left, x.left.right.right, x.right, 1, 1, 1);
                  else
                    return node_error();
				}
			}
          else
            return node_error();
		}
		else
          {
			tmp = x.right;
			x.right = insert(tmp, a);
			if ((height1(x.right) - height1(x.left)) == 2)
              {
				if (height1(x.right.right) > height1(x.right.left))
                  {
					return rotate_left1(x.left, x.right.left, x.right.right);
                  }
				else
                  {
					if ((height1(x.right.left) - 1) == height1(x.right.right))
                      return rotate_double_right1(x.left, x.right.left.left, x.right.left.right, x.right.right, 1, 1, 1);
					else
                      return node_error();
                  }
              }
			else
              return node_error();
		}
	}
}

/* function to insert in an avl tree (inline version) */
relation INSI1(int a, int b).
relation INSI2(int a, int b).
node insert_inline(node x, int a)
  infer[x]
  requires x::avl1< n>
  ensures res::avl1<n1> &  n<= n1 <= n+1;
{
	node k1, tmp, k2, tmp_null = null;
	int h, hl, hr, hlt;

	if (x == null)
		return new node(a, 1, tmp_null, tmp_null);
	else
	{
		if (a <= x.val)
		{
          tmp = x.left;
          x.left = insert_inline(tmp, a);
          if ((height1(x.left) - height1(x.right)) == 2)
			{
              k1 = x.left;
              if (height1(k1.left) > height1(k1.right))
				{
                  x.left = k1.right;
                  h = get_max(height1(k1.right), height1(x.right));
                  k1.right = x;
                  h = h + 1;
                  x.height = h;
                  h = get_max(height1(k1.left), h);
                  h = h + 1;
                  k1.height = h;
                  return k1;
				}
              else
				{
                  if (height1(k1.left) == (height1(k1.right) - 1))
					{
                      k2 = k1.right;
                      x.left = k2.right;
                      k1.right = k2.left;
                      hr = height1(k2.left);
                      k2.left = k1;
                      hlt = height1(k2.right);
                      k2.right = x;

                      hl = height1(k1.left);
                      h = get_max(hl, hr);
                      h = h + 1;
                      k1.height = h;

                      hr = height1(x.right);
                      h = get_max(hlt, hr);
                      h = h + 1;
                      x.height = h;

                      h = get_max(height1(k1), x.height);
                      h = h + 1;
                      k2.height = h;

                      return k2;
					}
                  else
                    return node_error();
				}
			}
			else
              return node_error();
		}
		else	
		{
          tmp = x.right;
          x.right = insert_inline(tmp, a);
          if ((height1(x.right) - height1(x.left)) == 2)
			{
              k1 = x.right;
              if (height1(k1.right) > height1(k1.left))
				{
                  x.right = k1.left;
                  hr = height1(k1.left);
                  k1.left = x;

                  hl = height1(x.left);
                  h = get_max(hr, hl);
                  h = h + 1;
                  x.height = h;

                  hr = height1(k1.right);
                  h = get_max(height1(x), hr);
                  h = h + 1;
                  k1.height = h;

                  return k1;
				}
              else
				{ 
                  if ((height1(k1.left) - 1) == height1(k1.right))
					{
                      k2 = k1.left;

                      x.right = k2.left;
                      k1.left = k2.right;
                      hr = height1(k2.left);
                      k2.left = x;
                      hlt = height1(k2.right);
                      k2.right = k1;

                      hl = height1(x.left);
                      h = get_max(hl, hr);
                      h = h + 1;
                      x.height = h;

                      hr = height1(k1.right);
                      h = get_max(hlt, hr);
                      h = h + 1;
                      k1.height = h;

                      h = get_max(height1(x), height1(k1));
                      k2.height = ++h;

                      return k2;
					}
                  else
                    return node_error();
				}
			}
          else
            return node_error();
		}
	}
}

relation MRG1(int a, int b).
node merge(node t1, node t2)
  infer[MRG1, t1,t2]
case {
      t1=null -> requires t2::avl1<h2> ensures res::avl1<h3> & MRG1(h2,h3);
      t1!=null -> requires t1::avl1<h1> * t2::avl1<h2> ensures res::avl1<_>;
}
{
 if (t1 == null) return t2;
    else {
	  node tmp = insert1(t2, t1.val);
	  node tmp1 = merge (tmp, t1.left);
	  return merge(tmp1, t1.right);
	  }
}

