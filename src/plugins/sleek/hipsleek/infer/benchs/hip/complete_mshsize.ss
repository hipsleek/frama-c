/*
	COMPLETE TREES
*/

data node2 {
	int val;
	node2 left;
	node2 right;
}
/*
	- complete binary tree:
		- a binary tree in which every level, except possibly the deepest is completely filled;
		- at depth n, the height of the tree, all nodes are as far left as possible.
*/

/* the view that we used:
 	- n is the height
	- nmin is the "minim height"
		- is the length of the shortest path to a leaf (the minimum between the heights for the two children)
		- we used it to make the insertion easier (because in the insertion there are points where we need to
		know if a subtree is perfect or not)
*/

complete<m,n, nmin> == self = null & n = 0 & nmin = 0 & m=0
  or self::node2<_, l, r> * l::complete<ml1,n-1, nmin1> * r::complete<mr1,n-2, nmin2> & nmin = min(nmin1, nmin2) + 1
  & m = ml1+mr1 + 1
  or self::node2<_, l, r> * l::complete<ml2,n-1, nmin1> * r::complete<mr2,n-1, nmin2> & nmin = min(nmin1, nmin2) + 1
  & m = ml2+mr2 + 1
	inv m>=0 & nmin >= 0 & n >= nmin;


int maxim(int a, int b)
	requires true
	ensures (a < b | res = a) & (a >= b | res = b);
{
	if(a >= b)
		return a;
	else
		return b;
}

int minim(int a, int b)
	requires true
	ensures (a < b | res = b) & (a >= b | res = a);
{
	if(a >= b)
		return b;
	else
		return a;
}

/* function to count the number of nodes in a tree */
relation CNT(int a, int b).
int count(node2 t)
  infer[CNT]
  requires t::complete<n, h, nmin>
  ensures t::complete<n, h, nmin> & CNT(res,n);//res>=0 & res=n
{
	int cleft, cright;

	if (t == null)
		return 0;
	else
	{
		cleft = count(t.left);
		cright = count(t.right);
		return (1 + cleft + cright);
	}
}

relation HGT(int a, int b).
int height(node2 t)
  infer[HGT]
  requires t::complete<n, h, nmin>
  ensures t::complete<m, h, nmin> & res=h & HGT(m,n);//m=n
{
	if (t != null)
		return maxim(height(t.left), height(t.right)) + 1;
	else return 0;
}
//for multi specs
int height1(node2 t)
  requires t::complete<n, h, nmin>
  ensures t::complete<n, h, nmin> & res=h;

relation MHGT(int a, int b).
int min_height(node2 t)
   infer[MHGT]
  requires t::complete<n, h, nmin>
  ensures t::complete<m, h, nmin> & res = nmin & MHGT(m,n);//m=n
{
	if (t != null)
		return minim(min_height(t.left), min_height(t.right)) + 1;
	else return 0;
}
//for multi specs
int min_height1(node2 t)
  requires t::complete<n, h, nmin>
  ensures t::complete<n, h, nmin> & res = nmin;

//relation INS1(int a, int b, int c).
//relation INS2(int a, int b, int c).
//relation INS3(int a, int b).
//relation INS4(int a, int b).
void insert(ref node2 t, int v)
//infer[INS4]
  requires t::complete<n, h1, nmin> & nmin < h1 // there is still place to insert
  ensures t'::complete<n+1, h2, nmin1> & (nmin1 = nmin | nmin1 = nmin + 1) & h1=h2 ;//'INS1(nmin1,nmin, n) & INS3(m,n);
  requires t::complete<n, h1, nmin> & nmin = h1 // there is no empty place -> we need to increase the height
  ensures t'::complete<n+1,h2, nmin1> & (nmin1 = nmin | nmin1 = nmin + 1) & h2=h1+1;//'INS2(nmin1,nmin,n)& INS4(m,n);
{
	node2 aux;

	if(t == null) {
		t = new node2(v, null, null);
		return;
	}
	else {
      if(min_height1(t.left) < height1(t.left)) {		// there is still space in the left subtree
        aux = t.left;
        insert(aux, v);
        t.left = aux;
        return;
      }
      else {
        if(min_height1(t.right) < height1(t.right)) {	// there is still space in the right subtree
          aux = t.right;
          insert(aux, v);
          t.right = aux;
          return;
        }
        else {
          node2 tmp = t.right;
          if(height1(t.left) == height1(t.right)) { // tree is full - we must start another level
            aux = t.left;
            insert(aux, v);
            t.left = aux;
            return;
          }
          else {
            aux = t.right;
            insert(aux, v);
            t.right = aux;
            return;
          }
        }
      }
	}
}

relation PEF(int a, int b).
int is_perfect(node2 t)
  infer[PEF]
  requires t::complete<n, h, nmin>
  ensures t::complete<m, h, nmin> & (nmin != h | res = 1) & (nmin = h | res = 0) & PEF(m,n);//m=n
{
	if(t == null)
		return 1;
	else {
		if(height1(t.left) != height1(t.right))
			return 0;
		else {
			if(is_perfect(t.left) == 1)
				return is_perfect(t.right);
			else
				return 0;
		}
	}
}
