/* singly linked lists */

/* representation of a node */
data node {
	int val;
	node next
}

/* view for a singly linked list with alz elements set to zero */
lz<L1> == self=null & L1=[||]
	or self::node<0, r> * r::lz<L2> & L1=0:::L2;

/* append two singly linked lists */
void append(node x, node y)

	requires x::lz<L1> * y::lz<L2> & len(L1) > 0
	ensures x::lz<L> & len(L) = len(L1) + len(L2);

{
	if (x.next == null) {
		x.next = y;
	} else {
		append(x.next, y);
	}
}

/* return the first element of a singly linked list */
int ret_first(node x)

	requires x::lz<L> & len(L) > 0
	ensures res = 0;
	
{
	return x.val;
}

/* return the tail of a singly linked list */
node get_next(node x)

	requires x::lz<L> & len(L) > 0
	ensures x::lz<L1> * res::lz<L2> & L1 = [| 0 |] & len(L1) = 1 & len(L2) = len(L) - 1;
	
{
	node tmp = x.next;
	x.next = null;
	return tmp;
}

/* function to set the tail of a list */
 void set_next(node x, node y)

	requires x::lz<L1> * y::lz<L2> & len(L1) > 0
	ensures x::lz<L> & L = app([| 0 |], L2) & len(L) = len(L2) + 1;
	
{
	x.next = y;
}

/* function to set null the tail of a list */
void set_null(node x)

	requires x::lz<L1> & len(L1) > 0
	ensures x::lz<L2> & L2 = [| 0 |] & len(L2) = 1;

{
	x.next = null;
}

/* function to get the third element of a list */
node get_next_next(node x) 

	requires x::lz<L1> & len(L1) > 1
	ensures res::lz<L2> & L1 = app([| 0, 0 |], L2) & len(L2) = len(L1) - 2;
	
{
	return x.next.next;
}

/* function to insert a node in a singly linked list */
void insert(node x, int a)

	requires x::lz<L1> & len(L1) > 0 & a = 0
	ensures x::lz<L2> & L2 = app(L1, [| 0 |]) & len(L2) = len(L1) + 1;
	
{
	if (x.next == null) {
		x.next = new node(a, null);
	} else {
		insert(x.next, a);
	}
} 

/* function to delete the a-th node in a singly linked list */
void delete(node x, int a)

	requires x::lz<L1> & len(L1) > a & a > 0 
	ensures x::lz<L2> & l1 = app(L2, [| 0 |]) & len(L2) = len(L1) - 1;

{
	if (a == 1) {
		x.next = x.next.next;
	} else {
		delete(x.next, a-1);
	}	
}

/* function to delete the node with value a in a singly linked list */
/*node delete_val(node x, int a)

	requires x::lz<L1> & a = 0 & x != null
	ensures res::lz<L2> & len(L2) = len(L1) - 1;
	
{
	if (x == null) {
		return x;
	} else {
		if (x.val == a) return x.next;
		else return new node(x.val, delete_val(x.next, a));
	}
}*/

/* function to create a singly linked list with a nodes */
node create_list(int a)

	requires a >= 0 
	ensures res::lz<L> & len(L) = a;

{
	if (a == 0) {
		return null;
	} else {
		a = a - 1;
		return new node (0, create_list(a));
	}
}

/* function to reverse a singly linked list */
void reverse(ref node xs, ref node ys)

	requires xs::lz<L1> * ys::lz<L2> 
	ensures ys'::lz<L> & len(L) = len(L1) + len(L2) & xs' = null;

{
	if (xs != null) {
		node tmp;
		tmp = xs.next;
		xs.next = ys;
		ys = xs;
		xs = tmp;
		reverse(xs, ys);
	}
}
