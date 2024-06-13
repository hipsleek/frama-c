/* Red Black Trees */
/* Given Shape -> Infer Size + Height Property */

data node5 {
  int val;
  int color; /* 0 for black, 1 for red */
  node5 left;
  node5 right;
}

rbt<> == self = null
  or self::node5<v, _, l, r> * l::rbt<> * r::rbt<>;

/******************************************/

/* Check if a node is red or not */
bool is_red(node5 x)
  requires x::rbt<>
  case {
    x=null -> ensures !res;
    x!=null -> ensures x::rbt<> & (res | !res);
  }
{
  if (x == null)
    return false;
  else
    if (x.color == 0)
      return false;
    else
      return true;
}


/* Check if a node is black or not */
bool is_black(node5 x)
  requires x::rbt<>
  case {
    x=null -> ensures res;
    x!=null -> ensures x::rbt<> & (!res | res);
  }
{
  if (x == null)
    return true;
  else
    if (x.color == 0)
      return true;
    else
      return false;
}

/* Rotate case 3 */
node5 rotate_case_3(node5 a, node5 b, node5 c)
  requires a::rbt<> * b::rbt<> * c::rbt<> & a!=null
  ensures res::rbt<> & res!=null;
{
  node5 tmp;

  tmp = new node5(0, 1, b, c);

  return new node5(0, 0, a, tmp);
}


/* Rotate to transform case 2 into case 3, then apply case 3 */
node5 case_2(node5 a, node5 b, node5 c, node5 d)
  requires a::rbt<> * b::rbt<> * c::rbt<> * d::rbt<>
  ensures res::rbt<> & res!=null;
{
  node5 tmp;

  tmp = new node5(0, 1, a, b);

  return rotate_case_3(tmp, c, d);
}

/* Rotate right case 3 */
node5 rotate_case_3r(node5 a, node5 b, node5 c)
  requires a::rbt<> * b::rbt<> * c::rbt<> & c!=null
  ensures res::rbt<> & res!=null;
{
  node5 tmp;

  tmp = new node5(0, 1, a, b);

  return new node5(0, 0, tmp, c);
}

/* Rotate to transform case 2 into case 3, then apply case 3 */
node5 case_2r(node5 a, node5 b, node5 c, node5 d)
  requires a::rbt<> * b::rbt<> * c::rbt<> * d::rbt<>
  ensures res::rbt<> & res!=null;
{
  node5 tmp;

  tmp = new node5(0, 1, c, d);

  return rotate_case_3r(a, b, tmp);
}

/* Case 6 (simple rotation) */
node5 del_6(node5 a, node5 b, node5 c, int color)
  requires a::rbt<> * b::rbt<> * c::rbt<> & (color=0 | color=1) & c!=null
  ensures res::rbt<> & (color=0 | color=1) & res!=null;
{
  node5 tmp;

  c.color = 0;
  tmp = new node5(0, 0, a, b);

  return new node5(0, color, tmp, c);
}

/* Case 6 (simple rotation) - right child */
node5 del_6r(node5 a, node5 b, node5 c, int color)
  requires a::rbt<> * b::rbt<> * c::rbt<> & (color=0 | color=1) & null!=a
  case {
    color=0 -> ensures res::rbt<>;
    color!=0 -> ensures res::rbt<>;
  }
{
  node5 tmp;

  a.color = 0;
  tmp = new node5(0, 0, b, c);

  return new node5(0, color, a, tmp);
}

/* Case 5 (double rotation) */
node5 del_5(node5 a, node5 b, node5 c, node5 d, int color)
  requires a::rbt<> * b::rbt<> * c::rbt<> * d::rbt<> & (color=0 | color=1)
  ensures res::rbt<> & (color=0 | color=1) & res!=null;
{
  node5 tmp;

  tmp = new node5(0, 1, c, d);

  return del_6(a, b, tmp, color);
}

/* Case 5(double rotation) - right child */
node5 del_5r(node5 a, node5 b, node5 c, node5 d, int color)
  requires a::rbt<> * b::rbt<> * c::rbt<> * d::rbt<> & (color=0 | color=1)
  ensures res::rbt<> & (color=0 | color=1) & res!=null;
{
  node5 tmp;

  tmp = new node5(0, 1, a, b);
  return del_6r(tmp, c, d, color);
}

/* Case 4 (just recolor) */
node5 del_4(node5 a, node5 b, node5 c)
  requires a::rbt<> * b::rbt<> * c::rbt<>
  ensures res::rbt<> & res!=null;
{
  node5 tmp1,tmp2;
  tmp1 = new node5(0, 1, b, c);
  tmp2 = new node5(0, 0, a, tmp1);
  return tmp2;
}

/* Case 4 (just recolor) - right child */
node5 del_4r(node5 a, node5 b, node5 c)
  requires a::rbt<> * b::rbt<> * c::rbt<>
  ensures res::rbt<> & res!=null;
{
  node5 tmp;

  tmp = new node5(0, 1, a, b);

  return new node5(0, 0, tmp, c);
}

/* Case 3 (just recolor) */
node5 del_3(node5 a, node5 b, node5 c)
  requires a::rbt<> * b::rbt<> * c::rbt<>
  ensures res::rbt<> & res!=null;
{
  node5 tmp;

  tmp = new node5(0, 1, b, c);

  return new node5(0, 0, a, tmp);
}

/* Case 3 (just recolor) - right child */
node5 del_3r(node5 a, node5 b, node5 c)
  requires a::rbt<> * b::rbt<> * c::rbt<>
  ensures res::rbt<> & res!=null;
{
  node5 tmp;

  tmp = new node5(0, 1, a, b);

  return new node5(0, 0, tmp, c);
}

/* Case 2 (simple rotation + applying one of the cases 4, 5, 6) */
node5 del_2(node5 a, node5 b, node5 c)
  requires a::rbt<> * b::rbt<> * c::rbt<> & b != null
  ensures res::rbt<> & res!=null;
{
  node5 tmp;

  if (is_black(b.right))
  {
    if (is_black(b.left))
      tmp = del_4(a, b.left, b.right);
    else
      tmp = del_5(a, b.left.left, b.left.right, b.right, 1);
  }
  else
    tmp = del_6(a, b.left, b.right, 1);
  return new node5(0, 0, tmp, c);
}


/* Case 2 (simple rotation + applying one of the cases 4, 5, 6) - right child */
node5 del_2r(node5 a, node5 b, node5 c)
  requires a::rbt<> * b::rbt<> * c::rbt<> & b != null
  ensures res::rbt<> & res!=null;
{
  node5 tmp, f;

  if (is_black(b.left))
  {
    if (is_black(b.right))
      tmp = del_4r(b.left, b.right, c);
    else
      tmp = del_5r(b.left, b.right.left, b.right.right, c, 1);
  }
  else
    tmp = del_6r(b.left, b.right, c, 1);
  f = new node5(0, 0, a, tmp);
  return f;
}

int bh(node5 x)
  requires true
  ensures false;

/* Delete the smallest element in a rbt and then rebalance */
int remove_min(ref node5 x)
  requires x::rbt<> & x != null
  ensures x'::rbt<>;
{
  int v1;

  if (x.left == null)
  {
    int tmp = x.val;

    if (is_red(x.right))
      x.right.color = 0;
    x = x.right;

    return tmp;
  }
  else
  {
    v1 = remove_min(x.left);

    //rebalance
    if (bh(x.left) < bh(x.right))
    {
      if (is_black(x.left))
      {
        if (is_red(x.right))
        {
          if (x.right.left != null)
          {
            x = del_2(x.left, x.right.left, x.right.right);
            return v1;
          }
          else
            return v1;
        }
        else
        {
          if (is_black(x.right.right))
          {
            if (is_black(x.right.left))
              if (x.color == 0)
              {
                x = del_3(x.left, x.right.left, x.right.right);
                return v1;
              }
              else
              {
                x = del_4(x.left, x.right.left, x.right.right);
                return v1;
              }
            else
            {
              x = del_5(x.left, x.right.left.left, x.right.left.right, x.right.right, x.color);
              return v1;
            }
          }
          else
          {
              x = del_6(x.left, x.right.left, x.right.right, x.color);
              return v1;
          }
        }
      }
      else
        return v1;
    }
    else
      return v1;
  }
}

/* Delete an element with value a in a red black tree */
void del(ref node5 x, int a)
  requires x::rbt<>
  ensures  x'::rbt<>;
{
  int v;

  if (x!=null)
  {  
    if (x.val == a)
    {
      if (x.right == null)
      {
        if (is_red(x.left))
          x.left.color = 0;
        x = x.left;
      }
      else
      {
        v = remove_min(x.right);
        if (bh(x.right) < bh(x.left))
        {
          if (is_black(x.right))
          {
            if (is_red(x.left))
            {
              if (x.left.right != null)
                x = del_2r(x.left.left, x.left.right, x.right);
            }
            else
            {
              if (is_black(x.left.left))
                if (is_black(x.left.right))
                  if (x.color == 0)
                    x = del_3r(x.left.left, x.left.right, x.right);
                  else
                    x = del_4r(x.left.left, x.left.right, x.right);
                else
                  x = del_5r(x.left.left, x.left.right.left, x.left.right.right, x.right, x.color);
              else
                x = del_6r(x.left.left, x.left.right, x.right, x.color);
            }
          }
        }
      }
    }
    else
    {
      if (x.val < a) //go right
      {
        del(x.right, a);

        // rebalance
        if (bh(x.right) < bh(x.left))
        {
          if (is_black(x.right))
            if (is_red(x.left))
            {
              if (x.left.right != null)
                x = del_2r(x.left.left, x.left.right, x.right);
            }
            else
            {
              if (is_black(x.left.left))
                if (is_black(x.left.right))
                  if (x.color == 0)
                    x = del_3r(x.left.left, x.left.right, x.right);
                  else
                    x = del_4r(x.left.left, x.left.right, x.right);
                else
                  x = del_5r(x.left.left, x.left.right.left, x.left.right.right, x.right, x.color);
              else
                x = del_6r(x.left.left, x.left.right, x.right, x.color);
            }
        }
      }
      else   // go left
      {
        del(x.left, a);
        // rebalance
        if (bh(x.left) < bh(x.right))
        {
          if (is_black(x.left))
            if (is_red(x.right))
            {
              if (x.right.left != null)
                x = del_2(x.left, x.right.left, x.right.right);
            }
            else
            {
              if (is_black(x.right.right))
                if (is_black(x.right.left))
                {
                  if (x.color == 0)
                    x = del_3(x.left, x.right.left, x.right.right);
                  else
                    x = del_4(x.left, x.right.left, x.right.right);
                }
                else
                  x = del_5(x.left, x.right.left.left, x.right.left.right, x.right.right, x.color);
              else
                x = del_6(x.left, x.right.left, x.right.right, x.color);
            }
        }
      }
    }
  }
}

node5 node_error() 
  requires true 
  ensures false;

node5 insert(node5 x, int v)
  requires x::rbt<>
  ensures res::rbt<> & res != null;
{
  node5 tmp, tmp_null = null;

  if (x == null)
    return new node5(v, 1, tmp_null, tmp_null);
  else
  {
    if (v <= x.val)
    { // left
      tmp = x.left;
      x.left = insert(tmp, v);
      // rebalance
      if (x.color == 0)
      {
        if (is_red(x.left))
        {
          if (is_red(x.left.left))
          {
            if (is_red(x.right))
            {
              x.left.color = 0;
              x.right.color = 0;
              return x;
            }
            else
            {
              x = rotate_case_3(x.left.left, x.left.right, x.right);
              return x;
            }
          }
          else
          {
            if (is_red(x.left.right))
            {
              if (is_red(x.right))
              {
                x.left.color = 0;
                x.right.color = 0;
                return x;
              }
              else
              {
                x = case_2(x.left.left, x.left.right.left, x.left.right.right, x.right);
                return x;
              }
            }
            else
              return node_error();
          }
        }
        else
          return node_error();
      }
      else
        return node_error();
    }
    else
    { // right
      tmp = x.right;
      x.right = insert(tmp, v);

      // rebalance
      if (x.color == 0)
      {
        if (is_red(x.right))
        {
        if (is_red(x.right.left))
          if (is_red(x.left))
          {
            x.left.color = 0;
            x.right.color = 0;
            return x;
          }
          else
          {
            x = case_2r(x.left, x.right.left.left, x.right.left.right, x.right.right);
            return x;
          }
          else
          {
            if (is_red(x.right.right))
              if (is_red(x.left))
              {
                x.left.color = 0;
                x.right.color = 0;
                return x;
              }
              else
              {
                x = rotate_case_3r(x.left, x.right.left, x.right.right);
                return x;
              }
            else
              return node_error();
          }
        }
        else
          return node_error();
      }
      else
        return node_error();
    }
  }
}
