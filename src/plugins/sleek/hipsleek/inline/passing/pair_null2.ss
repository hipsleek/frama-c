data pair {
  int x;
  int y;
}

int foo(pair p)
  requires p::pair<a,b>
  ensures p=null & res=a+1;//'
{
  int r = p.x + 1;
  p = null;
  //dprint;
  return r;
}

/*
int main()
  requires true
  ensures true;
{
  pair p = new pair(1,1);
  foo(p);
  return p.x + 1;
}
*/
