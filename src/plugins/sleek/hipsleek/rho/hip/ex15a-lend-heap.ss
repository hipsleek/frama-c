data cell { int val; }

void main()
  requires emp ensures emp;
{
  cell x = new cell(1);
  int y = 1;
  int z = 2;
  dprint;
  par {x@L,y,z} x'::cell<_>@L
  {
      case {x@L,y} x'::cell<_>@L ->
       y = y + x.val;
  || 
      case {x@L,z} x'::cell<_>@L ->
        /* x.val = z + 1; */
        z = x.val + 2;
  }
  dprint;
  assert y'=2 & z'=5;
}

/*

n/m
1/6
1/10
1/2
1/M & M=16


*/
