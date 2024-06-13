void foo (ref int x, ref int y, ref int z)
 infer [@term]
 requires true
 ensures true;
{
  if (x>0) {
    x = x+y;
    y = y+z;
    z = z-1;
    foo(x,y,z);
  }
}

/*
  ex3

void foo (ref int x, ref int y, int z)
 infer [@term]
 requires true
 ensures true;
{
  if (x>0) {
    x = x+y;
    y = y+z;
    foo(x,y,z);
  }
}

This example is from CAV13. 
Why did we get half-completed:
Base/Rec Case Splitting:

  [	foo: [[2] x<=0@B,[3] 1<=x@R]
 ]

Temporal Assumptions:
 termAssume !(v_bool_6_1315') & x'<=0 & z'=z & y'=y & 
x'=x --> foopost_1357(x,y,z).

 termAssume v_bool_6_1315' & 0<x_1369 & z'=z & y_1374=y & x_1369=x & 
x_1383=y_1374+x_1369 & y_1384=z'+
y_1374 & foopost_1357(x_1383,y_1384,z') --> foopost_1357(x,y,z).

 termAssume v_bool_6_1315' & 0<x_1369 & z'=z & y_1374=y & x_1369=x & 
x'=y_1374+x_1369 & y'=z'+y_1374 & foopre_0(x,y,z) --> foopre_0(x',y',z').


*/
