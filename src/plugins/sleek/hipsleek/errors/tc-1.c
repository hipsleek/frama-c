global int globA;
global int globB;


int alt(ref int globA)
  requires  true
  ensures  true;
{
  int a = globA+1;
  return a;
}


int main(int argc, ref int globA)
  requires  true
  ensures  true;
{

  //assume(globA >= 0 & globA <=3);
    alt(globA);
    return 0;
}
