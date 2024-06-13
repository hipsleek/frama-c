void loop()
  requires Loop
  ensures false;

bool nondet()
  requires Term
  ensures true;
  
void f(int x)
  infer [@term]
  requires true
  ensures true;
{
  if (x < 0) return;
  else {
    bool b = nondet();
    if (b) 
      loop();
    else f(x - 1);
  }
}

void main ()
  infer [@term]
  requires true
  ensures true;
{
  int x;
  f(x);
}

