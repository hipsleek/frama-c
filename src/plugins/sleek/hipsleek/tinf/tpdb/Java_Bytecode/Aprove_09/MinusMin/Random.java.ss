
global String[] Random_args;

global int Random_index = 0;
data Random
{

}
 int Random_random()
{
  String string = Random_args[Random_index];
  Random_index++;
  return String_length();
}



data MinusMin
{

}
 int MinusMin_min(int x, int y)
{
  if (x < y)
    return x;
  else
    return y;
}

void MinusMin_main(String[] args)
{
  Random_args = args;
  int x = Random_random();
  int y = Random_random();
  int __res = 0;
  while (MinusMin_min(x - 1, y) == y) {
    y++;
    __res++;
  }
}

global String[] Random_args;

int Random_random()
  requires true
  ensures true;

int String_length(String k)
  requires true
  ensures res >=0;