void foo(int* p /*@ my_attribute */){
  int /*@ my_attribute */ v ;
}

/*@ ghost
  void bar(int* p /@ my_attribute @/){
    int /@ my_attribute @/ v ;
  }
*/
