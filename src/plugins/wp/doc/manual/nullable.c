void foo(int* p /*@ wp_nullable */, int* q /*@ wp_nullable */){

}

// or equivalently:
//@ \wp::nullable_args p, q ;
void foo(int* p, int* q){

}
