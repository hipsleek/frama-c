data arrI {
  int val;
}

// Predicates
arr_seg<i,n> == i=n & i>=0
  or x::arrI<_>*self::arr_seg<i+1,n> & x=self+i & i>=0
  inv n>=i & i>=0;

arr_seg_sorted<i,n,mi> == x::arrI<v> & x=self+i & i=n-1 & i>=0 & v<=mi
  or x::arrI<v>*self::arr_seg_sorted<i+1,n,m2> & x=self+i & i>=0 & i<n-1 & v<=mi & mi>=m2 & v>=m2
  inv n>i & i>=0;

arr_seg_max_2<i,n,maxv> == i=n & i>=0 //& cur<=max_value
  or x::arrI<cur> * self::arr_seg_max_2<i+1,n,maxv2> & x=self+i & i>=0 & i<n & ((!(i=n-1))|maxv=cur) & maxv=max(cur,maxv2)
  inv n>=i & i>=0;

// Primitive operations
void merge_max(arrI base,int i, int k, int n)
  requires base::arr_seg_max_2<i,k,m1> * base::arr_seg_max_2<k,n,m2>
  ensures base::arr_seg_max_2<i,n,max(m1,m2)>;

void reverse_unfold(arrI base, int i,int n)
  requires x::arrI<m1>*base::arr_seg_max_2<i+1,n,m2> & x=base+i
  ensures base::arr_seg_max_2<i,n,max(m1,m2)>;

void upd_arr(arrI base, int i, int v)
  requires a::arrI<_> & a=base+i & i>=0
  ensures a::arrI<v> & a=base+i;

arrI arr_inc(arrI a)
  requires true //a::arrI<_>@L
  ensures res=a+1;

int get_arr(arrI base, int i)
  requires a::arrI<v>@L & a=base+i
  ensures res=v;



int get_max(arrI base,int i,int m)
  requires base::arr_seg_max_2<i,m,v> & i<m // generalization
  ensures  base::arr_seg_max_2<i,res,v1> * x::arrI<v> * base::arr_seg_max_2<res+1,m,v2> & v>=v1 & v>=v2 & x=base+res;
// {
//   if(i==m-1)
//     {
//       get_arr(base,i);
//       return i;
//     }
//   else{
//     int cur = get_arr(base,i);
//     int tmp_index = get_max(base,i+1,m);
//     int tmp = get_arr(base,tmp_index);
//     if(tmp<cur)
//       {
//         merge_max(base,i+1,tmp_index,m);
//         return i;
//       }
//     else
//       {
//         reverse_unfold(base,i,tmp_index);
//         return tmp_index;
//       }
//   }
// }

void selection_sort(arrI base, int i, int m)
  requires base::arr_seg_max_2<i,m,maxv> & i<m
  ensures base::arr_seg_sorted<i,m,maxv>;
{
  if(i==m-1){

    get_arr(base,i); // Just to enforce the unfolding
    return;
  }
  else{

    int val = get_arr(base,i);
    int tmp_index = get_max(base,i+1,m);
    int tmp = get_arr(base,tmp_index);

    if(tmp>val){
      upd_arr(base,tmp_index,val);
      upd_arr(base,i,tmp);
    }
    reverse_unfold(base,tmp_index,m);
    merge_max(base,i+1,tmp_index,m);
    selection_sort(base,i+1,m);
    return;
  }
}
