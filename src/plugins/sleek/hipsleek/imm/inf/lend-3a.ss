data cell{
 int fst;
}

//relation R(int v,int w).
relation R(int v,
   ann w).

/*
# lend-3a.slk --pcp

Why is theR relation not being printed
by --pcp

Message: ann is neither data, enum type, nor prim pred
 class cell extends Object {

infer [R] p::cell<f>@v * q::cell<2>@w & R(v,w) 
  |- p::cell<a> * q::cell<_>@L.
print residue.
*/
/*
# lend-3.slk --print-type 

@imm should pick up all relations with @ann parameters
for inference; where needed. 

Priority to relations, and then variables.

 error: Stream.Error("[infer_type] or [id_list] or CSQUARE expected after OSQUARE (in [id_list_w_itype])")
 at:(Program not linked with -g, cannot print stack backtrace)

*/
