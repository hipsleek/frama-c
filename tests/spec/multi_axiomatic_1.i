/* run.config
OPT: %{dep:./multi_axiomatic_2.i} -print
*/

/*@
	axiomatic ax { logic int Acc(int m); } 
	predicate Bnd(integer n,int m) = Acc(m)<=9; 
*/


