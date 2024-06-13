data node {
  node next;
}

ls<p> == self = p
  or self::node<q>* q::ls<p>;

clist<> == self::node<q> * q::ls<self>
  inv self!=null;

lasso<> == self::node<q>*q::lasso<>
      or self::clist<>
  inv self!=null; 

/*
# ll-back-3a.ss

This lasso data structure is not currently supported.

!!! dumping for finalize on hipStop Omega... 1 invocations caught
(Program not linked with -g, cannot print stack backtrace)
Exception occurred: Not_found

I suppose the exception is thrown when we read a
data table and found "clist" to be missing. I think
we can firslty give a better error message; and
secondly allow such examples to be accepted.

*/

