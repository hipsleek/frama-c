data node{
	node prev;
	node next;
}


HeapPred H(node a, node b).
HeapPred G(node a, node b).

  void set_tail (node x,node y)
  infer[H,G] 
  requires H(x,y) 
  ensures G(x,y);
{
  //node t = x.next;
  //assume t'=null;
   x.next = y;
}

/*
with #
 G(x_875,y_876) ::= HP_870(prev_17_868,y_876) * x_875::node<prev_17_868,y_876>@M,
 H(x_873,y_874) ::= x_873::node<prev_17_868,next_17_869>@M * HP_870(prev_17_868,y_874) *
HP_871(next_17_869,y_874),
 HP_870(prev_17_868,y) ::= NONE,
 HP_871(next_17_869,y) ::= NONE

Without #:

[ H(x,y) --> x::node<prev_15_778,next_15_779>@M * 
  HP_780(prev_15_778,y@NI) * HP_781(next_15_779,y@NI) 
  * HP_782(y,x@NI)&true,
 HP_780(prev_15_778,y@NI) * HP_782(y,x@NI) 
  * x::node<prev_15_778,y>@M& --> G(x,y)&true]

=============

[ H(x_784,y_785) ::=  x_784::node<prev_15_778,next_15_779>@M 
  * HP_780(prev_15_778,y_785) * HP_781(next_15_779,y_785) 
  * HP_782(y_785,x_784)&true,
 G(x_786,y_787) ::=  HP_780(prev_15_778,y_787) 
  * HP_782(y_787,x_786) * x_786::node<prev_15_778,y_787>@M&true,


============= --sa-inlining

 H1(c_795,y_796) ::= 
    c_795::node<val_16_779,UU_HP_782_UU,UU_HP_783_UU>@M&true,
 G1(c_797,y_798) ::= 
    c_797::node<val_16_779,UU_HP_782_UU,y_798>@M&true]

*/

