HeapPred HP2(node v0').
HeapPred HP0(node v1, node v0).
HeapPred HP(node v0').

append:SUCCESS[
ass [H1,G2][]: {
 x::node<val,v>@M * G2(v,y)&v!=null & y=null -->  G2(x,y)&true;
 HP(v)&v!=null -->  H1(v)&true;
 H1(x)&true -->  x::node<val,next>@M * HP(next)&true;
 x::node<val,y>@M&y=null -->  G2(x,y)&true;
 HP(v)&v=null -->  emp&true
}
hpdefs [H1,G2][]: {
 H1(x)&true -->  x::node<val,next>@M * HP2(next)&true;
 G2(x,y0)&true -->  x::node<val,y>@M * HP0(y,y0)&y0=null;
 HP(v)&true -->  
 v::node<val,next2>@M * next2::node<val,next>@M&next=null
 or v::node<val,next2>@M * next2::node<val,next0>@M * 
    next0::node<val,next1>@M&next1=null
 or v::node<val,next2>@M * next2::node<val,next3>@M * 
    next3::node<val,next4>@M&next4=null
 or v::node<val,next5>@M&next5=null
 or v::node<val,next6>@M * next6::node<val,next7>@M&next7=null
 or v::node<val,next8>@M * next8::node<val,next9>@M&next9=null
 or emp&v=null
 ;
 HP2(next)&true -->  
 emp&next=null
 or next::node<val,next0>@M * HP2(next0)&true
 ;
 HP0(y,y1)&true -->  
 emp&y=y1
 or y::node<val,y0>@M * HP0(y0,y1)&true
 
}
]

