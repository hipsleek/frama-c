data node {
  int val;
  node next;
}


HeapPred G2(node a, node b).
HeapPred G1(node a).
HeapPred G3(node b,node c, node d).

HeapPred H1(node a).
HeapPred H1a(node a).
HeapPred H1b(node a).
HeapPred H2(node a, node b).
HeapPred HP1(node a, node b).
/*
ll<> == self=null
  or self::node<_,q>*q::ll<>
  inv true;

lseg<p> == self=p
  or self::node<_,q>*q::lseg<p>
  inv true;

l2<y> == self::node<a,null> & y=self
  or self::node<_,q>*q::l2<y> 
  inv y!=null;
*/
void append(node x, node y)

  infer[H1,H1a,H1b,G2]
  requires H1(x) * H1a(y)
     ensures G2(x,y) * H1b(y);
  /*

[ HP_RELDEFN HP_613
HP_613(y_612,y) ::= 
 H1a(y)&y_612=y
 or y_612::node<val_58_611,y_616> * HP_613(y_616,y)&true
 ,
 HP_RELDEFN H1b
H1b(y) ::= htrue&true,
 HP_RELDEFN H1a
H1a(y) ::= htrue&true,
 HP_RELDEFN H1a
H1a(y) ::= H1b(y)&true,
 HP_RELDEFN H1
H1(x) ::= x::node<val_58_618,next_58_619> * next_58_619::ll[LHSCase]&true,
 HP_RELDEFN G2
G2(x,y) ::= x::node<val_58_611,y_612> * HP_613(y_612,y)&true]

   */
{
  if (x.next == null)
    x.next = y;
  else
    append(x.next, y);
}

/*
!!!    new hrel:  RELASS [H1a,HP_551,G2,H1b] unknown svl: [y];  unknown hps: [H1b; 
  H1a];  predefined: [x]; H1a(y) * HP_551(v_node_54_568) * 
  x::node<val_54_557,y>&v_node_54_568=null --> G2(x,y) * H1b(y)&true
!!!    new hrel:  RELASS [H1b] unknown svl: [y];  unknown hps: [H1b];  predefined: [x]; emp&
  true --> H1b(y)&true
!!!    new hrel:  RELASS [H1,H1a,HP_551] unknown svl: ;  unknown hps: ;  predefined: ; H1(x)&
  true --> x::node<val_54_532',next_54_533'> * HP_551(next_54_533')&true
!!!    new hrel:  RELASS [HP_551,H1,H1a] unknown svl: ;  unknown hps: ;  predefined: ; HP_551(v_node_54_574)&
  v_node_54_574!=null --> H1(v_node_54_574)&true
!!!    new hrel:  RELASS [H1a] unknown svl: [y];  unknown hps: [H1a];  predefined: ; H1a(y)&
  true --> H1a(y)&true
!!!    new hrel:  RELASS [G2,H1b] unknown svl: [y];  unknown hps: [H1b; 
  H1a];  predefined: ; x::node<val_54_559,v_node_54_574> * 
  G2(v_node_54_574,y) * H1b(y)&v_node_54_574!=null --> G2(x,y) * H1b(y)&true
!!!    new hrel:  RELASS [H1b] unknown svl: [y];  unknown hps: [H1b];  predefined: ; emp&
  true --> H1b(y)&true
 */
