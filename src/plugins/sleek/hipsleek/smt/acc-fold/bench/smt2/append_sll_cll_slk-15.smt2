(set-logic QF_S)

(declare-sort node 0)
(declare-fun next () (Field node node))

(define-fun lseg ((?in node) (?p node))
Space (tospace
(or
(and 
(= ?in ?p)

)(exists ((?p_21 node)(?q_20 node))(and 
(= ?p_21 ?p)
(tobool (ssep 
(pto ?in  (ref next ?q_20))
(lseg ?q_20 ?p_21)
) )
)))))

(define-fun ll ((?in node))
Space (tospace
(or
(and 
(= ?in nil)

)(exists ((?q_22 node))(and 
(tobool (ssep 
(pto ?in  (ref next ?q_22))
(ll ?q_22)
) )
)))))

(define-fun clist ((?in node))
Space (tospace
(exists ((?self_19 node)(?p_18 node))(and 
(= ?self_19 ?in)
(tobool (ssep 
(pto ?in  (ref next ?p_18))
(lseg ?p_18 ?self_19)
) )
))))





















(declare-fun v1prm () node)
(declare-fun xprm () node)
(declare-fun vprm () boolean)
(declare-fun yprm () node)
(declare-fun y () node)
(declare-fun x () node)
(declare-fun q () node)


(assert 
(and 
(= v1prm q)
bvar(distinct q nil)
(= xprm x)
(= yprm y)
(= y x)
(distinct x nil)
(tobool (ssep 
(ll q)
(pto xprm  (ref next q))
emp
) )
)
)

(assert (not 
(and 
(= v1prm q)
bvar(distinct q nil)
(= xprm x)
(= yprm y)
(= y x)
(distinct x nil)
(distinct v1prm nil)
(tobool (ssep 
(ll v1prm)
(pto xprm  (ref next q))
emp
) )
)
))

(check-sat)