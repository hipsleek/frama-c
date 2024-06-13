(set-logic QF_S)

(declare-sort node 0)
(declare-fun nxt () (Field node node))

(define-fun ll ((?in node) (?n Int))
Space (tospace
(or
(and 
(= ?in nil)
(= ?n 0)

)(exists ((?a node)(?m Int))(and 
(= ?n (+ ?m 1))
(tobool (ssep 
(pto ?in  (ref nxt ?a))
(ll ?a ?m)
) )
)))))
















(declare-fun q () node)
(declare-fun q2 () node)
(declare-fun n () Int)
(declare-fun x () node)


(assert 
(and 
(tobool (ssep 
(pto x  (ref nxt q))
(pto q  (ref nxt q2))
(ll q2 n)
emp
) )
)
)

(assert (not 
(exists ((m Int))(and 
(tobool (ssep 
(ll x m)
emp
) )
))
))

(check-sat)