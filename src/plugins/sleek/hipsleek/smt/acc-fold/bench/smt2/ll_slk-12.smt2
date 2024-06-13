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
















(declare-fun n () Int)
(declare-fun x () node)


(assert 
(and 
(> n 4)
(tobool (ssep 
(ll x n)
emp
) )
)
)

(assert (not 
(exists ((m Int)(q node))(and 
(> m 0)
(tobool (ssep 
(pto x  (ref nxt q))
(ll q m)
emp
) )
))
))

(check-sat)