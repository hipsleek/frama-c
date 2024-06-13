(set-logic QF_S)

(declare-sort node 0)
(declare-fun nxt () (Field node node))

(define-fun elseg ((?in node) (?p node))
Space (tospace
(or
(and 
(= ?in ?p)

)(exists ((?a node)(?b node))(and 
(tobool (ssep 
(pto ?in  (ref nxt ?a))
(pto ?a  (ref nxt ?b))
(elseg ?b ?p)
) )
)))))














(declare-fun x () node)
(declare-fun q () node)
(declare-fun z () node)
(declare-fun p () node)


(assert 
(and 
(tobool (ssep 
(pto z  (ref nxt x))
(elseg x q)
(pto q  (ref nxt p))
emp
) )
)
)

(assert (not 
(and 
(tobool (ssep 
(elseg z p)
emp
) )
)
))

(check-sat)