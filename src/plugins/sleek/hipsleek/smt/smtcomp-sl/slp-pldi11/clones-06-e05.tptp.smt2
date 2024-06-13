(set-logic QF_S)
(set-info :source |
A. Rybalchenko and J. A. Navarro Pérez.
[Separation Logic + Superposition Calculus = Heap Theorem Prover]
[PLDI 2011]
http://navarroj.com/research/papers.html#pldi11
|)
(set-info :smt-lib-version 2.0)
(set-info :category "random") 
(set-info :status unsat)


(declare-sort Sll_t 0)

(declare-fun f () (Field Sll_t Sll_t))

(define-fun ls ((?in Sll_t) (?out Sll_t)) Space
(tospace (or (= ?in ?out)
(exists ((?u Sll_t))
(tobool
(ssep (pto ?in (ref f ?u)) (ls ?u ?out)
))))))

(declare-fun nil () Sll_t)

(declare-fun x_emp () Sll_t)
(declare-fun y_emp () Sll_t)
(declare-fun z_emp () Sll_t)
(declare-fun t_emp () Sll_t)
(declare-fun x0 () Sll_t)
(declare-fun x1 () Sll_t)
(declare-fun x2 () Sll_t)
(declare-fun x3 () Sll_t)
(declare-fun x4 () Sll_t)
(declare-fun x5 () Sll_t)
(declare-fun x6 () Sll_t)
(declare-fun x7 () Sll_t)
(declare-fun x8 () Sll_t)
(declare-fun x9 () Sll_t)
(declare-fun x10 () Sll_t)
(declare-fun x11 () Sll_t)
(declare-fun x12 () Sll_t)
(declare-fun x13 () Sll_t)
(declare-fun x14 () Sll_t)
(declare-fun x15 () Sll_t)
(declare-fun x16 () Sll_t)
(declare-fun alpha0 () SetLoc)
(declare-fun alpha1 () SetLoc)
(declare-fun alpha2 () SetLoc)
(declare-fun alpha3 () SetLoc)
(declare-fun alpha4 () SetLoc)
(assert
  (and 
    (= nil nil)
(distinct nil x1 )
(distinct nil x3 )
(distinct nil x5 )
(distinct nil x7 )
(distinct nil x9 )
(distinct nil x11 )
    (tobool  (ssep  (pto x11  (ref f x12 ) ) (ssep  (pto x9  (ref f x10 ) ) (ssep  (pto x7  (ref f x8 ) ) (ssep  (pto x5  (ref f x6 ) ) (ssep  (pto x3  (ref f x4 ) ) (ssep  (pto x1  (ref f x2 ) )(ssep (pto x_emp (ref f y_emp)) (pto z_emp (ref f t_emp))))))))))
  )
)
(assert
  (not
        (tobool  (ssep  (pto x11  (ref f x12 ) ) (ssep  (pto x9  (ref f x10 ) ) (ssep  (pto x7  (ref f x8 ) ) (ssep  (pto x5  (ref f x6 ) ) (ssep  (pto x3  (ref f x4 ) ) (ssep  (pto x1  (ref f x2 ) )(ssep (pto x_emp (ref f y_emp)) (pto z_emp (ref f t_emp))))))))))
  ))

(check-sat)
