(set-logic QF_S)
(set-info :source |
A. Rybalchenko and J. A. Navarro Pérez.
[Separation Logic + Superposition Calculus = Heap Theorem Prover]
[PLDI 2011]
http://navarroj.com/research/papers.html#pldi11
|)
(set-info :smt-lib-version 2.0)
(set-info :category "random") 
(set-info :status sat)


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
(declare-fun x17 () Sll_t)
(declare-fun x18 () Sll_t)
(declare-fun x19 () Sll_t)
(declare-fun x20 () Sll_t)
(declare-fun x21 () Sll_t)
(declare-fun x22 () Sll_t)
(declare-fun x23 () Sll_t)
(declare-fun alpha0 () SetLoc)
(declare-fun alpha1 () SetLoc)
(declare-fun alpha2 () SetLoc)
(declare-fun alpha3 () SetLoc)
(declare-fun alpha4 () SetLoc)
(declare-fun alpha5 () SetLoc)
(declare-fun alpha6 () SetLoc)
(declare-fun alpha7 () SetLoc)
(declare-fun alpha8 () SetLoc)
(declare-fun alpha9 () SetLoc)
(declare-fun alpha10 () SetLoc)
(declare-fun alpha11 () SetLoc)
(declare-fun alpha12 () SetLoc)
(declare-fun alpha13 () SetLoc)
(declare-fun alpha14 () SetLoc)
(declare-fun alpha15 () SetLoc)
(declare-fun alpha16 () SetLoc)
(declare-fun alpha17 () SetLoc)
(declare-fun alpha18 () SetLoc)
(declare-fun alpha19 () SetLoc)
(declare-fun alpha20 () SetLoc)
(declare-fun alpha21 () SetLoc)
(declare-fun alpha22 () SetLoc)
(declare-fun alpha23 () SetLoc)
(declare-fun alpha24 () SetLoc)
(declare-fun alpha25 () SetLoc)
(assert
  (and 
    (= nil nil)
    (tobool  (ssep  (index alpha0 (ls x14 x18 )) (ssep  (pto x3  (ref f x7 ) ) (ssep  (pto x9  (ref f x4 ) ) (ssep  (index alpha1 (ls x6 x1 )) (ssep  (index alpha2 (ls x19 x3 )) (ssep  (pto x5  (ref f x11 ) ) (ssep  (pto x4  (ref f x13 ) ) (ssep  (pto x18  (ref f x19 ) ) (ssep  (index alpha3 (ls x17 x3 )) (ssep  (pto x1  (ref f x18 ) ) (ssep  (index alpha4 (ls x7 x10 )) (ssep  (pto x11  (ref f x2 ) ) (ssep  (pto x15  (ref f x3 ) ) (ssep  (pto x16  (ref f x4 ) ) (ssep  (index alpha5 (ls x12 x6 )) (ssep  (index alpha6 (ls x10 x3 )) (ssep  (pto x13  (ref f x19 ) ) (ssep  (index alpha7 (ls x2 x3 )) (ssep  (index alpha8 (ls x8 x14 ))(ssep (pto x_emp (ref f y_emp)) (pto z_emp (ref f t_emp)))))))))))))))))))))))
  )
)
(assert
  (not
        (tobool  (ssep  (index alpha9 (ls x15 x3 )) (ssep  (index alpha10 (ls x17 x3 )) (ssep  (index alpha11 (ls x12 x18 )) (ssep  (index alpha12 (ls x9 x4 )) (ssep  (index alpha13 (ls x16 x4 )) (ssep  (index alpha14 (ls x4 x13 )) (ssep  (index alpha15 (ls x5 x2 )) (ssep  (index alpha16 (ls x8 x18 )) (ssep  (index alpha17 (ls x13 x19 )) (ssep  (index alpha18 (ls x10 x3 )) (ssep  (index alpha19 (ls x18 x3 )) (ssep  (index alpha20 (ls x2 x10 ))(ssep (pto x_emp (ref f y_emp)) (pto z_emp (ref f t_emp))))))))))))))))
  ))

(check-sat)
