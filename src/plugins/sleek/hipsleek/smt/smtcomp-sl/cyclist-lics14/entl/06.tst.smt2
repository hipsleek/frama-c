(set-logic QF_S)
(set-info :source |
  James Brotherston, Carsten Fuhs, Nikos Gorogiannis, and Juan Navarro Pérez.
  A decision procedure for satisfiability in separation logic with inductive
  predicates. To appear at CSL-LICS, 2014.
  https://github.com/ngorogiannis/cyclist
|)
(set-info :smt-lib-version 2.0)
(set-info :category "crafted")
(set-info :status unknown)



;generic sort

(declare-sort GTyp 0)

;generic fields
(declare-fun f0 () (Field GTyp GTyp))
(declare-fun f1 () (Field GTyp GTyp))




(define-fun PeList ((?x GTyp) (?y GTyp)) Space
(tospace (or

        (= ?x ?y)


        (exists ((?xp GenTyp))

                 (and (distinct nil ?x)
                        (tobool
        (sep (pto ?x  (ref f0 ?xp) )
                (PeList ?xp ?y)
        )

                )))

) )
 )

;;;PeList(x,y) * PeList(y,z) |- PeList(x,z)             

(define-fun alpha2 () SetLoc)
(define-fun alpha3 () SetLoc)

(define-fun x () GenTyp)
(define-fun y () GenTyp)
(define-fun z () GenTyp)

(assert (tobool (sep
        (index alpha1 (PeList x y))
        (index alpha2 (PeList y z))
)))

(assert (not (tobool
        (index alpha3 (PeList x z))
)))



(check-sat)

