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





(define-fun BinListSecond ((?x GTyp)) Space
(tospace (or
emp

        (exists ((?yp GenTyp) (?xp GenTyp))

                 (and (distinct nil ?x)
                        (tobool
        (sep (pto ?x (sref  (ref f0 ?yp)  (ref f1 ?xp) ))
                (BinListSecond ?xp)
        )

                )))

) )
 )



(define-fun BinTree ((?x GTyp)) Space
(tospace (or
emp

        (exists ((?yp GenTyp) (?xp GenTyp))

                 (and (distinct nil ?x)
                        (tobool
        (sep (pto ?x (sref  (ref f0 ?yp)  (ref f1 ?xp) ))
                (BinTree ?yp)
                (BinTree ?xp)
        )

                )))

) )
 )

;;;BinListSecond(x) |- BinTree(x)                          

(define-fun alpha2 () SetLoc)

(define-fun x () GenTyp)

(assert (tobool
        (index alpha1 (BinListSecond x))
))

(assert (not (tobool
        (index alpha2 (BinTree x))
)))




(check-sat)

