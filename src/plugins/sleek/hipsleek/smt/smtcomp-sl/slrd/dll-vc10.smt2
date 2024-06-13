(set-logic QF_S)
(set-info :source |
C. Enea, O. Lengal, M. Sighireanu, and T. Vojnar
[Compositional Entailment Checking for a Fragment of Separation Logic]
http://www.liafa.univ-paris-diderot.fr/spen
|)
(set-info :smt-lib-version 2.0)
(set-info :category "crafted")
(set-info :status unsat)

(declare-sort Dll_t 0)

(declare-fun next () (Field Dll_t Dll_t))
(declare-fun prev () (Field Dll_t Dll_t))

; doubly-linked list
(define-fun dll ((?in Dll_t) (?ex Dll_t) (?pr Dll_t) (?hd Dll_t))
  Space (tospace (or (and (= ?in ?pr) (= ?hd ?ex)) 
    (exists ((?u Dll_t)) (tobool (ssep
      (pto ?in (sref (ref next ?u) (ref prev ?pr)))
      (dll ?u ?ex ?in ?hd))
)))))

(declare-fun x_emp () Dll_t)
(declare-fun y_emp () Dll_t)
(declare-fun z_emp () Dll_t)
(declare-fun w_emp () Dll_t)
(declare-fun u_emp () Dll_t)
(declare-fun t_emp () Dll_t)
(declare-fun alpha1 () SetLoc)
(declare-fun alpha2 () SetLoc)
(declare-fun alpha3 () SetLoc)
;
; unfolding at start of a circular dll(x,y,y,x)
; exp: unsat
;
(assert
    (tobool (ssep (pto w_emp (sref (ref next t_emp) (ref prev u_emp))) 
                  (index alpha2 (dll x_emp u_emp y_emp w_emp))
                  (index alpha3 (dll t_emp y_emp w_emp x_emp))
            )
    )
)
(assert
  (not
    (tobool (index alpha1 (dll x_emp y_emp y_emp x_emp)))
))

(check-sat)
