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

; data Sll_t {
;        Sll_t f;
; }.

(define-fun ls ((?in Sll_t) (?out Sll_t)) Space
(tospace (or (= ?in ?out)
(exists ((?u Sll_t))
(tobool
(ssep (pto ?in (ref f ?u)) (ls ?u ?out)
))))))

; pred ls<out> == self=out
;   or (exists u: self::Sll_t<f:u> * u::ls<out> ).

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
(declare-fun const_0 () Sll_t)
(declare-fun const_1 () Sll_t)
(declare-fun const_2 () Sll_t)
(declare-fun const_3 () Sll_t)
(declare-fun const_4 () Sll_t)
(declare-fun const_5 () Sll_t)
(declare-fun alpha0 () SetLoc)
(declare-fun alpha1 () SetLoc)
(declare-fun alpha2 () SetLoc)
(declare-fun alpha3 () SetLoc)
(declare-fun alpha4 () SetLoc)
(assert
  (and 
    (= nil nil)
    (tobool (ssep (pto x_emp (ref f y_emp)) (pto z_emp (ref f t_emp))))
  )
)
(assert
  (not
    (and (= const_1 const_1 )
    (tobool (ssep (pto x_emp (ref f y_emp)) (pto z_emp (ref f t_emp))))
)))

(check-sat)

; checkentail x_emp::Sll_t<y_emp> * z_emp::Sll_t<t_emp> 
;            & null=null 
;         |- x_emp::Sll_t<y_emp> * z_emp::Sll_t<t_emp> 
;            & const_1 = const_1.

==============================================================================

data Sll_t {
        Sll_t f;
}.

pred ls<out> == self=out
  or (exists u: self::Sll_t<f:u> * u::ls<out> ).

checkentail x_emp::Sll_t<y_emp> 
            * z_emp::Sll_t<t_emp> 
            & null=null 
         |- x_emp::Sll_t<y_emp> 
            * z_emp::Sll_t<t_emp> 
            & const_1 = const_1.
