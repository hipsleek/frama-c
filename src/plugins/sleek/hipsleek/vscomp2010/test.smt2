(set-logic AUFNIA)
(declare-fun n () Int)
(declare-fun b () (Array Int Int))
(declare-fun a () (Array Int Int))
(declare-fun b' () (Array Int Int))
(declare-fun i () Int)
(declare-fun Permute ((Array Int Int) Int Int Int Int) Bool)
(declare-fun IsLeftInverse ((Array Int Int) (Array Int Int) Int Int) Bool)
(declare-fun dom ((Array Int Int) Int Int) Bool)
(assert (forall (a (Array Int Int)) (i Int) (j Int) (l Int) (h Int) (= (Permute a i j l h) (forall (?k Int) (or (< ?k i) (or (> ?k j) (and (<= l (select a ?k)) (<= (select a ?k) h))))))))
(assert (forall (a (Array Int Int)) (b (Array Int Int)) (i Int) (j Int) (= (IsLeftInverse a b i j) (forall (?k Int) (or (< ?k i) (or (> ?k j) (= (select b (select a ?k)) ?k)))))))
(assert (forall (a (Array Int Int)) (i Int) (j Int) (= (dom a i j) true)))
;(assert (and (and (and (and (and (dom a 0 (- n 1)) (dom b 0 (- n 1))) (Permute a 0 (- n 1) 0 (- n 1))) (IsLeftInverse a b 0 (- i 1))) (< i n)) (= b' (store b (select a i) i))))
;(assert (not (IsLeftInverse a b' 0 i)))
(assert (IsLeftInverse a b 0 (- i 1)))
(assert (= (select b (select a i)) i))
(assert (not (IsLeftInverse a b 0 i)))
(check-sat)