(set-logic AUFNIA)
(declare-fun k_37' () Int)
(declare-fun a () (Array Int Int))
(declare-fun a_260 () (Array Int Int))
(declare-fun temp_38' () Int)
(declare-fun a' () (Array Int Int))
(declare-fun i () Int)
(declare-fun j () Int)
(declare-fun temp_38 () Int)
(declare-fun upperbnd ((Array Int Int) Int Int Int) Bool)
(assert (forall (a (Array Int Int)) (i Int) (j Int) (s Int) (= (upperbnd a i j s) (or (> i j) (forall (?k Int) (or (or (< ?k i) (> ?k j)) (<= (select a ?k) s)))))))
(assert (and (and (and (and (and (and (and (and (and (<= 0 i) (<= 0 j)) (< i j)) (<= i k_37')) (<= k_37' j)) (upperbnd a i j (select a k_37'))) (<= i j)) (= temp_38' (select a k_37'))) (= a_260 (store a k_37' (select a j)))) (= a' (store a_260 j temp_38'))))
(assert (not (upperbnd a' i (- j 1) temp_38')))
(check-sat)
