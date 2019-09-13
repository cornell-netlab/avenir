;; ;; Logical Holes
;; (declare-const ?d2 Int)
;; (declare-const ?d3 Int)
;; Real Hole Function
(declare-fun f (Int Int Int) (Int))
;; (declare-const ??d1 Int)
;; (declare-const ??d2 Int)
;; Precondition for the logical dataplane
(define-fun logical_prec ((ldest Int) (lgamma Int) (?d2 Int) (?d3 Int)) Bool
  (or (and (= ldest ?d2) (= lgamma 2))
      (and (= ldest ?d3) (= lgamma 3))))

;; Precondition for the real dataplane
(define-fun real_prec ((rdest Int) (rgamma Int) (?d2 Int) (?d3 Int)) Bool
  (or (and (= rdest 101) (= (f 101 ?d2 ?d3) 0) (= rgamma 2))
      (and (= rdest 101) (= (f 101 ?d2 ?d3) 1) (= rgamma 3))
      (and (= rdest 102) (= (f 102 ?d2 ?d3) 0) (= rgamma 2))
      (and (= rdest 102) (= (f 102 ?d2 ?d3) 1) (= rgamma 3))))

;; implements
(define-fun implements ((?d2 Int) (?d3 Int)) Bool
  (forall ((adest Int) (agamma Int))
	   (implies
	    (or (= adest 101) (= adest 102))
	    (implies
	     (logical_prec adest agamma ?d2 ?d3)
	     (real_prec adest agamma ?d2 ?d3)))))
;; make sure the domain of f is 0 and 1
(assert (forall ((x Int) (y Int) (z Int))
		(or (= (f x y z) 0) (= (f x y z) 1))))
;; the condition to check
(assert (forall ((?d2 Int) (?d3 Int))	
		(implies
		 (and (or (= ?d2 101) (= ?d2 102))
		      (or (= ?d3 101) (= ?d3 102))
		      (not (= ?d2 ?d3)))
		 (implements ?d2 ?d3))))
;; (not implements)))
(check-sat)
(get-model)
