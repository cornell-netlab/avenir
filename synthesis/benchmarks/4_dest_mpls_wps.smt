;; Real Hole Function
(declare-fun f (Int Bool Int Int Int Int Int Int Int Int) (Int))

;; helper functio
(define-fun lpath ((pdest Int) (?x Int) (?y Int) (gamma Int) (loc Int)) Bool
  (and (= pdest ?x) (= pdest ?y) (= gamma loc)))

;; Precondition for the logical dataplane
(define-fun logical_prec ((ldest Int) (lgamma Int)
			  (?1 Int) (?2 Int)
			  (?3 Int) (?4 Int)
			  (?5 Int) (?6 Int)
			  (?7 Int) (?8 Int)) Bool
  (or (lpath ldest ?1 ?3 lgamma 4)
      (lpath ldest ?1 ?4 lgamma 5)
      (lpath ldest ?2 ?3 lgamma 4)
      (lpath ldest ?2 ?4 lgamma 5)
      (lpath ldest ?5 ?7 lgamma 6)
      (lpath ldest ?5 ?8 lgamma 7)
      (lpath ldest ?6 ?7 lgamma 6)
      (lpath ldest ?6 ?7 lgamma 7)))


(define-fun rpath ((dest Int) (i Int) (?i1 Int) (?i2 Int) (gamma Int)) Bool
  (and (= dest i)
       (or (and (= ?i1 0) (= ?i2 0) (= gamma 4))
	   (and (= ?i1 0) (= ?i2 1) (= gamma 5))
	   (and (= ?i1 1) (= ?i2 0) (= gamma 6))
	   (and (= ?i1 1) (= ?i2 1) (= gamma 7)))))
    
;; Precondition for the real dataplane
(define-fun real_prec ((rdest Int) (gamma Int)
		       (?1 Int) (?2 Int)
		       (?3 Int) (?4 Int)
		       (?5 Int) (?6 Int)
		       (?7 Int) (?8 Int)) Bool
  (or (rpath rdest 101 (f 101 true ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) (f 101 false ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) gamma)
      (rpath rdest 102 (f 102 true ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) (f 102 false ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) gamma)
      (rpath rdest 103 (f 103 true ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) (f 103 false ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) gamma)
      (rpath rdest 104 (f 104 true ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) (f 104 false ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) gamma))
  )

;; implements
(define-fun implements ((?1 Int) (?2 Int) (?3 Int) (?4 Int) (?5 Int) (?6 Int) (?7 Int) (?8 Int)) Bool
  ;; for every real and symbolic packet
  (forall ((dest Int) (gamma Int))
	   (implies
	    (and (or (= dest 101) (= dest 102) (= dest 103) (= dest 104))
		 (or (= gamma 4) (= gamma 5) (= gamma 6) (= gamma 7)))
	    (= (logical_prec dest gamma ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8)
	       (real_prec dest gamma ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8)))))

;; make sure the codomain of f is {0, 1}
(assert (forall ((dst Int) (x Bool) (?1 Int) (?2 Int) (?3 Int) (?4 Int) (?5 Int) (?6 Int) (?7 Int) (?8 Int))
		(or (= (f dst x ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) 0) (= (f dst x ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) 1))))

;; Makes sure x is a valid destination
(define-fun isDest ((x Int)) Bool
  (or (= x 101) (= x 102) (= x 103) (= x 104)))

;; makes sure the holes satisfy the precondition
(define-fun precondition ((?1 Int) (?2 Int) (?3 Int) (?4 Int) (?5 Int) (?6 Int) (?7 Int) (?8 Int)) Bool
  (and (isDest ?1) (isDest ?2) (isDest ?3) (isDest ?4) (isDest ?5) (isDest ?6) (isDest ?7) (isDest ?8)
       (distinct ?1 ?2 ?5 ?6)
       (distinct ?3 ?4 ?7 ?8)))
;; the condition to check
(assert (forall ((?1 Int) (?2 Int) (?3 Int) (?4 Int) (?5 Int) (?6 Int) (?7 Int) (?8 Int))
		(implies
		 (precondition ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8)
		 (not (implements ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8)))))
;; (not implements)))
;; (assert (exists ((?1 Int) (?2 Int) (?3 Int) (?4 Int) (?5 Int) (?6 Int) (?7 Int) (?8 Int))
;; 		(precondition ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8)))
(check-sat)
(get-model)
