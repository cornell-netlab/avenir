(define-sort Addr () (_ BitVec 2))
(define-sort Loc () (_ BitVec 3))
(define-sort Tag () (_ BitVec 1))
(define-sort Index () (_ BitVec 1))

;; Real Hole Function
(declare-fun f (Addr Index Addr Addr Addr Addr Addr Addr Addr Addr) Tag)

;; Precondition for the logical dataplane
(define-fun lpath ((pdest Addr) (?x Addr) (?y Addr) (gamma Loc) (loc Loc)) Bool
  (and (= pdest ?x) (= pdest ?y) (= gamma loc)))

(define-fun logical_prec ((ldest Addr) (lgamma Loc)
			  (?1 Addr) (?2 Addr)
			  (?3 Addr) (?4 Addr)
			  (?5 Addr) (?6 Addr)
			  (?7 Addr) (?8 Addr)) Bool
   (or (lpath ldest ?1 ?3 lgamma #b100)
       (lpath ldest ?1 ?4 lgamma #b101)
       (lpath ldest ?2 ?3 lgamma #b100)
       (lpath ldest ?2 ?4 lgamma #b101)
       (lpath ldest ?5 ?7 lgamma #b110)
       (lpath ldest ?5 ?8 lgamma #b111)
       (lpath ldest ?6 ?7 lgamma #b110)
       (lpath ldest ?6 ?7 lgamma #b111)))
	 
;; Precondition for the real dataplane
(define-fun rpath ((dest Addr) (i Addr) (?i1 Tag) (?i2 Tag) (gamma Loc)) Bool
  (and (= dest i)
       (or 
           (and (= ?i1 #b0) (= ?i2 #b0) (= gamma #b100))
	   (and (= ?i1 #b0) (= ?i2 #b1) (= gamma #b101))
	   (and (= ?i1 #b1) (= ?i2 #b0) (= gamma #b110))
	   (and (= ?i1 #b1) (= ?i2 #b1) (= gamma #b111)))))

(define-fun real_prec ((rdest Addr) (gamma Loc)
		       (?1 Addr) (?2 Addr)
		       (?3 Addr) (?4 Addr)
		       (?5 Addr) (?6 Addr)
		       (?7 Addr) (?8 Addr)) Bool
  (or (rpath rdest #b00 (f #b00 #b1 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) (f #b00 #b0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) gamma)
      (rpath rdest #b01 (f #b01 #b1 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) (f #b01 #b0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) gamma)
      (rpath rdest #b10 (f #b10 #b1 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) (f #b10 #b0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) gamma)
      (rpath rdest #b11 (f #b11 #b1 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) (f #b11 #b0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8) gamma))
  )

;; implements
(define-fun implements ((?1 Addr) (?2 Addr) (?3 Addr) (?4 Addr) (?5 Addr) (?6 Addr) (?7 Addr) (?8 Addr)) Bool
  ;; for every real and symbolic packet
  (forall ((dest Addr) (gamma Loc))
  	    (implies 
               (logical_prec dest gamma ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8)
	       (real_prec dest gamma ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8))))

(assert (forall ((?1 Addr) (?2 Addr) (?3 Addr) (?4 Addr) (?5 Addr) (?6 Addr) (?7 Addr) (?8 Addr))
  (implements ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8)))
(check-sat)
(get-model)
