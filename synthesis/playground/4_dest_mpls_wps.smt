(set-logic UFBV)
(define-sort Addr () (_ BitVec 2))
(define-sort Loc () (_ BitVec 2))
(define-sort Tag () (_ BitVec 1))
(define-sort Index () (_ BitVec 3))

(declare-fun f000 (Addr Addr Addr Addr Addr Addr Addr Addr Addr) Tag)
(declare-fun f001 (Addr Addr Addr Addr Addr Addr Addr Addr Addr) Tag)
(declare-fun f010 (Addr Addr Addr Addr Addr Addr Addr Addr Addr) Tag)
(declare-fun f011 (Addr Addr Addr Addr Addr Addr Addr Addr Addr) Tag)
(declare-fun f100 (Addr Addr Addr Addr Addr Addr Addr Addr Addr) Tag)
(declare-fun f101 (Addr Addr Addr Addr Addr Addr Addr Addr Addr) Tag)
(declare-fun f110 (Addr Addr Addr Addr Addr Addr Addr Addr Addr) Tag)
(declare-fun f111 (Addr Addr Addr Addr Addr Addr Addr Addr Addr) Tag)


(define-fun implements () Bool
  (forall ((dest Addr) (gamma Loc)
	   (?1 Addr) (?2 Addr) (?3 Addr) (?4 Addr) (?5 Addr) (?6 Addr) (?7 Addr) (?8 Addr) (?9 Addr))
	  (=>
	   (and
	    (or                                
	     (and (= dest ?1 ?3) (= gamma #b00 ?9)) 
	     (and (= dest ?1 ?4) (= gamma #b01 ?9))
	     (and (= dest ?2 ?3) (= gamma #b00 ?9))
	     (and (= dest ?2 ?4) (= gamma #b01 ?9))
	     (and (= dest ?5 ?7) (= gamma #b10 ?9))
	     (and (= dest ?5 ?8) (= gamma #b11 ?9))
	     (and (= dest ?6 ?7) (= gamma #b10 ?9))
	     (and (= dest ?6 ?8) (= gamma #b11 ?9))
	    ))
	   (or                                      ;; dest @ loc
	    (and (= dest #b00) (= gamma #b00) ; 0 @ 0
		 (= #b0 (f000 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		 (= #b0 (f001 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
	    (and (= dest #b00) (= gamma #b01) ; 0 @ 1
		 (= #b0 (f000 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		 (= #b1 (f001 ?1 ?2 ?3 ?4 ?5 ?7 ?7 ?8 ?9)))
	    (and (= dest #b00) (= gamma #b10) ; 0 @ 2
		 (= #b1 (f000 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		 (= #b0 (f001 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
	    (and (= dest #b00) (= gamma #b11) ; 0 @ 3
		 (= #b1 (f000 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		 (= #b1 (f001 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
	    (and (= dest #b01) (= gamma #b00) ; 1 @ 0
		 (= #b0 (f010 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		 (= #b0 (f011 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
	    (and (= dest #b01) (= gamma #b01) ; 1 @ 1
		 (= #b0 (f010 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		 (= #b1 (f011 ?1 ?2 ?3 ?4 ?5 ?7 ?7 ?8 ?9)))
	    (and (= dest #b01) (= gamma #b10) ; 1 @ 2
		 (= #b1 (f010 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		 (= #b0 (f011 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
	    (and (= dest #b01) (= gamma #b11) ; 1 @ 3
		 (= #b1 (f010 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		 (= #b1 (f011 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
	    (and (= dest #b10) (= gamma #b00) ; 2 @ 0
		 (= #b0 (f100 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		 (= #b0 (f101 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
	    (and (= dest #b10) (= gamma #b01) ; 2 @ 1
		 (= #b0 (f100 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		 (= #b1 (f101 ?1 ?2 ?3 ?4 ?5 ?7 ?7 ?8 ?9)))
	    (and (= dest #b10) (= gamma #b10) ; 2 @ 2
		 (= #b1 (f100 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		 (= #b0 (f101 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
	    (and (= dest #b10) (= gamma #b11) ; 2 @ 3
		 (= #b1 (f100 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		 (= #b1 (f101 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
	    (and (= dest #b11) (= gamma #b00) ; 3 @ 0
		 (= #b0 (f110 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		 (= #b0 (f111 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
	    (and (= dest #b11) (= gamma #b01) ; 3 @ 1
		 (= #b0 (f110 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		 (= #b1 (f111 ?1 ?2 ?3 ?4 ?5 ?7 ?7 ?8 ?9)))
	    (and (= dest #b11) (= gamma #b10) ; 3 @ 2
		 (= #b1 (f110 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		 (= #b0 (f111 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
	    (and (= dest #b11) (= gamma #b11) ; 3 @ 3
		 (= #b1 (f110 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		 (= #b1 (f111 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))))))
;; (simplify implements)
(assert implements)
;; (apply (using-params qe :qe-nonlinear true))
(check-sat-using (then simplify (using-params qe :qe-nonlinear true) bit-blast smt))
(get-model)
