(set-logic UFBV)

(define-sort Addr () (_ BitVec 3))

;; trying to synthesize
;; (x = 4 -> y := 3) []
;; (x = 5 -> y := 4)
;; to
;; (x = ?x -> y := ?y)
;;
;; except we're reinterpreting the real program as
;; y := (f?y x)
;; where f?y is a function depending on the value of x


(define-fun f?y! ((x Addr)
		  (?x1 Addr) (?y1 Addr)
		  (?x2 Addr) (?y2 Addr))
  Addr
  (ite (= x ?x1) ?y1 ?y2))


(declare-fun f?y (Addr Addr Addr Addr Addr) Addr)

(define-fun implements () Bool
  (forall
   ((x Addr) (beta Addr)
    (?x1 Addr) (?y1 Addr) (?x2 Addr) (?y2 Addr))
   (=> (or (and (= x ?x1) (not (= x ?x2)) (= beta ?y1))
	   (and (= x ?x2) (not (= x ?x1)) (= beta ?y2)))
       (= beta (f?y x ?x1 ?y1 ?x2 ?y2)))))

(assert implements)
(check-sat-using (then
		  simplify
		  qe
		  bit-blast
		  smt))
(get-model)

(assert (forall ((x Addr) (?x1 Addr) (?y1 Addr) (?x2 Addr) (?y2 Addr))
	(= (f?y x ?x1 ?y1 ?x2 ?y2)
	   (f?y! x ?x1 ?y1 ?x2 ?y2))))

(check-sat-using (then
		  simplify
		  qe
		  bit-blast
		  smt))
;; (assert implements)
;; (check-sat)
;; (get-model)
