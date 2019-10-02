(set-logic AUFBV)

(define-sort Addr () (_ BitVec 2))
(define-sort Index () (_ BitVec 2))
(define-sort Addrs () (Array Addr Bool))
(define-sort Loc () (_ BitVec 2))

;;
;;  x = ?x && x1 = ?x1 -> y := ??y ▷ true -> skip
;;
;;  ==>>
;;
;;  (x = ??x -> y := ??y₀ ▷ true -> skip);
;;  (x1 = ??x1 -> y := ??y1 ▷ true -> skip)


;;
;; x = α ∧ x₁ = α₁ ∧ ((x = ?x ∧ x₁ = ?x₁ ⇒ β = ?y) ∧ (x ≠ ?x ∧ x₁ ≠ ?x₁ ⇒ y = β))
;; ⇒
;;
;; x = α ∧ x₁ = α₁ ∧ (x = ??x ⇒ ((x₁ = ??x₁ ⇒ β = ??y₁ ∧ x₁ ≠ ??x₁ ⇒ ??y₀ = β))
;;                    ∧ x ≠ ??x ⇒ ((x₁ = ??x₁ ⇒ β = ??y₁ ∧ x₁ ≠ ??x₁ ⇒ y = β)))
;;
;; ==
;;
;; x = α ∧ x₁ = α₁
;; ∧ x = ??x ∧ x₁ = ??x₁ ⇒ β = ??y₁
;; ∧ x = ??x ∧ x₁ ≠ ??x₁ ⇒ β = ??y₀
;; ∧ x ≠ ??x ∧ x₁ = ??x₁ ⇒ β = ??y₁
;; ∧ x ≠ ??x ∧ x₁ ≠ ??x₁ ⇒ β = y
;;
;; == 
;; 
;; x = α ∧ x₁ = α₁
;; ∨ x = ??x ∧ x₁ = ??x₁ ∧ β = ??y₁
;; ∨ x = ??x ∧ x₁ ≠ ??x₁ ∧ β = ??y₀
;; ∨ x ≠ ??x ∧ x₁ = ??x₁ ∧ β = ??y₁
;; ∨ x ≠ ??x ∧ x₁ ≠ ??x₁ ∧ β = y

;;  So the final verification condition is
;;
;; x = α ∧ x₁ = α₁ ∧ ((x = ?x ∧ x₁ = ?x₁ ⇒ β = ?y) ∧ (x ≠ ?x ∧ x₁ ≠ ?x₁ ⇒ y = β₁))
;; ⇒
;; x = α ∧ x₁ = α₁
;; ∨ x = ??x ∧ x₁ = ??x₁ ∧ β = ??y₁
;; ∨ x = ??x ∧ x₁ ≠ ??x₁ ∧ β = ??y₀
;; ∨ x ≠ ??x ∧ x₁ = ??x₁ ∧ β = ??y₁
;; ∨ x ≠ ??x ∧ x₁ ≠ ??x₁ ∧ β = y
;; which is equivalent to

;; (x = ?x ∧ x₁ = ?x₁ ∧ β = ?y)
;; ∨ (¬(x = ?x ∧ x₁ = ?x₁) ∧ y = β)
;; ⇒
;; ∨ x = ??x ∧ x₁ = ??x₁ ∧ β = ??y₁
;; ∨ x = ??x ∧ x₁ ≠ ??x₁ ∧ β = ??y₀
;; ∨ x ≠ ??x ∧ x₁ = ??x₁ ∧ β = ??y₁
;; ∨ x ≠ ??x ∧ x₁ ≠ ??x₁ ∧ β = y

(declare-fun necessary (Addr Addr Addr Addr) Bool)

(declare-fun ??x (Addr Addr Addr) Addrs)
(declare-fun ??x1 (Addr Addr Addr) Addrs)
(declare-fun ??y0 (Addrs Addrs) Addr)
(declare-fun ??y1 (Addrs Addrs) Addr)
 

(define-fun mem ((x Addr) (xs Addrs)) Bool
  (select xs x))

(assert (exists ((x Addr) (x1 Addr) (y Addr) (beta Addr)
		 (?x Addr) (?x1 Addr) (?y Addr))
		(and (necessary x x1 ?x ?x1)
		     (or (and (= x ?x) (= x1 ?x1)
			      (= beta ?y))
			 (and (not (and (= x ?x) (= x1 ?x1)))
			      (= beta y)))
		     )
		)
	)

(assert (forall ((x Addr) (x1 Addr) (y Addr) (beta Addr)
		 (?x Addr) (?x1 Addr) (?y Addr))
		(=>
		 (and
		  (necessary x x1 ?x ?x1)
		  (or (and (= x ?x) (= x1 ?x1)
			   (= beta ?y))
		      (and (not (and (= x ?x) (= x1 ?x1)))
			     (= beta y))))
		    (or (and (mem x (??x ?x ?x1 ?y))
			     (mem x1 (??x1 ?x ?x1 ?y))
			     (= beta (??y1 (??x ?x ?x1 ?y) (??x1 ?x ?x1 ?y))))
			(and (not (mem x (??x ?x ?x1 ?y)))
			     (mem x1 (??x1 ?x ?x1 ?y))
			     (= beta (??y1 (??x ?x ?x1 ?y) (??x1 ?x ?x1 ?y))))
			(and (mem x (??x ?x ?x1 ?y))
			     (not (mem x1 (??x1 ?x ?x1 ?y)))
			     (= beta (??y0 (??x ?x ?x1 ?y) (??x1 ?x ?x1 ?y))))
			(and (not (mem x (??x ?x ?x1 ?y)))
			     (not (mem x1 (??x1 ?x ?x1 ?y)))
			     (= beta y))))))

;; (check-sat-using (then simplify qe bit-blast qfaufbv))
(check-sat)
(get-model)

(assert (forall ((x Addr) (x1 Addr) (y Addr) (beta Addr)
		 (?x Addr) (?x1 Addr) (?y Addr))
		(= (necessary x x1 ?x ?x1)
		   (=> (= x ?x) (= x1 ?x1)))))
(check-sat)

;; (assert (forall ((x Addr) (x1 Addr) (y Addr))
;; 		(and (= x (??x x x1 y))
;; 		     (= x1 (??x1 x x1 y))
;; 		     (= y (??y0 x x1 y))
;; 		     (= y (??y1 x x1 y)))))
;; (check-sat)
		


