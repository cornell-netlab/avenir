(set-logic UFBV)
(define-sort OpCode () (_ BitVec 1))
(define-sort Addr () (_ BitVec 8))
(define-sort Index () (_ BitVec 1))


;; src -> {op := 0} | {op := 1}*
;; synth to
;; src -> {op := 0} | {op := 1}*

;; wp(op := 0, src = src' ∧ op = op')
;; = src = src' ∧ op' = 0

;; wp(op := 1, src = src' ∧ op = op')
;; = \src = src' ∧ op = 1

;; impl(0)
;; src = srcKey ∧ src = src' ∧ op = 0
;; ⇔ src = f10(srcKey) ∧ (f20(srcKey) = 0 ⇒ src = src' ∧ op' = 0
;;                       ∨ f20(srcKey) = 1 ⇒ src = src' ∧ op' = 1)

(declare-fun f10 ((Addr)) Addr)
(declare-fun f20 ((Addr)) Index) ;; actionIdentifier
(declare-fun f11 ((Addr)) Addr)
(declare-fun f21 ((Addr)) Index) ;; actionIdentifier


(assert (forall ((srcIn Addr) (srcOut Addr) (opOut OpCode)
		 (srcKey Addr))
		(= (and (= srcIn srcKey)
			(= srcIn srcOut)
			(= opOut #b0))
		   (and (= srcIn (f10 srcKey))
			(or (and (= #b0 (f20 srcKey))
				 (= srcIn srcOut)
				 (= #b0 opOut ))
			    (and (= #b1 (f20 srcKey))
				 (= srcIn srcOut)
				 (= #b1 opOut)))))))

;; impl(1,)
;; src = srcKey ∧ src = src' ∧ op = 1
;; ⇔ src = f11(srcKey) ∧ (f21(srcKey) = 0 ∧ src = src' ∧ op' = 0
;;                       ∨ f21(srcKey) = 1 ∧ src = src' ∧ op' = 1)
(assert (forall ((srcIn Addr) (srcOut Addr)
		 (opOut OpCode) (srcKey Addr))
		(= (and (= srcIn srcKey)
			(= srcIn srcOut)
			(= opOut #b1))
		   (and (= srcIn (f11 srcKey))
			(or (and (= #b0 (f21 srcKey))
				 (= srcIn srcOut)
				 (= #b0 opOut ))
			    (and (= #b1 (f21 srcKey))
				 (= srcIn srcOut)
				 (= #b1 opOut)))))))



;; (check-sat-using (then (using-params qe :qe-nonlinear true ) bit-blast smt))
(check-sat)
(get-model)
