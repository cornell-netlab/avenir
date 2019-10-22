;; (set-logic AUFBV)

(define-sort OpCode () (_ BitVec 1))
(define-sort Addr () (_ BitVec 2))
(define-sort AddrPair () (_ BitVec 4))
(define-sort AddrAddrOp () (_ BitVec 5))
(define-sort AddrOp () (_ BitVec 3))
(define-sort Index () (_ BitVec 2))
(define-sort Loc () (_ BitVec 2))

(define-sort Addrs () (Array Addr Bool))
(define-sort AddrSet () (Array Addr Bool))
(define-sort AddrPairSet () (Array AddrPair Bool))
(define-sort AddrPairs () (Array AddrPair Bool))
(define-sort AddrAddrOpSet () (Array AddrAddrOp Bool))
(define-sort AddrOpSet () (Array AddrOp Bool))
(define-sort OpCodes () (Array OpCode Bool))
(define-sort OpFuncAddr () (Array Addr OpCode))
(define-sort OpFuncAddrPair () (Array AddrPair OpCode))

;;
;;  x = ?x && x₁ = ?x₁ -> y := ??y₀ ▷ true -> skip
;;
;;  ==>>
;;
;;  (x = ??x -> y := ??y₀ ▷ true -> skip);
;;  (x₁ = ??x₁ -> y := ??y₁ ▷ true -> skip)


;;
;; x = α ∧ x₁ = α₁ ∧ ((x = ?x ∧ x₁ = ?x₁ ⇒ β = ?y) ∧ (x ≠ ?x ∧ x₁ ≠ ?x₁ ⇒ y = β))
;; ⇒
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
;; x = ??x ∧ x₁ = ??x₁ ∧ β = ??y₁
;; ∨ x = ??x ∧ x₁ ≠ ??x₁ ∧ β = ??y₀
;; ∨ x ≠ ??x ∧ x₁ = ??x₁ ∧ β = ??y₁
;; ∨ x ≠ ??x ∧ x₁ ≠ ??x₁ ∧ β = y


(define-fun mem ((x Addr) (xs Addrs)) Bool
  (select xs x))
(push)
(declare-fun ??x (Addr Addr Addr) Addrs)
(declare-fun ??x1 (Addr Addr Addr) Addrs)
(declare-fun ??y0 (Addrs Addrs) Addr)
(declare-fun ??y1 (Addrs Addrs) Addr)
 

(declare-fun necessary (Addr Addr Addr Addr) Bool)

;; (assert (exists ((x Addr) (x1 Addr) (y Addr) (beta Addr)
;; 		 (?x Addr) (?x1 Addr) (?y Addr))
;; 		(and (necessary x x1 ?x ?x1)
;; 		     (or (and (= x ?x) (= x1 ?x1)
;; 			      (= beta ?y))
;; 			 (and (not (and (= x ?x) (= x1 ?x1)))
;; 			      (= beta y)))
;; 		     )
;; 		)
;; 	)

(assert (forall ((x Addr) (x1 Addr) (y Addr) (beta Addr)
		 (?x Addr) (?x1 Addr) (?y Addr))
		(=>
		 (and
		  ;;(necessary x x1 ?x ?x1)
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

(check-sat-using (then simplify qe bit-blast qfaufbv))
;; (check-sat)
(get-model)
(pop)

;; (assert (forall ((x Addr) (x1 Addr) (y Addr) (beta Addr)
;; 		 (?x Addr) (?x1 Addr) (?y Addr))
;; 		(= (necessary x x1 ?x ?x1)
;; 		   (=> (= x ?x) (= x1 ?x1)))))
;; (check-sat)

;; (assert (forall ((x Addr) (x1 Addr) (y Addr))
;; 		(and (= x (??x x x1 y))
;; 		     (= x1 (??x1 x x1 y))
;; 		     (= y (??y0 x x1 y))
;; 		     (= y (??y1 x x1 y)))))
;; (check-sat)



(push)

;; (declare-fun ??x (Addr Addr Addr Addr) Addrs)
(define-fun ??x ((?x Addr) (?x1 Addr) (?y0 Addr) (?y1 Addr))
  Addrs (store ((as const (Array Addr Bool)) false) ?x true))

(define-fun ??x1 ((?x Addr) (?x1 Addr) (?y0 Addr) (?y1 Addr))
  Addrs (store ((as const (Array Addr Bool)) false) ?x1 true))

(declare-fun ??y0 (Addr Addr Addr Addr Addrs Addrs) Addr)

;; Now we flip the direction and still fail!
;; 
;; x = ?x ∧ x₁ = ?x₁ ∧ β = ?y₁
;; ∨ x = ?x ∧ x₁ ≠ ?x₁ ∧ β = ?y₀
;; ∨ x ≠ ?x ∧ x₁ = ?x₁ ∧ β = ?y₁
;; ∨ x ≠ ?x ∧ x₁ ≠ ?x₁ ∧ β = y
;; ⇒
;; (x = ??x ∧ x₁ = ??x₁ ∧ β = ??y₀)
;;  ∨ (¬(x = ??x ∧ x₁ = ??x₁) ∧ y = β)
(assert (forall ((x Addr) (x1 Addr) (y Addr) (beta Addr)
		 (?x Addr) (?x1 Addr) (?y0 Addr) (?y1 Addr))
		(=
		 (or (and (= x ?x) (= x1 ?x1) (= beta ?y1))
		     (and (= x ?x) (not (= x1 ?x1)) (= beta ?y0))
		     (and (not (= x ?x)) (= x1 ?x1) (= beta ?y1))
		     (and (not (= x ?x)) (not (= x1 ?x1)) (= beta y)))
		 ;; <=>
		 (or (and (mem x (??x ?x ?x1 ?y0 ?y1))
			  (mem x1 (??x1 ?x ?x1 ?y0 ?y1))
			  (= beta (??y0 ?x ?x1 ?y0 ?y1
					(??x ?x ?x1 ?y0 ?y1)
					(??x1 ?x ?x1 ?y0 ?y1))))
		     (and (not (and (mem x (??x ?x ?x1 ?y0 ?y1))
				    (mem x1 (??x1 ?x ?x1 ?y0 ?y1))))
			  (= y beta))))))	      
(check-sat)		  


(pop)



(push)

;; src = ?src -> op = ?ops ; dst = ?dst -> op = ?op
;; src = src && dst = dst && op = ?op
;;
;; 
;; src = ??src && dst = ??dst && op = ??op


(assert (forall ((src Addr) (?src Addr) (dst Addr) (?dst Addr)
		 (op OpCode) (?op OpCode))
		(exists ((??src Addr) (??dst Addr) (??op OpCode))
			(= (and (and (= ?src src)
				     (= ?dst dst))
				(= op ?op))
			   (and (and (= ??src src)
				     (= ??dst dst))
				(= ??op op))))))

(check-sat)
(get-model)
(pop)


(push)

(assert (forall ((src Addr) (?src Addr) (dst Addr) (?dst Addr)
		 (op OpCode) (?op OpCode))
		(exists ((??src Addrs) (??dst Addrs) (??op OpCodes))
			(= (and (and (= ?src src)
				     (= ?dst dst))
				(= op ?op))
			   (and (and (select ??src src)
				     (select ??dst dst))
				(select ??op op))))))
			


(check-sat)
(pop)

(push)

(declare-fun ??src (Addr Addr OpCode) Addrs)
(declare-fun ??dst (Addr Addr OpCode) Addrs)
(declare-fun ??op (Addr Addr OpCode) OpCode)

(assert (exists ((s Addr) (d Addr) (o OpCode) (x Addr)) (select (??src s d o) x)))
(assert (exists ((s Addr) (d Addr) (o OpCode) (x Addr)) (select (??dst s d o) x)))

(assert (forall ((src Addr) (?src Addr) (dst Addr) (?dst Addr)
		 (op OpCode) (?op OpCode))
		(= (and (and (= ?src src)
			     (= ?dst dst))
			(= op ?op))
		   (and (and (select (??src ?src ?dst ?op) src)
			     (select (??dst ?src ?dst ?op) dst))
			(= (??op ?src ?dst ?op) op)))))
			


(check-sat)
(get-model)
(pop)


(push)

(declare-fun ??src (Addrs Addrs OpCodes) Addrs)
(declare-fun ??dst (Addrs Addrs OpCodes) Addrs)
(declare-fun ??op (Addrs Addrs OpCodes) OpCode)

(assert (exists ((s Addrs) (d Addrs) (o OpCodes) (x Addr)) (select (??src s d o) x)))
(assert (exists ((s Addrs) (d Addrs) (o OpCode) (x Addr)) (select (??dst s d o) x)))

(assert (forall ((src Addr) (?src Addrs) (dst Addr) (?dst Addrs)
		 (op OpCode) (?op OpCodes))
		(= (and (and (select ?src src)
			     (select ?dst dst))
			(select ?op op))
		   (and (and (select (??src ?src ?dst ?op) src)
			     (select (??dst ?src ?dst ?op) dst))
			(= (??op ?src ?dst ?op) op)))))
			

(check-sat)
(pop)


(push)

;; (declare-fun ??src (Addr Addr OpCode) Addrs)
;; (declare-fun ??dst (Addr Addr OpCode) Addrs)
;; (declare-fun ??op (Addr Addr OpCode) OpCode)

;; (assert (exists ((s Addr) (d Addr) (o OpCode) (x Addr)) (select (??src s d o) x)))
;; (assert (exists ((s Addr) (d Addr) (o OpCode) (x Addr)) (select (??dst s d o) x)))

;; (assert (forall ((src Addr) (?src Addr) (dst Addr) (?dst Addr)
;; 		 (op OpCode) (?op OpCode))
;; 		(=
;; 		 (or (and (= src #b11) (= dst #b11) (= op #b1))
;; 		 (and (and (= ?src src)
;; 			   (= ?dst dst))
;; 			(= op ?op))
;; 		   (and (and (select (??src ?src ?dst ?op) src)
;; 			     (select (??dst ?src ?dst ?op) dst))
;; 			(= (??op ?src ?dst ?op) op)))))
			


;; (check-sat)
;; (get-model)

(pop)





;;; The predicate that we want to implement is
;;;
;;; Given R, L, 
;;; synthesize f : TableRow → TableRow⁺ such that
;;;
;;; ∀ pkt. ∀ pkt'. ∀ τₗ. ∀ τᵣ. ∀ ρ : Table × Match × ActionData.
;;;
;;;  (wp(L τᵣ) ⇔ wp (R τₗ))
;;;  ⇒  
;;;  (wp(L (τᵣ ⊕ ρ)) ⇔ wp(R (τₗ ⊕ f(ρ))))

;;;
;;; we can synthesize in two parts, once for additions and once for deletions
;;; modifications will just be delete + add
;;;


;;; For additions, we can model it more or less as we have been.
;;;
;;; Our running example becomes as follows for the single-table logical program
;;;   src::dst ∈ SD -> op := op(src, dst)
;;;   [] ?dst ∉ D ∧ ?src ∉ S ∧ dst = ?dst ∧ src = ?src -> op := ?op (?src, ?dst)
;;;
;;; if the single-table program is concrete, then we have to allow for deletions and insertions
;;;
;;;   assert (¿SD ∩ SD = ∅);
;;;   assert (¿SD ∩ ¿Del = ∅);
;;;   (src::dst ∈ SD -> assert (src::dst ∉ ¿Del) ∧ op(src, dst)
;;;   [] src::dst ∈ ¿SD           ;;; and match the current packet
;;;      -> op := ¿op(src::dst))
;;;
;;; considering the two-table program as a logical, we generate two problems, one for additions in the `dst` table and one for additions in the `src` table
;;;
;;; [DST TABLE ADDITIONS]
;;; (src ∈ S → op := op₂(src));
;;; (dst ∈ D → op := op₁(dst) [] dst = ?dst ∧ ?dst ∉ D -> op := ?op₁(?dst))
;;;
;;; [SRC TABLE ADDITIONS]
;;; (src ∈ S → op := op₂(src) [] ?src ∉ S ∧ src = ?src → op := ?op₂(?src));
;;; (dst ∈ D → op := op₁(dst));


;;; This corresponds two z3 queries
(push)
(echo "-----------------START--------------------------")
(echo "two-table dst addition -> 1 table add/delete")

(define-fun consistent
  ((src Addr) (dst Addr) (op OpCode)
   (S Addrs)  (D Addrs)  (opFSD OpFuncAddrPair)
   (SD AddrPairs)        (opFD OpFuncAddr))
  Bool
  (= (and (select SD (concat src dst))
	  (= op (select opFSD (concat src dst))))
     (and (select S src) (select D dst) (= op (select opFD dst)))))


(define-fun implements
  ((src Addr) (dst Addr)   (op OpCode)
   (S Addrs)  (D Addrs)    (opFSD OpFuncAddrPair)
       (SD AddrPairs)      (opFD OpFuncAddr)
       (newDst Addr)  (newOpFD OpFuncAddr)
   )
  Bool
  (exists ((??SD AddrPairs) (??Del AddrPairs) (??opFSD OpFuncAddrPair))
	  (and ;; (forall ((x AddrPair)) (=> (select ??SD x) (not (select SD x))))
	       ;; (forall ((x AddrPair)) (=> (select ??SD x) (not (select ??Del x))))

	       (=> (consistent src dst op S D opFSD SD opFD)
		   ;; two tables, addition to dst Table row (x = newDst -> op := newOpFD)
		   (= (and (select S src)
			   (or (and (select D dst)
				    (= op (select opFD dst)))
			       (and (not (select D newDst))
				    (= dst newDst)
				    (= op (select newOpFD newDst)))))
		      ;; one table, add & delete
		      (and 
		       (or (and (select SD (concat src dst))
				(not (select ??Del (concat src dst)))
				(= op (select opFSD (concat src dst))))
			   (and (select ??SD (concat src dst))
				(= op (select ??opFSD (concat src dst)))))))))))
	  

(assert (forall ((src Addr) (dst Addr) (op OpCode)
		 (S Addrs)  (D Addrs)  (opFSD OpFuncAddrPair)
		 (SD AddrPairs)        (opFD OpFuncAddr)
		 (newDst Addr) (newOpFD OpFuncAddr)
		 )
		(implements src dst op S D opFSD SD opFD newDst newOpFD)))
(check-sat)

(echo "------------------END---------------------------")
(pop)



;;; The previous encoding makes z3 unhappy (.. because we are leaving the world of MSO?)
;;;
;;; We take the strange approach of unifying the match condition with the action data.
;;; 
;;; Our running example becomes as follows for the single-table logical program
;;;   where SDO ⊆ Addr × Addr × OpCode
;;;   src::dst::op' ∈ SDO₀ ∧ ?act = 0 -> op := op'
;;;   [] ?dst::?src::?op ∉ SD0₀ ∧ ?dst = dst ∧ ?src = src -> op := ?op
;;;
;;; if the single-table program is concrete, then we have to allow for deletions and insertions
;;;   assert (∀ s d o o'. (s::d::o ∈ (?)SDO₀ ∧ s::d::o' ∈ (?)SDO₀) ⇒ o = o');
;;;   assert (∀ s d o. (s::d::o) ∉ ?SDO₀ ∨ (s::d::o) ∉ SDO₀);
;;;   assert (∀ s d o.¬(s::d::o ∈ ?SDO₀ ∧ s::d ∈ ?Del));
;;;   assume (∀ s d. s::d ∈ ?Del ⇒ ∃ o. s::d::o ∈ SDO₀);
;;;   (src::dst::op' ∈ SDO₀ ∧ assert (src::dst ∉ ?Del) → op := op'
;;;   [] src::dst::op' ∈ ?SDO₀ → op := op')
;;;
;;; considering the two-table program as a logical, we generate two problems, one for additions in the `dst` table and one for additions in the `src` table
;;;
;;; [DST TABLE ADD ONE RULE]
;;; (src::op₁ ∈ SO₀ → op := op₁);
;;; ((dst::op₂ ∈ DO₀ → op := op₂) [] (dst = ?dst ∧ ?dst::?op ∉ DO₀ → op := ?op))
;;;
;;; [SRC TABLE ADD ONE RULE]
;;; ((src::op₁ ∈ SO₀ → op := op₁) [] (?src::?op ∉ SO₀ ∧ src = ?src → op := ?op));
;;; (dst::op₂ ∈ DOₒ → op := op₂);



(push)
(echo "-----------------START--------------------------")
(echo "An Attempt with a new table encoding")
(echo "two-table dst addition -> 1 table add/delete")

(define-fun consistent
  ((src Addr) (dst Addr) (op OpCode)
   (opSrc OpCode) (opDst OpCode) (opSD OpCode)
   (SO AddrOpSet)  (DO AddrOpSet)  
   (SDO AddrAddrOpSet))
  Bool
  (= (and (select SDO (concat src (concat dst opSD)))
	  (= op opSD))
     (and (select SO (concat src opSrc))
	  (select DO (concat dst opDst)) (= op opDst))))


(define-fun implements
  ((src Addr) (dst Addr) (op OpCode)
   (?dst Addr) (?op OpCode)
   (opSrc OpCode) (opDst OpCode) (opSD OpCode)
   (SO AddrOpSet)  (DO AddrOpSet)  
   (SDO AddrAddrOpSet))
  Bool
  (exists ((??SDO AddrAddrOpSet) (??Del AddrPairSet))
	  (and
	       ;; (SDO₀(s,d,o1) ∧ SDOₒ(s,d,o2)) ⇒ o1 = o2 (necessary)
	       (forall ((s Addr) (d Addr) (o1 OpCode) (o2 OpCode))
		       (=> (and (select SDO (concat s (concat d o1)))
				(select SDO (concat s (concat d o2))))
			   (= o1 o2)))
	       ;; (??SDO₀(s,d,o1) ∧ ??SDO₀(s,d,o2)) ⇒ o1 = o2 (necessary)
	       (forall ((s Addr) (d Addr) (o1 OpCode) (o2 OpCode))
		       (=> (and (select ??SDO (concat s (concat d o1)))
				(select ??SDO (concat s (concat d o2))))
			   (= o1 o2)))
	       ;; ??SDO ∩ ??Del = ∅ (optional)
	       (forall ((s Addr) (d Addr) (o OpCode))
	       	       (not (and (select ??SDO (concat s (concat d o)))
	       			 (select ??Del (concat s d)))))
	       ;; ??Del ⊆ SDO₀  (optional)
	       (forall ((s Addr) (d Addr))
	       	       (=> (select ??Del (concat s d))
	       		   (exists ((o OpCode)) (select SDO (concat s (concat d o))))))
	       (=> (consistent src dst op opSrc opDst opSD SO DO SDO)
		   (=
		    ;; ADD RULE (dst = ?dst -> op := ?op)
		    (and (select SO (concat src opSrc))
			 (or (and (select DO (concat dst opDst))
				  (= op opDst))
			     (and (= ?dst dst)
				  (select DO (concat ?dst ?op))
				  (= op ?op))))
		    (or (and (select SDO (concat src (concat dst opSD)))
			     (not (select ??Del (concat src dst)))
			     (= op opSD))
			(and (select ??SDO (concat src (concat dst opSD)))
			     (= op opSD))))))))
	  
(assert (forall ((src Addr) (dst Addr) (op OpCode)
		 (?dst Addr) (?op OpCode)
		 (opSrc OpCode) (opDst OpCode) (opSD OpCode)
		 (SO AddrOpSet)  (DO AddrOpSet)  
		 (SDO AddrAddrOpSet))
		(implements src dst op ?dst ?op opSrc opDst opSD SO DO SDO)))

(check-sat)

(echo "------------------END---------------------------")
(pop)






;;; Deletions
;;;
;;; The instrumentation for the concrete table remains as in the
;;; addition case, since deleting a logical rule might require rule
;;; modifications in the concrete table
;;;
;;; For a DSL encoding of our single-table program, we have
;;; dst ∈ D ∧ src ∈ S
;;;  ∧ dst ∈ D ∧ src ∈ S
;;;  ∧ dst ≠ ?dst ∧ src ≠ ?src
;;;  -> op := op(src, dst)
;;;
;;;
