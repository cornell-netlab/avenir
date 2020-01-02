(define-sort Addr () (_ BitVec 2))
(define-sort OpCode () (_ BitVec 1))
(define-sort ActId () (_ BitVec 1))

(define-sort FinSet (T) (Array T Bool))


(define-fun skip ((x Addr) (x_out Addr) (op OpCode) (op_out OpCode))
  Bool (and (= x x_out)
	    (= op op_out)))

(define-fun assgn ((x Addr) (x_out Addr) (op OpCode) (op_out OpCode))
  Bool (and (= x x_out)
	    (= #b1 op_out)))

(push)
(echo "Checking with sets size 1 (expect unsat, i.e. valid)")
(define-fun f_x ((x Addr) (a ActId)) Addr x)
(define-fun f_a ((x Addr) (a ActId)) ActId a)


(declare-const Mt_skip Addr)
(declare-const Mt_assgn Addr)
(declare-const Ms_skip Addr)
(declare-const Ms_assgn Addr)

(assert (distinct Mt_skip Mt_assgn))
(assert (distinct Ms_skip Ms_assgn))	

(assert	(forall ((x Addr) (x_out Addr) (op OpCode) (op_out OpCode))
	 (= (and
	    (=> (= Mt_assgn x) (assgn x x_out op op_out))
	    (=> (= Mt_skip x) (skip x x_out op op_out))
	    (=> (not (= Mt_assgn x)) (not (= Mt_skip x))
		(skip x x_out op op_out)))		    
	   (and
	    (=> (= Ms_assgn x) (assgn x x_out op op_out))
	    (=> (= Ms_skip x) (skip x x_out op op_out))
	    (=> (not (= Mt_assgn x)) (not (= Mt_skip x))
		(skip x x_out op op_out)))
	   )))

(declare-const x Addr)
(declare-const x_out Addr)
(declare-const op OpCode)
(declare-const op_out OpCode)
(declare-const x_NEW Addr)
(declare-const a_NEW ActId)
	 
(assert	(not (= (and
		 (=> (= Mt_assgn x) (not (= x x_NEW))
		     (assgn x x_out op op_out))
		 (=> (= Mt_skip x) (not (= x x_NEW))
		     (skip x x_out op op_out))
		 (=> (= x x_NEW) (= a_NEW #b1)
		     (assgn x x_out op op_out))
		 (=> (= x x_NEW) (= a_NEW #b0)
		     (skip  x x_out op op_out))
		 (=> (not (= x x_NEW)) (not (= Mt_assgn x)) (not (= Mt_skip x))
		     (skip x x_out op op_out)))
		(and
		 (=> (= Ms_assgn x) (not (= x (f_x x_NEW a_NEW)))
		     (assgn x x_out op op_out))
		 (=> (= Ms_skip  x) (not (= x (f_x x_NEW a_NEW)))
		     (skip  x x_out op op_out))
		 (=> (= x (f_x x_NEW a_NEW)) (= #b1 (f_a x_NEW a_NEW))
		     (assgn x x_out op op_out))
		 (=> (= x (f_x x_NEW a_NEW)) (= #b0 (f_a x_NEW a_NEW))
		     (skip  x x_out op op_out))
		 (=> (not (= Ms_assgn x)) (not (= Ms_skip  x)) (not (= x (f_x x_NEW a_NEW)))
		     (skip x x_out op op_out)))
		)))
			    
(check-sat)
(pop)


(push)
(echo "Checking with sets size N (expect unsat, i.e. valid)")

(define-fun f_x ((x Addr) (a ActId)) Addr x)
(define-fun f_a ((x Addr) (a ActId)) ActId a)

(declare-const Mt_skip (FinSet Addr))
(declare-const Mt_assgn (FinSet Addr))
(declare-const Ms_skip (FinSet Addr))
(declare-const Ms_assgn (FinSet Addr))

(assert (forall ((x Addr)) (not (and (select Mt_skip x) (select Mt_assgn x)))))
(assert (forall ((x Addr)) (not (and (select Ms_skip x) (select Ms_assgn x)))))

(assert	(forall ((x Addr) (x_out Addr) (op OpCode) (op_out OpCode))
	 (= (and
	    (=> (select Mt_assgn x) (assgn x x_out op op_out))
	    (=> (select Mt_skip x) (skip x x_out op op_out))
	    (=> (not (select Mt_assgn x)) (not (select Mt_skip x))
		(skip x x_out op op_out)))		    
	   (and
	    (=> (select Ms_assgn x) (assgn x x_out op op_out))
	    (=> (select Ms_skip x) (skip x x_out op op_out))
	    (=> (not (select Mt_assgn x)) (not (select Mt_skip x))
		(skip x x_out op op_out)))
	   )))

(declare-const x Addr)
(declare-const x_out Addr)
(declare-const op OpCode)
(declare-const op_out OpCode)
(declare-const x_NEW Addr)
(declare-const a_NEW ActId)
	 
(assert	(not (= (and
		 (=> (select Mt_assgn x) (not (= x x_NEW))
		     (assgn x x_out op op_out))
		 (=> (select Mt_skip x) (not (= x x_NEW))
		     (skip x x_out op op_out))
		 (=> (= x x_NEW) (= a_NEW #b1)
		     (assgn x x_out op op_out))
		 (=> (= x x_NEW) (= a_NEW #b0)
		     (skip  x x_out op op_out))
		 (=> (not (= x x_NEW)) (not (select Mt_assgn x)) (not (select Mt_skip x))
		     (skip x x_out op op_out)))
		(and
		 (=> (select Ms_assgn x) (not (= x (f_x x_NEW a_NEW)))
		     (assgn x x_out op op_out))
		 (=> (select Ms_skip  x) (not (= x (f_x x_NEW a_NEW)))
		     (skip x x_out op op_out))
		 (=> (= x (f_x x_NEW a_NEW)) (= #b1 a_NEW)
		     (assgn x x_out op op_out))
		 (=> (= x (f_x x_NEW a_NEW)) (= #b0 a_NEW)
		     (skip  x x_out op op_out))
		 (=> (not (select Ms_assgn x)) (not (select Ms_skip x)) (not (= x (f_x x_NEW a_NEW)))
		     (skip x x_out op op_out)))
		)))
			    
(check-sat)
(pop)



(push)
;;;; SETS ARE EMPTY DOES IT WORK?
(define-fun f_x ((x Addr) (a ActId)) Addr x)
(define-fun f_a ((x Addr) (a ActId)) ActId a)

(declare-const x Addr)
(declare-const x_out Addr)
(declare-const op OpCode)
(declare-const op_out OpCode)
(declare-const x_NEW Addr)
(declare-const a_NEW ActId)
(echo "checking with empty sets (expect unsat, i.e. valid)")		
(assert	(not (=> (= (skip x x_out op op_out)
		    (skip x x_out op op_out))
		 
		 (= (and
		     (=> (and (= x x_NEW) (= a_NEW #b1)) (assgn x x_out op op_out))
		     (=> (and (= x x_NEW) (= a_NEW #b0)) (skip x x_out op op_out))
		     (=> (and (not (= x x_NEW))) (skip x x_out op op_out)))
		    (and
		     (=> (and (= x (f_x x_NEW a_NEW)) (= #b1 (f_a x_NEW a_NEW))) (assgn x x_out op op_out))
		     (=> (and (= x (f_x x_NEW a_NEW)) (= #b0 (f_a x_NEW a_NEW))) (skip x x_out op op_out))
		     (=> (not (= x (f_x x_NEW a_NEW))) (skip x x_out op op_out)))
		     ))))
			    
(check-sat)
(pop)



(push)

(echo "trying to synthesize the function for |S| = 1")
(declare-fun f_x (Addr ActId) Addr)
(declare-fun f_a (Addr ActId) ActId)

(assert (forall ((x Addr) (x_out Addr) (op OpCode) (op_out OpCode)
		 (Mt_skip Addr) (Mt_assgn Addr)
		 (Ms_skip Addr) (Ms_assgn Addr)
		 (x_NEW Addr) (a_NEW ActId))
		
		(=>
		 (distinct Mt_skip Mt_assgn)
		 (distinct Ms_skip Ms_assgn)
		 (forall
		  ((x Addr) (x_out Addr) (op OpCode) (op_out OpCode))
		  (= (and
		      (=> (= Mt_assgn x) (assgn x x_out op op_out))
		      (=> (= Mt_skip x) (skip x x_out op op_out))
		      (=> (not (= Mt_assgn x)) (not (= Mt_skip x))
			  (skip x x_out op op_out)))
		     
		     (and
		      (=> (= Ms_assgn x) (assgn x x_out op op_out))
		      (=> (= Ms_skip x) (skip x x_out op op_out))
		      (=> (not (= Mt_assgn x)) (not (= Mt_skip x))
			  (skip x x_out op op_out)))))
		 
		    (= (and
			(=> (= Mt_assgn x) (not (= x x_NEW))
			    (assgn x x_out op op_out))
			(=> (= Mt_skip x) (not (= x x_NEW))
			    (skip x x_out op op_out))
			(=> (= x x_NEW) (= a_NEW #b1)
			    (assgn x x_out op op_out))
			(=> (= x x_NEW) (= a_NEW #b0)
			    (skip x x_out op op_out))
			(=> (not (= x x_NEW)) (not (= Mt_assgn x)) (not (= Mt_skip x))
			    (skip x x_out op op_out)))
		       
		       (and
			(=> (= Ms_assgn x) (not (= x (f_x x_NEW a_NEW)))
			    (assgn x x_out op op_out))
			(=> (= Ms_skip x) (not (= x (f_x x_NEW a_NEW)))
			    (skip x x_out op op_out))
			(=> (= x (f_x x_NEW a_NEW)) (= #b1 (f_a x_NEW a_NEW))
			    (assgn x x_out op op_out))
			(=> (= x (f_x x_NEW a_NEW)) (= #b0 (f_a x_NEW a_NEW))
			    (skip x x_out op op_out))
			(=> (not (= Ms_assgn x)) (not (= Ms_skip x))  (not (= x (f_x x_NEW a_NEW)))
			    (skip x x_out op op_out))
			)))))
(check-sat)
(get-model)

(echo "Checking that the synthesized f_x and f_a are solutions for |S| = N (unsat = Valid)")
(declare-const Mt_skip (FinSet Addr))
(declare-const Mt_assgn (FinSet Addr))
(declare-const Ms_skip (FinSet Addr))
(declare-const Ms_assgn (FinSet Addr))

(assert (forall ((x Addr)) (not (and (select Mt_skip x) (select Mt_assgn x)))))
(assert (forall ((x Addr)) (not (and (select Ms_skip x) (select Ms_assgn x)))))

(assert	(forall ((x Addr) (x_out Addr) (op OpCode) (op_out OpCode))
	 (= (and
	    (=> (select Mt_assgn x) (assgn x x_out op op_out))
	    (=> (select Mt_skip x) (skip x x_out op op_out))
	    (=> (not (select Mt_assgn x)) (not (select Mt_skip x))
		(skip x x_out op op_out)))		    
	   (and
	    (=> (select Ms_assgn x) (assgn x x_out op op_out))
	    (=> (select Ms_skip x) (skip x x_out op op_out))
	    (=> (not (select Mt_assgn x)) (not (select Mt_skip x))
		(skip x x_out op op_out)))
	   )))

(declare-const x Addr)
(declare-const x_out Addr)
(declare-const op OpCode)
(declare-const op_out OpCode)
(declare-const x_NEW Addr)
(declare-const a_NEW ActId)
	 
(assert	(not (= (and
		 (=> (select Mt_assgn x) (not (= x x_NEW))
		     (assgn x x_out op op_out))
		 (=> (select Mt_skip x) (not (= x x_NEW))
		     (skip x x_out op op_out))
		 (=> (= x x_NEW) (= a_NEW #b1)
		     (assgn x x_out op op_out))
		 (=> (= x x_NEW) (= a_NEW #b0)
		     (skip  x x_out op op_out))
		 (=> (not (= x x_NEW)) (not (select Mt_assgn x)) (not (select Mt_skip x))
		     (skip x x_out op op_out)))
		(and
		 (=> (select Ms_assgn x) (not (= x (f_x x_NEW a_NEW)))
		     (assgn x x_out op op_out))
		 (=> (select Ms_skip  x) (not (= x (f_x x_NEW a_NEW)))
		     (skip x x_out op op_out))
		 (=> (= x (f_x x_NEW a_NEW)) (= #b1 a_NEW)
		     (assgn x x_out op op_out))
		 (=> (= x (f_x x_NEW a_NEW)) (= #b0 a_NEW)
		     (skip  x x_out op op_out))
		 (=> (not (select Ms_assgn x)) (not (select Ms_skip x)) (not (= x (f_x x_NEW a_NEW)))
		     (skip x x_out op op_out)))
		)))
			    
(check-sat)

(pop)

(push)

(echo "trying to synthesize the function for |S| = N")
(declare-fun f_x (Addr ActId) Addr)
(declare-fun f_a (Addr ActId) ActId)

(assert (forall ((x Addr) (x_out Addr) (op OpCode) (op_out OpCode)
		 (Mt_skip (FinSet Addr)) (Mt_assgn (FinSet Addr))
		 (Ms_skip (FinSet Addr)) (Ms_assgn (FinSet Addr))
		 (x_NEW Addr) (a_NEW ActId))
		
		(=>
		 (not (and (select Mt_skip x) (select Mt_assgn x)))
		 (forall
		  ((x Addr) (x_out Addr) (op OpCode) (op_out OpCode))
		  (= (and
		      (=> (select Mt_assgn x) (assgn x x_out op op_out))
		      (=> (select Mt_skip x) (skip x x_out op op_out))
		      (=> (not (select Mt_assgn x)) (not (select Mt_skip x))
			  (skip x x_out op op_out)))
		     
		     (and
		      (=> (select Ms_assgn x) (assgn x x_out op op_out))
		      (=> (select Ms_skip x) (skip x x_out op op_out))
		      (=> (not (select Mt_assgn x)) (not (select Mt_skip x))
			  (skip x x_out op op_out)))))
		 
		    (= (and
			(=> (select Mt_assgn x) (not (= x x_NEW))
			    (assgn x x_out op op_out))
			(=> (select Mt_skip x) (not (= x x_NEW))
			    (skip x x_out op op_out))
			(=> (= x x_NEW) (= a_NEW #b1)
			    (assgn x x_out op op_out))
			(=> (= x x_NEW) (= a_NEW #b0)
			    (skip x x_out op op_out))
			(=> (not (= x x_NEW)) (not (select Mt_assgn x)) (not (select Mt_skip x))
			    (skip x x_out op op_out)))
		       
		       (and
			(=> (select Ms_assgn x) (not (= x (f_x x_NEW a_NEW)))
			    (assgn x x_out op op_out))
			(=> (select Ms_skip x) (not (= x (f_x x_NEW a_NEW)))
			    (skip x x_out op op_out))
			(=> (= x (f_x x_NEW a_NEW)) (= #b1 (f_a x_NEW a_NEW))
			    (assgn x x_out op op_out))
			(=> (= x (f_x x_NEW a_NEW)) (= #b0 (f_a x_NEW a_NEW))
			    (skip x x_out op op_out))
			(=> (not (select Ms_assgn x)) (not (select Ms_skip x))  (not (= x (f_x x_NEW a_NEW)))
			    (skip x x_out op op_out))
			)))))
(check-sat)
(get-model)

(pop)
