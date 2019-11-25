(set-logic UFBV)
(define-sort OpCode () (_ BitVec 1))
(define-sort Addr () (_ BitVec 2))
(define-sort Index () (_ BitVec 1))


;; apply(Ls,src, {op:=0} | {op := 1} ) ;apply(Lt,dst, {op:=0} | {op:=1} )
;; translates to
;; apply(LsLt,(src,dst), { op:=0; op:=0 } | { op:=0; op:=1 } | { op:=1;op:=0 } | { op:=1;op:=1 } )
;; simplifies to 
;; apply(LsLt',(dst), { op:=0 } | { op:=1 } )



;; apply(Rt, (src,dst), { op:=0 } | { op:=1 })


;; Translate update from Rt -> Ls & Lt
;; (+, Rt, (4,5), 0)
;; -> (+, LsLt, (4, 5), 0) or (+, LsLt, (4,5), 2)
;; -> (+, Ls, 4, 0); (+, Lt, 5, 0) or (+, Ls, 4, 1); (+, Lt, 5, 0)
;; (+, Rt, (5,5), 1)
;; -> (+, LsLt, (5,5), 0) or (+, LsLt, (5,5), 2)

;; Translate update from LsLt to Rt
;; (+, LsLt, (4, 5), 0)
;; -> (+, Rt, (4, 5), 0)
;; (+, LsLt, (4,5), 1)
;; -> (+, Rt, (4,5) 1)
;; (+, LsLt, (4,5), 2)
;; -> (+, Rt, (4,5), 0)
;; (+, LsLt, (4,5), 3)
;; -> (+, Rt, (4,5), 1)
(declare-fun f10dst (Addr Addr) Addr)
(declare-fun f20 (Addr Addr) Index)

;; assert that (src = s, dst = d, op := 0) can be implemented
(assert (forall ((srcIn Addr) (srcKey Addr) (srcOut Addr)
		 (opOut OpCode)
		 (dstIn Addr) (dstKey Addr) (dstOut Addr))
		(= (and (= srcIn srcKey)
			(= srcIn srcOut)
			(= dstIn dstKey)
			(= dstIn dstOut)
			(= #b0 opOut))
		   (and (= dstIn (f10dst srcKey dstKey))
			(or (and (= #b0 (f20 srcKey dstKey))
				 (= srcIn srcOut)
				 (= dstIn dstOut)
				 (= #b0 opOut))
			    (and (= #b1 (f20 srcKey dstKey))
				 (= srcIn srcOut)
				 (= dstIn dstOut)
				 (= #b1 opOut)))))))


(check-sat) 
