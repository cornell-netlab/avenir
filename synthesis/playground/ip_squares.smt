
;; Implementing IP over IP & Eth networks
;;

;; (declare-fun f (Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int) Int)

(define-fun logical_prec ((loc Int) (ipDst Int) (gamma Int) (sigma Int)
			  (?ipDst Int) (?sigma Int))
  Bool
  ;; (or (and (= loc 0)
  ;; 	   (= ipDst ?1)
  ;; 	   (= ipDst ?7)
  ;; 	   (= gamma 11)
  ;; 	   (= ?8 sigma))
  (and (= loc 1)
       (= ipDst ?ipDst) ;; 101
       (= gamma 100)
       (= ?sigma sigma))) ;; 49

(define-fun real_prec ((loc Int) (ipDst Int) (dstMac Int) (srcMac Int)
		       (alpha Int) (gamma Int) (sigma Int) (delta Int)
		       (??ipDst Int) (??dstMac Int) (??srcMac Int) 
		       (??alpha Int) (??sigma Int) (??delta Int))
  Bool
  ;; (or (and (= loc 0) (= ipDst ??_5_10_0) (= ??_1_5_1 ??_5_10_0)
  ;; 	   (= dstMac ??_5_10_2) (= ??_5_10_4 sigma) (= ??_5_10_3 alpha)
  ;; 	   (= delta ??_10_5) (= 11 gamma))
  (and (= loc 1)
       (= gamma 100)
       (= sigma ??sigma) ;; 49
       (= ipDst ??ipDst) ;; 101
       (= dstMac ??dstMac) ;; 49
       (= srcMac ??srcMac) ;; 49
       (= alpha ??alpha) ;; 101
       (= delta ??delta))) ;; 49


;; (assert ((forall ((loc Int) (ipDst Int) (dstMac Int) (srcMac Int)
;; 		 (alpha Int) (beta Int) (gamma Int) (sigma Int) (delta Int)
;; 		 (?ipDst Int) (?sigma Int))
		     
;; 		(exists ((??ipDst Int) (??dstMac Int) (??srcMac Int) (??alpha Int)(??sigma Int) (??delta Int))
;; 			;; (implies (and
;; 			;; 	  (or (= loc 1) (= loc 0))
;; 			;; 	  (or (=  gamma 100) (= gamma 11))
;; 			;; 	  (< 100 ipDst 110) (< 48 srcMac 58) (< 48 dstMac 58)
;; 			;; 	  (< 100 alpha 110) (< 48 sigma  58) (< 48 delta  58)
;; 			;; 	  (< 100 ?ipDst 110) (< 48 ?sigma 58)
;; 			;; 	  )
				 
;; 			;; 	 (and (< 100 ??ipDst 110) (< 48 ??srcMac 58) (< 48 ??dstMac 58)
;; 			;; 	      (< 100 ??alpha 110) (< 48 ??sigma 58) (< 48 ??delta 58)
;; 			(implies (logical_prec loc ipDst gamma sigma ?ipDst ?sigma)
;; 			    (real_prec loc ipDst dstMac srcMac alpha gamma sigma delta
;; 				       ??ipDst ??dstMac ??srcMac ??alpha ??sigma ??delta))))))
;; (check-sat)
;; (get-model)


(declare-const i1 Int)
(declare-const i2 Int)
(declare-const i3 Int)
(declare-const i4 Int)
(declare-const i5 Int)
(declare-const i6 Int)
(define-fun f ((idx Int) (?3 Int) (?4 Int)
	       (loc Int) (ipDst Int) (dstMac Int) (srcMac Int)
	       (alpha Int) (delta Int) (gamma Int) (sigma Int)) Int
  (ite (= idx 1) ?3
       (ite (= idx 2) ?4
	    (ite (= idx 3) loc
		 (ite (= idx 4) ipDst
		      (ite (= idx 5) dstMac
			   (ite (= idx 6) srcMac
				(ite (= idx 7) alpha
				     (ite (= idx 8) delta
					  (ite (= idx 9) gamma
					       (ite (= idx 10) sigma 0)))))))))))

(assert (forall ((loc Int) (ipDst Int) (dstMac Int) (srcMac Int) (alpha Int) (delta Int) (gamma Int) (sigma Int) (?3 Int) (?4 Int))
		;; (exists ((??ipDst Int) (??sigma Int) (??dstMac Int) (??srcMac Int) (??alpha Int) (??delta Int))
			(implies (and (= loc 1)
				      (= gamma 100)
				      (= ipDst ?3)
				      (= sigma ?4))
				 (and (= loc 1)
				      (= gamma 100)
				      (= ipDst (f i1 ?3 ?4 loc ipDst dstMac srcMac alpha delta gamma sigma))  ;; ??ipDst)
				      (= sigma (f i2 ?3 ?4 loc ipDst dstMac srcMac alpha delta gamma sigma)) ;;??sigma)
				      (= dstMac (f i3 ?3 ?4 loc ipDst dstMac srcMac alpha delta gamma sigma)) ;; ??dstMac)
				      (= srcMac (f i4 ?3 ?4 loc ipDst dstMac srcMac alpha delta gamma sigma)) ;;??srcMac)
				      (= alpha (f i5 ?3 ?4 loc ipDst dstMac srcMac alpha delta gamma sigma)) ;; ??alpha)
				      (= delta (f i6 ?3 ?4 loc ipDst dstMac srcMac alpha delta gamma sigma)) ;; ??delta)
				      ))))
(check-sat)
(get-model)

