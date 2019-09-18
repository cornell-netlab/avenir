(define-sort Addr () (_ BitVec 2))
(define-sort Loc () (_ BitVec 2))
(define-sort Tag () (_ BitVec 1))
(define-sort Index () (_ BitVec 3))

(echo "synthesize function" "")
(assert
 (forall ((dest Addr) (gamma Loc) (h0 Tag) (h1 Tag)
	  (?1 Addr) (?2 Addr) (?3 Addr) (?4 Addr) (?5 Addr) (?6 Addr) (?7 Addr) (?8 Addr))
	 (exists ((??000 Tag) (??001 Tag) (??010 Tag) (??011 Tag) (??100 Tag) (??101 Tag) (??110 Tag) (??111 Tag))
		  (=>
		   (or                                  
		    (and (= dest ?1 ?3) (= gamma #b00)) 
		    (and (= dest ?1 ?4) (= gamma #b01))
		    (and (= dest ?2 ?3) (= gamma #b00))
		    (and (= dest ?2 ?4) (= gamma #b01))
		    (and (= dest ?5 ?7) (= gamma #b10))
		    (and (= dest ?5 ?8) (= gamma #b11))
		    (and (= dest ?6 ?7) (= gamma #b10))
		    (and (= dest ?6 ?8) (= gamma #b11))
		    )
		   (or                                                             ;; dest @ loc
		    ;; dest = 0   
		    (and (= dest #b00) (= ??000 #b0) (= ??001 #b0) (= gamma #b00)) ;; 0 @ 0
		    (and (= dest #b00) (= ??000 #b0) (= ??001 #b1) (= gamma #b01)) ;; 0 @ 1
		    (and (= dest #b00) (= ??000 #b1) (= ??001 #b0) (= gamma #b10)) ;; 0 @ 2
		    (and (= dest #b00) (= ??000 #b1) (= ??001 #b1) (= gamma #b11)) ;; 0 @ 3
		    ;; dest = 1
		    (and (= dest #b01) (= ??010 #b0) (= ??011 #b0) (= gamma #b00)) ;; 1 @ 0
		    (and (= dest #b01) (= ??010 #b0) (= ??011 #b1) (= gamma #b01)) ;; 1 @ 1
		    (and (= dest #b01) (= ??010 #b1) (= ??011 #b0) (= gamma #b10)) ;; 1 @ 2
		    (and (= dest #b01) (= ??010 #b1) (= ??011 #b1) (= gamma #b11)) ;; 1 @ 3
		    ;; dest = 2
		    (and (= dest #b10) (= ??100 #b0) (= ??101 #b0) (= gamma #b00)) ;; 2 @ 0
		    (and (= dest #b10) (= ??100 #b0) (= ??101 #b1) (= gamma #b01)) ;; 2 @ 1
		    (and (= dest #b10) (= ??100 #b1) (= ??101 #b0) (= gamma #b10)) ;; 2 @ 2
		    (and (= dest #b10) (= ??100 #b1) (= ??101 #b1) (= gamma #b11)) ;; 2 @ 3
		    ;; dest = 3
		    (and (= dest #b11) (= ??110 #b0) (= ??111 #b0) (= gamma #b00)) ;; 3 @ 0
		    (and (= dest #b11) (= ??110 #b0) (= ??111 #b1) (= gamma #b01)) ;; 3 @ 1
		    (and (= dest #b11) (= ??110 #b1) (= ??111 #b0) (= gamma #b10)) ;; 3 @ 2
		    (and (= dest #b11) (= ??110 #b1) (= ??111 #b1) (= gamma #b11)) ;; 3 @ 3
		    )))))
(check-sat)
(get-model)
