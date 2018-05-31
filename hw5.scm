; Return #t if obj is an empty listdiff, #f otherwise.
;checks if obj is a pair then checks if the address
;if the head and tail in the pair is the same address
;if it is the same address then it is empty, because of 
;what listdiff (the difference of a list)
(define (null-ld? obj)  
		(if (pair? obj) ;if is pair
			(eq?(car obj)(cdr obj)) ;then check if car and cdr point to same address 
			#f ;else f
		) 
)

;A listdiff is a pair whose car is L and whose cdr is eq? 
;to either L, or to (cdr L), or to (cdr (cdr L))), etc
;Return #t if obj is a listdiff, #f otherwise.
(define (ld? obj)
			(if (null-ld? obj) #t  ;if obj is an empty listdiff then true else check 
			(and (pair? obj) (not(null? obj)) (not(null? (car obj))) (pair? (car obj)) 
				(ld? (cons (cdr (car obj)) (cdr obj)))
			 )
			)
)

;Return a listdiff whose first element is obj and whose remaining elements 
;are listdiff. (Unlike cons, the last argument cannot be an arbitrary object
; it must be a listdiff.)
(define (cons-ld obj listdiff)
		(if (ld? listdiff) (cons (cons obj (car listdiff)) (cdr listdiff)) #f)
		;checks if last argument is a listdiff
)

;Return the first element of listdiff. It is an error if listdiff has no elements. 
;("It is an error" means the implementation can do anything it likes when this happens, 
;and we won't test this case when grading.)
;return false if error
(define (car-ld listdiff)
		(and (ld? listdiff) (if (null-ld? listdiff) #f #t) (car (car listdiff)) )

)

;Return a listdiff containing all but the first element of listdiff. It is an 
;error if listdiff has no elements
(define (cdr-ld listdiff)
		(and (ld? listdiff) (if (null-ld? listdiff) #f #t) (cons (cdr (car listdiff)) (cdr listdiff)) )		
)

;(ld obj …)
;Return a newly allocated listdiff of its arguments.
;FRom Piazza:
;You just have to return a listdiff that isn't the same as some other listdiff. 
(define (ld . args) ;dot notation for declaring a procedure that receives a variable number of arguments
	  			
	  		(cons args null) ;Every call to cons/list allocates a new object.
	  		;cons obj and arg  and cons that with with empty list 
)

;Return the length of listdiff.
(define (length-ld listdiff)
			(if (ld? listdiff) ;if listdiff is listdiff 
					(let c_len ( (lst listdiff) (cnt_len 0) ) ;c_len helps count as you go through listdiff
						(if (null-ld? lst) ;if lst is empty then count is still 0
							cnt_len
							(c_len (cons (cdr (car lst)) (cdr lst)) (+ cnt_len 1)) ;else go through listdiff and increment count
						)
					)

				"error" ;else not listdiff then error 
			)
)

;Return a listdiff consisting of the elements of the first listdiff followed by the elements of the other
;listdiffs. The resulting listdiff is always newly allocated, except that it shares structure with
;the last argument. (Unlike append, the last argument cannot be an arbitrary object; it must be a listdiff.)
(define (append-ld listdiff . arg)
	(if (null? arg)  ;args empty return listdiff
		listdiff
	  	(apply append-ld (cons (append (ld->list listdiff) (car (car arg)) ) (cdr (car arg)))  (cdr arg) )
	  	;applies append-ld to append the ld->list of listdiff (a d) to the very first el of agr 
	  	;then cons that to the the second elemet of arg; kind of like now the thing that was append is now the head
	)
)	

; ;Return listdiff, except with the first k elements omitted. If k is zero, return listdiff. It is an error 
; ;if k exceeds the length of listdiff.
(define (ld-tail listdiff k)
		(if ( < k 0)	
			"error"
			(if ( > k (length-ld listdiff) ) 
				"error" ;if k > (length of listdiff) error	
				;(if (= k (length-ld listdiff))
					;null ;returns the difference of the returned listdiff and listdiff -> they both point to the same thing
					(if (= k 0)
						listdiff
						(let get_els ((lst listdiff) (num k)) 
							(if (= num 0) 
								lst
								(get_els (cons (cdr (car lst))(cdr lst)) (- num 1))
							)

						)
					)
				;)	
			)
		)
)

;Return a listdiff that represents the same elements as list.
(define (list->ld list)
	(if (not(list? list))
		"error"
		 (if (null? list)
		 	(cons list list)
		 	(cons (list) null)
		 )
		 
	)
)

;Return a list that represents the same elements as listdiff.
;takes length-ld listdiff number of elements and appends that 
(define (ld->list listdiff)
	(if (not(ld? listdiff)) ;if listdiff is not a listdiff then error
		"error"
			(let create_l ((lst (car listdiff))) ;tail-rercusion
				(if (eq? lst (cdr listdiff)) ;keep recursing until the car of the new/current list is eq?
											 ;to the cdr of the original listdiff and return null if eq?
				 	 null
				 	 (cons (car lst) (create_l (cdr lst))) ;else cons the car of the lst with the call on 
				 	 										;create_l that does all that on the cdr of the current list
				)

			)

	)
)

; ;(map-ld proc listdiff1 listdiff2 ...)
; ;The listdiffs should all have the same listdiff length. Proc should accept as many arguments as there are 
; ;listdiffs and return a single value. Proc should not mutate any of the listdiffs. The map-ld procedure 
; ;applies proc element-wise to the elements of the listdiffs and returns a listdiff of the results, in order. 
; ;This acts like the standard map function, except that it uses listdiffs instead of lists, and it avoids the 
; ;overhead of converting its listdiff arguments to lists.

(define (map-ld proc . arg) ; . arg turns '((1 1 1)) '((1 1 1)) into '( ((1 1 1)) ((1 1 1)) )
		(define (use_proc lst) 
			(if (null? lst) ;if lst is null then null  
				null
				(cons (apply proc (car lst)) (use_proc (cdr lst))  )
				;apply proc to the head of the lst and cons it to the tail of the list, which
				;is passed into use_proc 
			)
		)

		(cond 
			[(null? arg) (ld arg)] ;if arg is null return the ld of arg 
			[else (list (use_proc (apply map list (map car arg) ) ) )] ;else call use_proc and list the result to make it a list

		) 
)

; ;Return a Scheme expression that is like expr except that it uses listdiff procedures instead
; ;of the corresponding list procedures.
(define (expr2ld expr)
		; (let ([hd (car expr)]))
		; (let ([tl (cdr expr)]))
		(if (eq? expr null)
			expr
			
			(begin
				;(define hd (car expr) ) 
				;(define tl (cdr expr) ) 
				; (if (eq? hd 'map) 
				; 	#t
				; 	#f
				; )
				(cond 
						[(eq? (car expr) null) expr] 
						[(eq? (car expr) 'map) (cons 'map-ld (expr2ld (cdr expr)) ) ]
						[(eq? (car expr) 'null?) (cons 'null-ld? (expr2ld (cdr expr)) )]
						[(eq? (car expr) 'cons) (cons 'cons-ld (expr2ld (cdr expr)) )]	
						[(eq? (car expr) 'cdr) (cons 'cdr-ld (expr2ld (cdr expr)) )]
						[(eq? (car expr) 'car) (cons 'car-ld (expr2ld (cdr expr)) )]
						[(eq? (car expr) 'length) (cons 'length-ld (expr2ld (cdr expr)) )]
						[(eq? (car expr) 'append) (cons 'append-ld (expr2ld (cdr expr)) )]
						[(eq? (car expr) 'list) (cons 'ld (expr2ld (cdr expr)) )]
						[(eq? (car expr) 'list-tail) (cons 'ld-tail (expr2ld (cdr expr)) )]
						[(eq? (car expr) 'list?) (cons 'ld? (expr2ld (cdr expr)) )]

						[(list? (car expr)) (cons (expr2ld (car expr)) (expr2ld (cdr expr)) ) ]

						[ else (cons (car expr) (expr2ld (cdr expr)) ) ]
				)
			)
		)
)
