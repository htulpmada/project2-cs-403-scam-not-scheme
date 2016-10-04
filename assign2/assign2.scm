(define (author)
	(println "AUTHOR: Adam Pluth apluth@crimson.ua.edu")
	)

(define (run1)
	(iterate i (list 5) (inspect i))
)

(define (iterate # $i lst $)
	(define l 
	  (cons
	    'lambda
	      (cons 
		'(x)
	        (cons $ nil)
	    )
	  )
	)
	(define ll (eval l #))
(inspect ll)
	(define $i (getElement lst 0))
(inspect $i)
(ppTable #)
	(ll (eval $i #))
)

(define (Stack)
	(define store nil)

	(define (push x)
		(define store (cons x store))
	)
	(define (speek)
		(define tmp (car store))
	)
	(define (pop)
		(define tmp (car store))
		(define store (cdr store))
		tmp
	)
	(define (empty?)
		(eq? store nil)
	)
	this
)








;-----------------------------test functions----------------------------------
;(define (run1)

;)

;(define (run2)

;)

(define (run3)

)

;(define (run4)

;)

;(define (run5)

;)

;(define (run6)

;)

;(define (run7)

;)

;(define (run8)

;)

;(define (run9)

;)

;(define (run10)

;)

;(run1)
;(run2)
;(run3)
;(run4)
;(run5)
;(run6)
;(run7)
;(run8)
;(run9)
;(run10)
(author)
