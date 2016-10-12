(define (author)
	(println "AUTHOR: Adam Pluth apluth@crimson.ua.edu")
	)

;(define (run1)
;	(iterate i (list 5 2 4) (inspect i))
;)

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
	(define ll (eval l this))
(inspect ll)
	(define $i (getElement lst 0))
(inspect $i)
(ppTable #)
	(eval (ll  $i) #)
)

;(define (run2)
;	(define (f x y z) (+ x y z))
;	(inspect((peval f 1 2 ) 3))
;)

(define (peval f . a) 
	(define l (lambda (b) (f b a)))
;(inspect l)
)

(define (Stack)
	(define size 0)
	(define store nil)
)
(define (push store x)
	(define store (cons x store))
)
(define (speek store)
	(define tmp (car store));))
)
(define (pop store)
	(define store (cdr store))
)
(define (empty? store)
	(eq? store nil)
)
(define (ssize store)
	(define (itersize sz store)
	(cond
	((empty? store) sz)
	(else (itersize (+ 1 sz) (cdr store)))))
	(itersize 0 store)
)


(define (Queue)
	(define store nil)
)
(define (enqueue store x)
	(define store (append store (list x)))
)
(define (dequeue store)
	(define store (cdr store))
)
(define (qpeek store)
	(define tmp (car store))
)
(define (empty? store)
	(eq? store nil)
)
(define (qsize store)
	(define (itersize sz store)
	(cond
	((empty? store) sz)
	(else (itersize (+ 1 sz) (cdr store)))))
	(itersize 0 store)
;(length store)
)

(define (no-locals @)
	(define lam (cons
	 (car (car @)) (cdr (car @))))
	(inspect (car lam))
	(inspect (cdr lam))
;	(cond
;	((? lam nil) lam)
;	(no-locals (cdr lam));)
)
(define (run5)
	(define zero (lambda (f) (lambda (x) x)))
	(define one  (lambda (f) (lambda (x) (f x))))
	(define two  (lambda (f) (lambda (x) (f (f x)))))
	(define three  (lambda (f) (lambda (x) (f (f (f x))))))
;	(inspect(pred zero))
	(inspect(((pred one)+)0))
	(inspect(((pred two)+)1))
	(inspect(pred three))
)
(define (pred l)
	(define zero (lambda (f) (lambda (x) x)))
	(define succ 
	    (lambda (n)
		(lambda (f)
		    (lambda (x)
			(f ((n f) x))
		    )
		)
	    )
	)
;church to int
	(define (c->i chrch)
	  (
	  (chrch
		(lambda (a) (+ a 1))
	  )
	  0
	  )
	)
;int -1
;int to church 
	(define (i->c n)
		(if (= n 0)
			zero
			(succ (i->c (- n 1)))
		)
	)
(define i (c->i l))
(i->c (- i 1))
;all using helper functions on website
)




;-----------------------------test functions----------------------------------
;(define (run1)

;)

;(define (run2)

;)

(define (run3)
(inspect (Stack))
(inspect (push (Stack) 1))
(inspect (push (push (Stack) 1)2))
(inspect (pop (push (push (Stack) 1) 2)))
(inspect (ssize (pop (push (push (Stack) 1) 2))))
(define (loop stack queue x)
        (define x (+ x 1 ))
        (if (= x 10)
	    (list stack queue)
            (loop (push stack x) (enqueue queue x)x)))
(define (popper s)
        (cond
            ((!= (ssize s) 0)
                (inspect (speek s))
                (popper (pop s)))))
(define (dequeuer q)
        (cond
            ((!= (qsize q) 0)
                (inspect (speek q))
                (dequeuer (dequeue q)))))
(define data (loop (Stack) (Queue)0))
(popper (car data))
(dequeuer (cadr data))
)

(define (run4)
;	(no-locals )
)

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
