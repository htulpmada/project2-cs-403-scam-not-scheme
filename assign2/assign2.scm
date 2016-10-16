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

(define (run7)
	(inspect (queens 0))
	(inspect (queens 1))
	(inspect (queens 2))
	(inspect (queens 3))
	(inspect (queens 4))
	(inspect (length(queens 5)))
)

;queens function

(define (enum low high)
;	(inspect low)
;	(inspect high)
	(if (> low high)
		'()
		(cons low (enum (+ low 1) high))))

(define (filter pred seq)
	(cond ((null? seq) '())
		((pred (car seq))
		(cons (car seq)
			(filter pred (cdr seq))))
		(else (filter pred (cdr seq)))))

(define (accumulate op init seq)
	(if (null? seq)
		init
		(op (car seq)
			(accumulate op init (cdr seq)))))
(define (flatmap pro s)
;(inspect s)
	(accumulate append '() (map pro s)))

;main 

(define (queens size)
	(define (q-col k)
;	(inspect size)
		(if (= k 0);make iterative will order correctly
			(list empty-b)
				(filter
				(lambda (pos) (safe k pos))
				(flatmap
				(lambda (rest)
					(map (lambda (newr)
						(adjoinPos newr k rest))
;						(inspect rest)
						(enum 1 size)))
			(q-col (- k 1))))))
		(define answer(q-col size))
		(cond ((equal? answer nil) empty-b) 
			(else answer ))
)

(define empty-b '())

(define (adjoinPos new-r k rest)
;(inspect rest)
	(cons (list k new-r) rest))

(define (safe k posistions)
;(inspect k)
;(inspect posistions)
	(define (safe-r?)
	  (null? (filter (lambda (pos) (= (cadr pos) (cadar posistions)))
	(cdr posistions))))
	(define (safe-d?)
	  (null? (filter (lambda (pos) (= (abs (- (caar posistions) (car pos))) (abs (- (cadar posistions) (cadr pos))))) (cdr posistions))))
;(inspect (safe-r?))
;(inspect (safe-d?))
	(and (safe-r?) (safe-d?)))

(define (last-col-p k)
	(map (lambda (j) (list j k))
		(enum 1 (- k 1))))

(define (cadar l)
	(car (cdr (car l)))
)

(define (run8)
	(inspect(cxr 'a))
	(inspect(cxr 'd))
	(inspect((cxr 'ad) (cons 1 (cons 2 (cons 3 nil)))))
)

(define (cxr a)
	(define a (string a))
	(define (customEval arg)
;	(inspect (list? arg))
;	(inspect (car arg))
	(inspect  arg)
;	(inspect  (equal? arg "a"))
		(cond
		((null? arg) nil)
		((equal? (car arg) "a");------------this is backwards------- 
	;		(define s (customEval (cdr arg))	
			(cons '(define x (car x)) (customEval (cdr arg)))
		)
		((equal? (car arg) "d") 
			(cons '(define x (cdr x)) (customEval (cdr arg)))
		)
		(else 
		(inspect (cdr arg))
		(customEval (cdr arg))))
	)
	(define l (cons
		'lambda
		(cons '(x)
			(customEval a);needs to return cons symbols for x in cxr
			)
		)
	)
(inspect l)
	(define lam (eval l this))

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
