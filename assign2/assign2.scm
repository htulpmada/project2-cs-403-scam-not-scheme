(define (author)
	(println "AUTHOR: Adam Pluth apluth@crimson.ua.edu")
)
(include "table.scm")



;-----------task 1------------;

(define (iterate # $i lst $)
	(define l 
	  (cons
	    'lambda
	      (cons 
		'(i)
	        $ 
	    )
	  )
	)
	(define ll (eval l this))
	(define (it scope $i lst lam)
;(inspect lst)
	  (cond ((=(length lst) 1)   
		(define $i (car lst))
		(eval (ll $i) scope)
	  )
		(else 	(define $i (car lst))
			(eval (ll $i) scope)
			(it scope $i (cdr lst) lam)))
)
	(define $i (car lst))
;(inspect $i)
;	(eval (ll $i) #)
	(it # $i lst ll)
)

;-----------task 2-----------;

;(define (peval f . a) 
;	(define l 
;(lambda (b) (f b a)))
;(inspect l)
;)

(define (peval fn @)

;	(inspect @)
	(define (ffn @)
		(define arg1 @)
;		(inspect arg1)
		(lambda (opt-arg)
			(cond((null? opt-arg) arg1)
				;(inspect 2)
				;((null? agr1) opt-arg)
				(ffn (fn (car opt-arg) ag1))
			)
		)
	)
	(inspect @)
	ffn
)


;-----------task 3-----------;

(define (Stack)
	;(cons 0 nil)
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

;----------task 4----------;

(define (no-locals @)
	(define lam (cons
	 (car (car @)) (cdr (car @))))
	(inspect (car lam))
	(inspect (cdr lam))
;	(cond
;	((? lam nil) lam)
;	(no-locals (cdr lam));)
)


;-------------task 5-----------;

(define pred 
;(define pred
	    (lambda (n)
		(lambda (f)
		    (lambda (x)
			(((n succ) (lambda (u) x)) (lambda (u) u))
		    )
		)
	    )
	)
(define zero (lambda (u) x))
(define succ (lambda (b) (lambda (h) (h (b f)))))
(define iden (lambda (u) u)) 
;	
;	(define (c->i chrch)
;	  (
;	  (chrch
;		(lambda (a) (+ a 1))
;	  )
;	  0
;	  )
;	)
;	
;	(define (i->c n)
;		(if (= n 0)
;			zero
;			(succ (i->c (- n 1)))
;		)
;	)
;	
	;church to int
	;int -1
	;int to church 
;	(define i (c->i l))
;	(i->c (- i 1))




;-----------task 7-------------;

;(define (run7)
;	(inspect (queens 0))
;	(inspect (queens 1))
;	(inspect (queens 2))
;	(inspect (queens 3))
;	(inspect (queens 4))
;	(inspect (length(queens 5)))
;	(inspect (length(queens 7)))
;)
;queens function

(define (enum low high)
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
	(accumulate append '() (map pro s)))

;main 

(define (queens size)
	(define (q-col k)
		(if (= k 0)
			(list empty-b)
				(filter
				(lambda (pos) (safe k pos))
				(flatmap
				(lambda (rest)
					(map (lambda (newr)
						(adjoinPos newr k rest))
						(enum 1 size)))
			(q-col (- k 1))))))
		(define answer(q-col size))
		(cond ((equal? answer nil) "()") 
			(else answer ))
)

(define empty-b '())

(define (adjoinPos new-r k rest)
	(cons (list k new-r) rest))

(define (safe k posistions)
	(define (safe-r?)
	  (null? (filter (lambda (pos) (= (cadr pos) (cadar posistions)))
	(cdr posistions))))
	(define (safe-d?)
	  (null? (filter (lambda (pos) (= (abs (- (caar posistions) (car pos))) (abs (- (cadar posistions) (cadr pos))))) (cdr posistions))))
	(and (safe-r?) (safe-d?)))

(define (last-col-p k)
	(map (lambda (j) (list j k))
		(enum 1 (- k 1))))

(define (cadar l)
	(car (cdr (car l)))
)

;-------task 8--------;


(define (cxr a)
	(define a (string a))
	(define (customEval arg)
		(cond
		((null? arg) nil)
		((equal? (car arg) "a");------------this is backwards------- 
			(cons '(define x (car x)) (customEval (cdr arg)))
		)
		((equal? (car arg) "d") 
			(cons '(define x (cdr x)) (customEval (cdr arg)))
		)
		(else 
		(customEval (cdr arg))))
	)
(define i (reverse (customEval a)))
(define l (cons 'lambda (cons '(x) i)))
;(inspect l) ;-----------view final lambda----------;
	(define lam (eval l this))
)

;---------------task 9-----------------;

(define old+ +)
(define old- -)
(define old* *)
(define old/ /)

(define (make-type z)
	(inspect (type z))
	(cond	((equal? (type z) 'INTEGER) (makeInteger z))
		((String? z) (makeString z))
		(else (makeOther z)))
)

(define (makeOther x)
	(list 'Other x)
)

(define (makeString x)
	(print "str made")
	(list 'STRING x)
)
(define (makeInteger y)
	(print "int made")
	(list 'INTEGER y)
)
(define (typ a)
	(car a)
)
(define (contents a)
	(cdr a)
)

(define (install-generic)
	(clearTable)
	(set! + (lambda( a b) (apply-generic '+ (make-type a) (make-type b))))
	(set! - (lambda( a b) (apply-generic '- (make-type a) (make-type b))))
	(set! * (lambda( a b) (apply-generic '* (make-type a) (make-type b))))
	(set! / (lambda( a b) (apply-generic '/ (make-type a) (make-type b))))
	(putTable '+ '(STRING STRING) addStrings)
	(putTable '+ '(STRING INTEGER) addStringAndInteger)
	(putTable '+ '(INTEGER STRING) addIntegerAndString)
	(putTable '- '(STRING STRING) subStrings)
	(putTable '- '(STRING INTEGER) subStringAndInteger)
	(putTable '- '(INTEGER STRING) subIntegerAndString)
	(putTable '* '(STRING STRING) mulStrings)
	(putTable '* '(STRING INTEGER) mulStringAndInteger)
	(putTable '* '(INTEGER STRING) mulIntegerAndString)
	(putTable '/ '(STRING STRING) divStrings)
	(putTable '/ '(STRING INTEGER) divStringAndInteger)
	(putTable '/ '(INTEGER STRING) divIntegerAndString)
	'generic-system-installed
	)

(define (uninstall-generic)
	(set! + old+)
	(set! - old-)
	(set! * old*)
	(set! / old/)
	'generic-system-uninstalled
)

(define (apply-generic op arg1 arg2 )
;	(define arg1 (make-type arg1))
;	(define arg2 (make-type arg2))
	(inspect arg1)
	(inspect arg2)
	(let ((types (cons (typ arg1) (typ arg2)))
		(f (get op types)))
		(apply f (map contents (list arg1 arg2)))
	)
)

(define (addStrings s1 s2)
	(append s1 s2)
)
(define (addStringAndInteger s i)
	(append s (String i))
)
(define (addIntegerAndString i s)
	(old+ i (Integer s))
)
(define (subStrings s1 s2)

)
(define (subStringAndInteger s i)

)
(define (subIntegerAndString i s)

)
(define (mulStrings s1 s2)

)
(define (mulStringAndInteger s i)

)
(define (mulIntegerAndString i s)

)
(define (divStrings s1 s2)

)
(define (divStringAndInteger s i)

)
(define (divIntegerAndString i s)

)






;-----------------------------test functions----------------------------------
(define (run1)
	(inspect (iterate i (list 5 2 4) (inspect i)))
	(inspect (iterate i (list 4 8 1) (inspect i) (inspect (+ i i))))
	(inspect (iterate i (list 5 7 1) (inspect i) (inspect (* i i))))
)

(define (run2)
	(define . 'MISSING)
	(define (f x y z) (+ x y z))
	(inspect((peval f 1 . . ) 2 3))
	(inspect((peval f 1 2 . ) 3))
	(inspect((peval f 1 2 3)))
)

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

(define (run5)
	(define z (lambda (f) (lambda (x) x)))
	(define one  (lambda (f) (lambda (x) (f x))))
	(define two  (lambda (f) (lambda (x) (f (f x)))))
	(define three  (lambda (f) (lambda (x) (f (f (f x))))))
	(inspect(((pred one)+)0))
;	(inspect(((pred two)+)0))
	(inspect(pred three))
)

;(define (run6)

;)

(define (run7)
	(inspect (queens 0))
	(inspect (queens 1))
	(inspect (queens 2))
	(inspect (queens 3))
	(inspect (queens 4))
	(inspect (length(queens 5)))
;	(inspect (length(queens 7)))
)

(define (run8)
	(inspect(cxr 'a))
	(inspect(cxr 'd))
	(inspect((cxr 'ad) (cons 1 (cons 2 (cons 3 nil)))))
	(inspect(cxr 'dddad))
	(inspect(cxr 'adddaaa))
)

(define (run9)
	(inspect apply-generic)
        (inspect (install-generic))
      ; (ppTable (+ 0 0))
	
)

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
