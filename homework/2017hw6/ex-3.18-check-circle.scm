#lang racket
(require r5rs)
(define env (scheme-report-environment 5))

(eval '(define (last-pair lst)
				 (if (null? (cdr lst))
						 lst
						 (last-pair (cdr lst))))
			env)

(eval '(define (make-cycle lst)
				 (set-cdr! (last-pair lst) lst)
				 lst)
			env)

(eval '
;MY CODE BEGIN
(define (check-cycle x)
	(cond
		[(not (pair? x)) #f]
		[(eq? (car x) 'Visited) #t]
		[else (set-car! x 'Visited) (check-cycle (cdr x))]))
#|
(define (check-cycle x)
	(define visit '())
	(define (check-cycle-dfs x)
		(if (member x visit) #t
			(if (pair? x)
				(begin (set! visit (cons x visit)) (check-cycle-dfs (cdr x)))
				#f)
		)
	)
	(check-cycle-dfs x)
)
|#
;MY CODE END
env)

(define (myloop)
	(define (eval-codes codes last-val)
		(if (null? codes)
				last-val
				(eval-codes (cdr codes) (eval (car codes) env))))
		
	(let ((codes (read)))
		(if (eq? codes eof)
				(void)
				(begin (displayln (eval-codes codes (void))) (myloop)))))


(myloop)


#|
INPUT
((check-cycle (list 1 2 3 4 5 6)))
((define x4 (list (list 1) 2))  (set-cdr! (car x4) (cdr x4))   (check-cycle x4))
((define e1 (cons 'a '())) (define e2 (cons e1 e1)) (define e7 (cons e2 e2)) (check-cycle e7))
((define clst (make-cycle (list 1 2 3 4 5 6 7 8 9 10))) (check-cycle clst))

OUTPUT
#f
#f
#f
#t
|#
