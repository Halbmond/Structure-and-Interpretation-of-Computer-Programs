#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 '
;MY CODE BEGIN
(define (count-pairs x)
	(cond
		[(not (pair? x)) 0]
		[(eq? (car x) 'Visited) 0]
		[else (let ((car-x (car x)))
			(set-car! x 'Visited)
			(+ 1 (count-pairs car-x) (count-pairs (cdr x))))]))
;MY CODE END
#|
(define (count-pairs x)
	(define visit '())
	(define (count-pairs-dfs x)
		;(printf "x = ~a\n" x)  couldn't use set or printf, they're undefined.
		(cond
			[(not (pair? x)) 0]
			[(member x visit) 0]
			[else
				(begin (set! visit (cons x visit))
					(+ 1
						(count-pairs-dfs (car x))
						(count-pairs-dfs (cdr x))))]))
	(count-pairs-dfs x)
)
|#
env)

(define (myloop)
	(define (eval-codes codes last-val)
		(if (null? codes)
				last-val
				(eval-codes (cdr codes) (eval (car codes) env))))
		
	(let ([codes (read)])
		(if (eq? codes eof)
				(void)
				(begin (displayln (eval-codes codes (void))) (myloop)))))


(myloop)


#|
INPUT
((count-pairs (list 1 2 3 4 5 6)))
((define x4 (list (list 1) 2))  (set-cdr! (car x4) (cdr x4))   (count-pairs x4))
((define e1 (cons 'a '())) (define e2 (cons e1 e1)) (define e7 (cons e2 e2)) (count-pairs e7))

OUTPUT
6
3
3
|#
