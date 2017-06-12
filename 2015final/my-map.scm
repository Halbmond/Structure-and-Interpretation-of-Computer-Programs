#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your my-map, the program in the input can use my-map
 '

(define (my-map proc . args)
;MY CODE BEGIN
	(define (filter pred lst)
		(if (null? lst)
			'()
			(if (pred (car lst))
				(cons (car lst) (filter pred (cdr lst)))
				(filter pred (cdr lst))
			)
		)
	)
	(let ([non-empty-args (filter pair? args)])
		(if (null? non-empty-args)
			'()
			(cons (apply proc (map car non-empty-args))
				(apply my-map (cons proc (map cdr non-empty-args)))
			)
		)
	)
)
;MY CODE END
env)

(define (myloop)
  (let ((codes (read)))
	(if (eq? codes eof)
		(void)
		(begin  (displayln (eval codes env)) (myloop)))))


(myloop)
