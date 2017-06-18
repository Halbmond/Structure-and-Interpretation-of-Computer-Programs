#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your my-map-ex, the program in the input can use my-map-ex
 '

(define (my-map-ex proc . args)
;MY CODE BEGIN
	(define (make-list k v)
		(if (= k 0) '()
			(cons v (make-list (- k 1) v))))
	(define (last a)
		(if (eq? (cdr a) '()) (car a)
			(last (cdr a)))
	)
	(if (eq? args '())
		'()
		(begin
			;(displayln args)
			(let ((len (apply max (map length args))))
			(apply map
				(cons proc
					(map (lambda (a) (append a (make-list (- len (length a)) (last a))))
					args)
				)
			)
			)
			;(displayln len)
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
