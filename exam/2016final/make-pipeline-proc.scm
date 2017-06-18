#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating this function, the program in the input can use it
 '
(define (make-pipeline-proc . arg)
;MY CODE BEGIN
	(if (null? arg) (lambda (a) a)
		(lambda (x)
			((apply make-pipeline-proc (cdr arg)) ((car arg) x))))
)
;MY CODE END
env)

(define (myloop)
	(let ((codes (read)))
	(if (eq? codes eof)
		(void)
		(let ((result (eval codes env)))
			(if (eq? (void) result)
				(myloop)
				(begin (displayln result) (myloop)))))))
              
(myloop)
