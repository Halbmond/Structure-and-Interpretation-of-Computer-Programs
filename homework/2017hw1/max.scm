#lang racket

#|
(let ((CASE (read)))
	(for ([tCASE CASE])
		(define n (read))
		(printf "~a\n"
			(apply max (build-list n (lambda (x) (read))))
		)
	)
)
|#

#|
(void (build-list (read) (lambda (T) (displayln (apply max (build-list (read) (lambda (x) (read))))))))
|#

#|
(void (map (lambda (T) (displayln (apply max (build-list (read) (lambda (x) (read)))))) (range (read))))
|#

(for-each
	(lambda (T)
		(displayln (apply max (build-list (read) (lambda (x) (read))))))
	(range (read)))
