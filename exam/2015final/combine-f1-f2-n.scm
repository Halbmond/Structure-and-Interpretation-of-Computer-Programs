#lang racket

(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (db x) (* x 2))

(define (combine f1 f2 n)
;MY CODE BEGIN
	(define (combine2 g1 g2) (lambda (x) (g1 (g2 x))))
	(foldl combine2 values (build-list n (lambda (i) (combine2 f1 f2)))))
;MY CODE END
((combine square inc 1) 2)
((combine square inc 2) 3)
((combine db inc 3) 2)
((combine inc inc 4) 3)

(display "********") (newline)

(define (myloop)
  (let ((n (read))
        (x (read)))
    (if (eq? n eof)
        (void)
        (begin (display ((combine inc square n) x)) 
               (newline) (myloop)))))

(myloop)
