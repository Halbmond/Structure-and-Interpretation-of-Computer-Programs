#lang racket
(define (accumulate op init seq)
	(if (null? seq)
			init
			(op (car seq) (accumulate op init (cdr seq)))))
(define (enumerate-interval a b)
	(if (> a b)
			'()
			(cons a (enumerate-interval (+ a 1) b))))
(define (flatmap proc seq)
	(accumulate append '() (map proc seq)))
(define (tri-num-list n s)
;MY CODE BEGIN
	(for*/list ([i n] [j n] [k n] #:when (and (< i j k) (= (+ i j k 3) s)))
		(map add1 (list i j k))))
;MY CODE END
 
(define (myloop)
	(let ([n (read)] [s (read)])
		(unless (eq? n eof) (displayln (tri-num-list n s)) (myloop))))
(myloop)
