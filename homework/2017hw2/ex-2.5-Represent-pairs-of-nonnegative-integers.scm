#lang racket

;;MY CODE BEGIN

(define (extract-expt a n)
	(define (divides? a b)
		(= (remainder b a) 0))
	(define (iter n acc)
		(if (divides? a n)
				(iter (/ n a) (+ 1 acc))
				acc))
	(iter n 0))

(define (car z)
	(extract-expt 2 z))

(define (cdr z)
	(extract-expt 3 z))

;;MY CODE END

(define (fast-exp a n)
	(define (square x) (* x x))
	(define (iter a n result)
		(if (= n 0)
				result
				(if (even? n) 
						(iter (square a) (/ n 2) result)
						(iter (square a) (/ (- n 1) 2) (* a result)))))
	(iter a n 1))
	
(define (cons a b)
	(* (fast-exp 2 a) (fast-exp 3 b)))

(define (myloop)
	(let ((a (read))
				(b (read)))
		(if (eq? a eof)
				(void)
				(begin (display (car (cons a b)))
							 (display " ")
							 (display (cdr (cons a b)))
							 (newline) 
							 (myloop)))))

(myloop)

