#lang racket

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define-syntax cons-stream
	(syntax-rules ()
		[(cons-stream x y) (cons x (delay y))]))
 
(define the-empty-stream '())
 
(define (stream-null? stream)
	(null? stream))

(define (stream-ref s n)  ;取 stream里面第 n 项,n从0开始算
	(if (stream-null? s) the-empty-stream
			(if (= n 0)
					(stream-car s)
					(stream-ref (stream-cdr s) (- n 1)))))
 
(define (stream-map proc s)
	(if (stream-null? s)
			the-empty-stream
			(cons-stream (proc (stream-car s)) 
									 (stream-map proc (stream-cdr s)))))

(define (display-stream-n s n)
	(if (= n 0)
			(void)
			(begin (displayln (stream-car s)) (display-stream-n (stream-cdr s) (- n 1)))))
;MY CODE BEGIN
(define (add-stream s1 s2) (cons-stream (+ (stream-car s1) (stream-car s2)) (add-stream (stream-cdr s1) (stream-cdr s2))))
(define (stream-scale s m) (stream-map (lambda (x) (* x m)) s))
(define (RC R C dt)
	(lambda (i v0)
		(define si (cons-stream v0 (add-stream (stream-scale i (/ dt C)) si)))
		(add-stream (stream-scale i R) si)))

#| CAUTION!! this is a wrong version, 输出中有 2.0 17.0, 而正确答案中是 2 17。 因为将v0/=(dt/C), v0*=(dt/C) 导致出现小数
(define (add-stream s1 s2) (cons-stream (+ (stream-car s1) (stream-car s2)) (add-stream (stream-cdr s1) (stream-cdr s2))))
(define (stream-scale s m) (stream-map (lambda (x) (* x m)) s))
(define (RC R C dt)
	(lambda (i v0)
		(define si (cons-stream (/ v0 (/ dt C)) (add-stream i si)))
		(add-stream (stream-scale i R) (stream-scale si (/ dt C))))
)
|#
;MY CODE END
(define (integers-from n)
	(cons-stream n (integers-from (+ n 1))))
(define integers (integers-from 1))
(define fibs 
	(cons-stream 0
							 (cons-stream 1
														(add-stream fibs (stream-cdr fibs)))))
							 
(define RC1 (RC 5 1 0.5))
(display-stream-n (RC1 fibs 2) 10)
(displayln "******")
(display-stream-n (RC1 integers 2) 10)
(define RC2 (RC 15 10 0.2))
(displayln "******")
(display-stream-n (RC2 fibs 2) 10)
(displayln "******")
(display-stream-n (RC2 integers 2) 10)