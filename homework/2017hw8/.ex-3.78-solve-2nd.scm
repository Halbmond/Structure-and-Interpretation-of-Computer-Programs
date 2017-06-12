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
	(if (= n 1)
			(display (stream-car s))
			(begin (display (stream-car s)) (display " ") (display-stream-n (stream-cdr s) (- n 1)))))
;begin
(define (stream-add s1 s2) (cons-stream (+ (stream-car s1) (stream-car s2)) (stream-add (stream-cdr s1) (stream-cdr s2))))
(define (stream-scale s m) (stream-map (lambda (x) (* x m)) s))
(define-syntax stream-int-def (syntax-rules ()
	[(stream-int-def int s v0 d) (define int (cons-stream v0 (stream-add int (stream-scale s d))))])
)
(define (solve-2nd a b dt y0 dy0)
	(stream-int-def dy ddy dy0 dt)
	(stream-int-def y dy y0 dt)
	(define ddy (stream-add (stream-scale dy a) (stream-scale y b)))
	y
)
;end
(define (myloop)
	(let ((a (read)))
		(if (eq? a eof)
				(void)
				(let ((b (read))
							(dt (read))
							(y0 (read))
							(dy0 (read))
							(n (read)))
					(begin (display-stream-n (solve-2nd a b dt y0 dy0) n) (newline) (myloop))))))

(myloop)