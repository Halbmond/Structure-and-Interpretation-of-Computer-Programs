#lang racket
(define (square x) (* x x))
(define (divisible? x y ) (= (remainder x y ) 0))

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

;=================
;MY CODE BEGIN
(define (interleave s1 s2 proc)
	(let*([f1 (stream-car s1)] [w1 (proc f1)]
		[f2 (stream-car s2)] [w2 (proc f2)])
		(if (< w1 w2)
			(cons-stream f1 (interleave s2 (stream-cdr s1) proc))
			(cons-stream f2 (interleave s1 (stream-cdr s2) proc))))
)

(define (weighted-pairs s t proc)
	(cons-stream (list (stream-car s) (stream-car t))
		(interleave
			(stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
			(weighted-pairs (stream-cdr s) (stream-cdr t) proc)
			proc))
)

(define (Ramanujan s)
	(let*(	[x (weight3 (stream-car s))]
			[ns (stream-cdr s)][nx (weight3 (stream-car ns))]
			[nns (stream-cdr ns)])
		(if (= x nx) (cons-stream x (Ramanujan nns)) (Ramanujan ns)))
)
;MY CODE END
(define (integers-from n)
	(cons-stream n (integers-from (+ n 1))))
(define integers (integers-from 1))
(define (cube x)  (* x x x))
(define weight3 (lambda (x) (+ (cube (car x)) (cube (cadr x)))))
(define lst (weighted-pairs integers integers weight3)) 
(define result-stream  (Ramanujan lst))



(define (myloop)
	(let ((n (read)))
		(if (eq? n eof)
				(void)
				(begin (displayln (stream-ref result-stream n)) (myloop)))))

(myloop)
